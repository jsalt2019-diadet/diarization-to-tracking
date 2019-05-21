import pandas as pd
import numpy as np
import argparse
import os

"""
Use case :
python pairing_enrollment_test.py --enrollments ami_diarization/dev_enrollments_30.txt --test_segments ami_diarization/dev_test_segments_60.txt -s 0 -m 1 --sep .
python pairing_enrollment_test.py --enrollments chime5_diarization/dev_enrollments_30.txt --test_segments chime5_diarization/dev_test_segments_60.txt -s 0 -m 1 --sep _
python pairing_enrollment_test.py --enrollments babytrain_diarization/dev_enrollments_30.txt --test_segments babytrain_diarization/dev_test_segments_120.txt
"""

### Global variables that won't be set up by the user
NB_MIN = 10                     # number of minimal trials for the natural subsampling
SEED = np.random.seed(seed=42)  # random seed (for reproducibility)


def remove_silence_test(test_segments):
    # Compute chunks composed of silence
    silences = test_segments.groupby(['filename', 'beginning_time', 'end_time'])['duration_total_speech'].sum().reset_index()
    silences = silences.loc[silences["duration_total_speech"] == 0.0]

    # Merge
    merged = silences.merge(test_segments, on=['filename', 'beginning_time', 'end_time'], how='right',
                            right_index=True, indicator=True, suffixes=('_x', ''))

    # Keep columns that did not belong to silences dataframe
    test_segments = merged[merged["_merge"] == "right_only"].drop(columns=["_merge", "duration_total_speech_x"])
    return test_segments


def remove_100_overlapping(test_segments):
    # Remove segments whose percentage of overlapping speech is equal to 100%
    test_segments["percent_overlapping"] = test_segments["duration_overlapping_speech"] / test_segments["duration_total_speech"]
    test_segments = test_segments[test_segments["percent_overlapping"] != 1.0]
    test_segments = test_segments.drop(columns=["percent_overlapping"])
    return test_segments


def maximal_pairing(enrollments, test_segments, babytrain=False):
    def agg_onset_offset(x):
        d = {}
        d['on_off'] = list(zip(x['onset'].values, x['offset'].values))
        d['filename'] = np.unique(x['filename'].values)
        d['session'] = '&&'.join(np.unique(x['session'].values))
        d['microphone'] = '&&'.join(np.unique(x['microphone'].values))
        return pd.Series(d, index=['on_off', 'filename', 'session', 'microphone'])

    # Aggregate enrollments so that each line is an enrollment (instead of each line being a speech utterance)
    enrollment_agg = enrollments.groupby(["speaker", "model_number"]).apply(
             agg_onset_offset).reset_index()

    # Cartesian product between test_segments and enrollments_agg
    trials = test_segments.merge(enrollment_agg, how="left", left_on="target_speaker", right_on="speaker",
                                 suffixes=("_test_seg", "_enrollment"))

    trials["beginning_time"] = trials["beginning_time"].astype(np.int32)
    trials["end_time"] = trials["end_time"].astype(np.int32)
    trials["model_number"] = trials["model_number"].astype(np.int64)

    # Keeping only (same sessions but different mic) or (different sessions)
    trials = trials[((trials["session_test_seg"] == trials["session_enrollment"]) & (
                trials["microphone_test_seg"] != trials["microphone_enrollment"])) | (
                                trials["session_test_seg"] != trials["session_enrollment"])]
    # For babytrain, enrollment can come from multiple files
    # We need to ensure that the target test doesn't come from the same files.
    if babytrain:
        trials["isContain"] = trials.apply(lambda x: x["session_test_seg"] in str(x["session_enrollment"]).split("&&"), axis=1)
        trials = trials[~trials["isContain"]]
        trials.drop(["isContain"], axis=1, inplace=True)
    return trials


def natural_subsampling(trials, nb_trials_end, nb_min):
    """
    Here's the idea is to keep the proportion of target trials, and the proportion of non target trials in the subsampled dataset.

    :param trials:          The dataset that needs to be subsampled
    :param nb_trials_end:   The wished number of trials
    :param nb_min:          A minimal number of trials (all the speakers below this number will be removed).
                            Removed after having subsampled !
    :return:                the subsampled trials
    """
    # When groupby and counting, speakers with a count equal to 0 are removed.
    # We don't want this behaviour, so we'll add this serie to ensure all the speakers are present.
    neutral_el = pd.Series(0, index=np.unique(trials["target_speaker"])).astype('int')
    nb_trials_beg = len(trials.index)
    if nb_trials_beg < nb_trials_end:
        raise ValueError("You asked for %d trials. But the unsampled dataset contains only %d trials." % (nb_trials_end, nb_trials_beg))

    # Get number of target trials and number of non target trials
    t_trials_beg = len(trials[trials["duration_total_speech"] != 0].index)
    nt_trials_beg = len(trials.index) - t_trials_beg

    nb_speakers_beg = len(np.unique(trials["target_speaker"]))

    # Get number of target trials and number of non target trials for each speaker
    t_trials_per_speaker_beg = trials[trials["duration_total_speech"] != 0].groupby(["target_speaker"])[
        "model_number"].agg('count')
    nt_trials_per_speaker_beg = trials[trials["duration_total_speech"] == 0].groupby(["target_speaker"])[
        "model_number"].agg('count')

    # Get target proportion  and non target proportion of each speaker : that is what we want to keep after the subsampling
    t_prop_per_speaker_beg = t_trials_per_speaker_beg / t_trials_beg
    nt_prop_per_speaker_beg = nt_trials_per_speaker_beg / nt_trials_beg

    # Compute the number of target trials and the number of non target trials that needs to be removed for each speaker
    # number of target trials that we want : proportion of target trials * nb_trials_end
    t_trials_end = nb_trials_end * t_trials_beg / (t_trials_beg + nt_trials_beg)
    t_trials_per_speaker_end = np.round(t_prop_per_speaker_beg * t_trials_end).astype(int)
    t_trials_remove = t_trials_per_speaker_beg - t_trials_per_speaker_end

    # number of non target trials that we want
    nt_trials_end = nb_trials_end * nt_trials_beg / (t_trials_beg + nt_trials_beg)
    nt_trials_per_speaker_end = np.round(nt_prop_per_speaker_beg * nt_trials_end).astype(int)
    nt_trials_remove = nt_trials_per_speaker_beg - nt_trials_per_speaker_end

    for speaker in neutral_el.index:
        if speaker in t_trials_remove:
            trials = trials.drop(trials.query('target_speaker == @speaker & duration_total_speech != 0')
                                 .sample(n=t_trials_remove[speaker]).index)
        if speaker in nt_trials_remove:
            trials = trials.drop(trials.query('target_speaker == @speaker & duration_total_speech == 0')
                                 .sample(n=nt_trials_remove[speaker]).index)

    # We need to perform these operations to avoid getting NaN
    # for speakers who have only target trials or only non target trials
    t_trials_per_speaker_end = t_trials_per_speaker_end.add(neutral_el, fill_value=0)
    nt_trials_per_speaker_end = nt_trials_per_speaker_end.add(neutral_el, fill_value=0)
    enough_trials = t_trials_per_speaker_end + nt_trials_per_speaker_end > nb_min
    enough_trials = enough_trials[enough_trials].index
    trials = trials[trials["target_speaker"].isin(enough_trials)]

    nb_speakers_end = len(np.unique(trials["target_speaker"]))
    print("Natural subsampling : %d speakers to %d speakers. %d speakers lost !" % (nb_speakers_beg, nb_speakers_end,
                                                                                   nb_speakers_beg - nb_speakers_end))
    print("The goal was to get %d trials. We got %d." % (nb_trials_end, len(trials.index)))
    return trials

def square_subsampling(trials, nb_trials):
    """
    Here's the idea is, given a number N :
        1) Remove all the speakers whose number of trials is lower than N
        2) Randomly remove trials of other speakers, so that their (invidivual) number of trials is equal to N.

    :param trials:          The dataset that needs to be subsampled
    :param nb_trials_end:   The wished number of trials per speaker
    :return:
    """
    # Get number of trials per speaker
    trials_per_speaker = trials.groupby(["target_speaker"])[
        "model_number"].agg('count')
    nb_speakers_beg = len(trials_per_speaker.index)

    # Compute number of trials that needs to be remove for each speaker
    nb_to_remove = trials_per_speaker - nb_trials
    for speaker in trials_per_speaker.index:
        if nb_to_remove[speaker] >= 0:
            trials = trials.drop(trials.query('target_speaker == @speaker')
                                 .sample(n=nb_to_remove[speaker]).index)
        else:
            trials = trials[trials["target_speaker"] != speaker]
    nb_speakers_end = len(np.unique(trials["target_speaker"]))
    print("Square subsampling : %d speakers to %d speakers. %d speakers lost !" % (nb_speakers_beg, nb_speakers_end,
                                                                                   nb_speakers_beg-nb_speakers_end))
    return trials

def write_trials(trials, output_path):
    # # Choose more explicit column name
    df = trials.drop(["speaker", "filename_enrollment", "session_enrollment", "microphone_enrollment", "on_off"], axis=1)
    df.columns = ['filename', 'beginning_time', 'end_time', 'target_speaker',
                       'duration_total_speech', 'duration_overlapping_speech',
                       'session', 'microphone', 'enrollment_number']
    df_path = os.path.join(output_path)
    df.to_csv(df_path, header=True, sep="\t", index=False, float_format="%.4f",
                  columns=['target_speaker', 'enrollment_number',
                           'filename', 'beginning_time', 'end_time',
                           'duration_total_speech',
                           'duration_overlapping_speech',
                           'session', 'microphone'])


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('-e', '--enrollments', type=str, required=True,
                        help='''Path to the enrollment .txt files''')
    parser.add_argument('-t', '--test_segments', type=str, required=True,
                        help='''Path to the test_segments .txt files''')
    parser.add_argument('-m', '--mic_idx', type=int, default=None,
                        help='''Index of the microphone when the filename is separated by --sep''')
    parser.add_argument('-s', '--session_idx', type=int, default=None,
                        help='''Index of the session when the filename is separated by --sep''')
    parser.add_argument('--sep', type=str, default=".",
                        help='''Separating character to retrieve session and microphone information''')
    parser.add_argument('--nb_trials', type=int, default=1000,
                        help='''Number of trials that needs to be kept (for the subsampling)''')
    parser.add_argument('--square', action='store_true',
                        help='''If True, performs a square subsampling. 
                        Otherwise, a natural subsampling will be executed.''')
    args = parser.parse_args()

    # Open files and store data to pandas dataframe
    enrollments = pd.read_csv(args.enrollments, sep="\t",
                              dtype={'speaker': 'category',
                                     'model_number': np.int16,
                                     'filename': 'category',
                                     'onset': np.float16,
                                     'offset': np.float16})
    test_segments = pd.read_csv(args.test_segments, sep="\t",
                                dtype={'target_speaker': 'category',
                                       'filename': 'category',
                                       'beginning_time': np.int16,
                                       'end_time': np.int16,
                                       'duration_total_speech': np.float16,
                                       'duration_overlapping_speech': np.float16})

    # Some speakers might have only enrollment because the duration of a test segment is too long (ie. all the
    # files in which the speaker appear are less than the test_segment duration)
    # Some speakers might have only test segments, because they don't talk enough to allow us to build an enrollment
    common_speakers = set(np.unique(enrollments["speaker"])) & set(np.unique(test_segments["target_speaker"]))
    enrollments = enrollments[enrollments["speaker"].isin(common_speakers)]
    test_segments = test_segments[test_segments["target_speaker"].isin(common_speakers)]

    # Remove silences and 100% overlapping test segments
    test_segments = remove_silence_test(test_segments)
    test_segments = remove_100_overlapping(test_segments)

    # Add session and microphone info
    session_idx = args.session_idx
    mic_idx = args.mic_idx
    sep = args.sep

    test_segments["session"] = test_segments["filename"]
    enrollments["session"] = enrollments["filename"]
    test_segments["microphone"] = test_segments["filename"]
    enrollments["microphone"] = enrollments["filename"]

    # Convert types for optimizing memory usage
    test_segments["session"] = test_segments["session"].astype('category')
    enrollments["session"] = enrollments["session"].astype('category')
    test_segments["microphone"] = test_segments["microphone"].astype('category')
    enrollments["microphone"] = enrollments["microphone"].astype('category')

    if session_idx is not None:
        test_segments["session"] = test_segments["filename"].str.split(sep, n=1).map(lambda x: x[session_idx])
        enrollments["session"] = enrollments["filename"].str.split(sep, n=1).map(lambda x: x[session_idx])

    if mic_idx is not None:
        test_segments["microphone"] = test_segments["filename"].str.split(sep, n=1).map(lambda x: x[mic_idx])
        enrollments["microphone"] = enrollments["filename"].str.split(sep, n=1).map(lambda x: x[mic_idx])

    # Compute maximal pairing
    trials = maximal_pairing(enrollments, test_segments, babytrain=(mic_idx is None)) # if mic_idx is not provided, consider that we're treating babytrain

    print(trials.memory_usage())
    # Write the file containing the maximum number of trials
    folder_output = os.path.dirname(args.enrollments)
    dur_enr = os.path.basename(args.enrollments).split('_')[1].replace('.txt', '')
    dur_test = os.path.basename(args.test_segments).split('_')[2].replace('.txt', '')
    output_path = os.path.join(folder_output, "trials_%s_enr_%s_test_%s.txt" % ('unsampled', dur_enr, dur_test))
    write_trials(trials, output_path)

    # Subsample
    # if args.square:
    #     subsamp = "square"
    #     trials = square_subsampling(trials, args.nb_trials)
    # else:
    #     subsamp = "natural"
    #     trials = natural_subsampling(trials, args.nb_trials, NB_MIN)
    #
    # # Update enrollments.txt with mic and session information + filtered enrollments that we've lost because of the subsampling
    # enrollments = enrollments.merge(trials, how='inner',
    #                                 left_on=["speaker", "model_number"],
    #                                 right_on=["target_speaker", "model_number"], suffixes=('','_y'))
    # enrollments = enrollments[['speaker', 'model_number', 'filename','onset', 'offset', 'session', 'microphone']]
    # enrollments.to_csv(args.enrollments.replace("enrollments", "enrollments_%s" % subsamp), header=True,
    #                    sep="\t", index=False, float_format="%.4f")
    #
    # # Write paired test_segments.txt
    # output_path = os.path.join(folder_output, "trials_%s_enr_%s_test_%s_N_%s.txt" % (subsamp, dur_enr, dur_test, args.nb_trials))
    # write_trials(trials, output_path)

    # for i in range(100,1100,100):
    #     NB_TRIALS = i
    #     subsamp = "square"
    #     trials_subsampled = square_subsampling(trials, NB_TRIALS)
    #     # Update enrollments.txt with mic and session information + filtered enrollments that we lost because of the subsampling
    #     enrollments_ss = enrollments.merge(trials_subsampled, how='inner',
    #                                     left_on=["speaker", "model_number"],
    #                                     right_on=["target_speaker", "model_number"], suffixes=('','_y'))
    #     enrollments_ss = enrollments_ss[['speaker', 'model_number', 'filename','onset', 'offset', 'session', 'microphone']]
    #     enrollments_ss.to_csv(args.enrollments.replace("enrollments", "enrollments_%s" % subsamp), header=True,
    #                        sep="\t", index=False, float_format="%.4f")
    #
    #     # Write paired test_segments.txt
    #     output_path = os.path.join(folder_output, "trials_%s_enr_%s_test_%s_N_%s.txt" % (subsamp, dur_enr, dur_test, NB_TRIALS))
    #     write_trials(trials_subsampled, output_path)
    #
    #     NB_TRIALS = i*10
    #     subsamp = "natural"
    #     trials_subsampled = natural_subsampling(trials, NB_TRIALS, NB_MIN)
    #     # Update enrollments.txt with mic and session information + filtered enrollments that we lost because of the subsampling
    #     enrollments_ss = enrollments.merge(trials_subsampled, how='inner',
    #                                     left_on=["speaker", "model_number"],
    #                                     right_on=["target_speaker", "model_number"], suffixes=('','_y'))
    #     enrollments_ss = enrollments_ss[['speaker', 'model_number', 'filename','onset', 'offset', 'session', 'microphone']]
    #     enrollments_ss.to_csv(args.enrollments.replace("enrollments", "enrollments_%s" % subsamp), header=True,
    #                        sep="\t", index=False, float_format="%.4f")
    #
    #     # Write paired test_segments.txt
    #     output_path = os.path.join(folder_output, "trials_%s_enr_%s_test_%s_N_%s.txt" % (subsamp, dur_enr, dur_test, NB_TRIALS))
    #     write_trials(trials_subsampled, output_path)

if __name__ == '__main__':
    main()
