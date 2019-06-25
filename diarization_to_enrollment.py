#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""Usage: diarization_to_enrollment.py <path> [--cross-file] [--bbt] [-d MIN_DURATION]

Arguments:
    path                folder path containing train, dev and test folders,
                        themselves containing a gold folder containing .rttm files

Options:
    --cross-file        indicates whether an enrollment can be extracted from several files or not.
    -d MIN_DURATION     minimum duration for a segment [default: 30]
    --bbt               indicates if the dataset is BabyTrain [default: False]
                        If True, skip the speakers whose name does NOT start by !

Use case:
    python diarization_to_enrollment.py babytrain_diarization --bbt
    python diarization_to_enrollment.py ami_diarization
    python diarization_to_enrollment.py chime5_diarization
"""

import os
from docopt import docopt
from utils import utils


def generate_enrollment(folder_path, fold, min_duration=30, cross_file=False, babytrain=False):
    """Generates enrolment file from rttm files folder

    Parameters
    ----------
    folder_path : `str`
        Folder path of rttm files
    min_duration : `int`
        Minimum duration for a segment
    cross_file : `boolean`
        Using cross-file enrollment
    """
    def reset_cum_per_speaker(cum_per_speaker):
        for k, v in cum_per_speaker.items():
            model_number = cum_per_speaker[k][2]
            cum_per_speaker[k] = [0.0, "", model_number]
        return cum_per_speaker

    rttm_files = utils.get_rttm(folder_path, fold)

    full_text = ""
    cum_per_speaker = {}

    for rttm in rttm_files:
        if not cross_file:
            cum_per_speaker = reset_cum_per_speaker(cum_per_speaker)

        with open(rttm, mode='r') as f:
            basename = os.path.basename(rttm).replace(".rttm", "")

            prev_start, prev_end, prev_speaker = 0.0, 0.0, "fake_speaker_whose_name_must_not_appear"
            # Dictionnary whose keys are the name of the speakers
            # And values are cumulated speech, string_buffer, model_number associated to this speaker
            cum_per_speaker[prev_speaker] = [0.0, "", 0]

            for line in f:
                splitted = line.split(' ')
                start, duration, speaker = round(float(splitted[3]), 3), round(float(splitted[4]),3), splitted[7]
                end = round(start + duration, 3)

                if speaker not in cum_per_speaker.keys():
                    cum_per_speaker[speaker] = [0.0, "", 0]

                model_number = cum_per_speaker[prev_speaker][2]
                if start >= prev_end:
                    # Non overlapping
                    # |----|                prev_utterrance
                    #           |-----|     utterrance
                    if prev_end > prev_start:
                        cum_per_speaker[prev_speaker][0] += prev_end - prev_start
                        cum_per_speaker[prev_speaker][1] += f"{prev_speaker}\t{model_number}\t{basename}\t{prev_start}\t{prev_end}\n"
                else:   # start < prev_end
                    # Overlapping
                    # |--------|
                    #    |--------|
                    if start-prev_start > 0:
                        # Uncomment these 2 lines for taking into account clean part of overlapping speech

                        # cum_per_speaker[prev_speaker][0] += start-prev_start
                        # cum_per_speaker[prev_speaker][1] += f"{prev_speaker}\t{model_number}\t{basename}\t{prev_start}\t{start}\n"

                        # In that case, we need to update the current speech utterrance
                        start = prev_end
                        if start > end:
                            end = start

                # If we have enough speech, we got an enrollment for prev_speaker
                if cum_per_speaker[prev_speaker][0] >= min_duration:
                    full_text += cum_per_speaker[prev_speaker][1]
                    cum_per_speaker[prev_speaker][0] = 0.0
                    cum_per_speaker[prev_speaker][1] = ""
                    cum_per_speaker[prev_speaker][2] += 1

                # Update previous fields to current fields
                prev_start, prev_end, prev_speaker = start, end, speaker

    if babytrain:
        full_text = full_text.split('\n')
        full_text = [line for line in full_text if line.split('\t')[0].startswith("!")]
        full_text = '\n'.join(full_text)

    full_text = "speaker\tmodel_number\tfilename\tonset\toffset\n" + full_text
    with open(os.path.join(folder_path, fold, "%s_enrollments_%d.txt" % (fold, min_duration)), "w") as f:
        f.write(full_text[:-1])


def main(args):
    folder = args["<path>"]
    cross_file = args["--cross-file"]
    babytrain = args["--bbt"]
    min_dur = int(args['-d'])

    for fold in ["train", "dev", "test"]:
        generate_enrollment(folder, fold, min_dur, cross_file, babytrain)
        print("%s_enrollments_%d.txt generated in %s" % (fold, min_dur, folder))


if __name__ == '__main__':
    args = docopt(__doc__)
    main(args)
