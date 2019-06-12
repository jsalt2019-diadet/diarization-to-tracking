#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""Usage: python diarization_to_test_segments.py <path> [-d DURATION] [--bbt BABYTRAIN]

Arguments:
                        folder path containing train, dev and test folders,
                        themselves containing a gold folder containing .rttm files

Options:
    -d DURATION         duration of a test segment (in seconds) [default: 60]
    --bbt BABYTRAIN     indicates if the folder that needs to be treated is BabyTrain.
                        if True, skip all the speakers whose name does NOT start by !

Use case:
    python diarization_to_test_segments.py sri_diarization --sri
    python diarization_to_test_segments.py ami_diarization
    python diarization_to_test_segments.py chime5_diarization
    python diarization_to_test_segments.py babytrain_diarization --bbt -d 120

Requirements:
    pyannote.database
    pyannote.core
"""

import pyannote.database.util as pyda
from pyannote.core import Segment, Annotation
import os
import argparse
from utils import utils


def get_friends_per_speaker(rttm_files, BABYTRAIN):
    """
    Given a list of .rttm files, return the dictionnary
    whose :
        - key is a speaker.
        - value is a list of speakers that appear in the same file than the key.

    If BABYTRAIN = True, skip the speakers whose name does not start by !
    """
    friends_per_speaker = {}
    for rttm in rttm_files:
        basename = os.path.splitext(os.path.basename(rttm))[0]
        data = pyda.load_rttm(rttm)

        if data != {}:
            annotation = data[basename]

            # Get the list of speakers participating to this file
            speakers = annotation.labels()

            if BABYTRAIN:
                speakers = [s for s in speakers if s.startswith("!")]

            # For each speaker, add his/her friends.
            for speaker in speakers:
                if speaker not in friends_per_speaker.keys():
                    friends_per_speaker[speaker] = set(speakers)
                else:
                    friends_per_speaker[speaker] |= set(speakers)

    # Replace friends of investigators by empty set
    if BABYTRAIN:
        for k, v in friends_per_speaker.items():
            if k.startswith("!INV"):
                friends_per_speaker[k] = set()

    return friends_per_speaker


def get_friends_of_participants(friends_per_speaker, list_of_speakers):
    """
    Given a list of speakers, and a dictionnary containing all the friends (values) of each speaker (key),
    returns the list of all friends of every speaker belonging to the list_of_speakers.
    """
    all_friends = set()
    for speaker in list_of_speakers:
        all_friends |= friends_per_speaker[speaker]
    return all_friends

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("path", type=str, help="Relative path to the database folder, containing"# Positional Argument
                                              "train, dev, and test sub-folders.")
    parser.add_argument("-d", "--duration", type=int, default=60, help="Duration of a test segment. (Default to 60 seconds)")

    parser.add_argument('--bbt', action='store_true', help="Indicates whether the corpora is BabyTrain or not. If true,"
                                                           "skip all the speakers whose name doesn't start by '!'.")
    parser.add_argument('--sri', action='store_true', help="Indicates whether the corpora is SRI or not.")
    args = parser.parse_args()

    # Parameters
    DURATION_TEST = args.duration
    DATABASE_PATH = os.path.join(os.getcwd(), args.path)  # needs to loop through dev and test
    BABYTRAIN = args.bbt
    SRI = args.sri

    folds = ["train", "dev", "test"]
    if SRI:
        folds = ["dev", "test"]

    for fold in folds:
        # Header
        test_segments_txt = "target_speaker\tfilename\tbeginning_time\tend_time\tduration_total_speech\tduration_overlapping_speech\n"

        # Extract target and non-target test_segments
        rttm_files = utils.get_rttm(DATABASE_PATH, fold)
        if SRI:
            # Discarding enrollment files
            rttm_files = [rttm for rttm in rttm_files if len(rttm.split('-')) != 2]

        ## First, get the dictionnary of every friends of every speakers.
        friends_per_speaker = get_friends_per_speaker(rttm_files, BABYTRAIN)

        for rttm in rttm_files:
            basename = os.path.splitext(os.path.basename(rttm))[0]
            data = pyda.load_rttm(rttm)

            if data != {}:
                annotation = data[basename]

                participants = annotation.labels()
                if BABYTRAIN:
                    participants = [p for p in participants if p.startswith("!")]

                all_friends = get_friends_of_participants(friends_per_speaker, participants)
                last_offset = annotation.get_timeline()[-1][1]

                for end in range(DURATION_TEST, int(last_offset), DURATION_TEST):
                    beg = end - DURATION_TEST
                    chunk = annotation.crop(Segment(beg, end))
                    targets = chunk.labels()

                    if BABYTRAIN:
                        targets = [t for t in targets if t.startswith("!")]

                    overlapping_chunk = utils.overlapping_annotation(chunk)
                    # A speaker is defined as a target speaker for a chunk c,
                    # when he/she is speaking in c.
                    for target in targets:
                        tot_speech = chunk.label_duration(target)
                        overlapping_speech = overlapping_chunk.label_duration(target)
                        test_segments_txt += "%s\t%s\t%d\t%d\t%.3f\t%.3f\n" % (target, basename, beg, end, tot_speech, overlapping_speech)

                    non_targets = list(set(all_friends) - set(targets))

                    # A speaker is defined as a non-target speaker for a chunk c,
                    # when he/she is not speaking in c, but speaks somewhere in whatever file
                    # where one of the target speaker is also participating
                    for non_target in non_targets:
                        test_segments_txt += "%s\t%s\t%d\t%d\t%.1f\t%.1f\n" % (non_target, basename, beg, end, 0.0, 0.0)

        with open(os.path.join(DATABASE_PATH, fold, "%s_test_segments_%d.txt" % (fold, DURATION_TEST)), "w") as f:
            f.write(test_segments_txt[:-1])

        print("%s_test_segments_%d.txt generated in %s" % (fold, DURATION_TEST, os.path.join(DATABASE_PATH, fold)))


if __name__ == '__main__':
    main()
