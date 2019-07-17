#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Generates test segments of the SRI dataset.
The target test segments for a given speaker S are chunks where S is speaking
The non-target test segments for a given speaker S are chunks coming from a file F where S does NOT participate.

Usage: python sri_to_test_segments.py <path> [-d DURATION]

Arguments:
                        folder path containing train, dev and test folders,
                        themselves containing a gold folder containing .rttm files
                        These folders must contain a file called all_{fold}.uem (so that
                        we can know the duration of the wav)

Options:
    -d DURATION         duration of a test segment (in seconds) [default: 60]

Use case:
    python diarization_to_test_segments.py sri_diarization

Requirements:
    pyannote.database
    pyannote.core
"""

import pyannote.database.util as pyda
from pyannote.core import Segment, Annotation
import os, glob
import argparse
import pandas
from utils import utils


def get_sri_enrolled_speakers(sri_path):
    folds = ['dev', 'test']
    speakers = []
    for fold in folds:
        fold_speakers = glob.glob(os.path.join(sri_path,fold,'gold/*.rttm'))
        fold_speakers = [os.path.basename(rttm).split('-')[0] for rttm in fold_speakers if len(os.path.basename(rttm).split('-')) == 2]
        speakers += fold_speakers
    return speakers


def get_speakers_file(rttm):
    speakers = set()
    with open(rttm) as data:
        for line in data:
            splitted = line.split(' ')
            speakers |= set([splitted[7]])
    return speakers

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("path", type=str, help="Relative path to the database folder, containing"# Positional Argument
                                              "train, dev, and test sub-folders.")
    parser.add_argument("-d", "--duration", type=int, default=60,
                        help="Duration of a test segment. (Default to 60 seconds)")
    args = parser.parse_args()

    # Parameters
    DURATION_TEST = args.duration
    DATABASE_PATH = os.path.join(os.getcwd(), args.path)  # needs to loop through dev and test

    folds = ["dev", "test"]
    speakers = get_sri_enrolled_speakers(DATABASE_PATH)

    for fold in folds:
        # Load uem
        uem_path = os.path.join(DATABASE_PATH, fold, "diego_cond_%s.uem" % fold)
        uem = pandas.read_csv(uem_path, sep=" ", names=["filename", "channel", "beg", "end"])
        valid_path = os.path.join(DATABASE_PATH, "valid.lst")
        valid_lst = pandas.read_csv(valid_path, sep="\t", names=["rm_session_mic"])

        # Header
        test_segments_txt = "target_speaker\tfilename\tbeginning_time\tend_time\tduration_total_speech\tduration_overlapping_speech\n"

        # Extract target and non-target test_segments
        rttm_files = utils.get_rttm(DATABASE_PATH, fold)

        # Discarding enrollment files
        rttm_files = [rttm for rttm in rttm_files if len(rttm.split('-')) != 2]
        rttm_files = [rttm for rttm in rttm_files if
                      '-'.join(os.path.basename(rttm).split('-')[1:3]) + " " +
                      os.path.basename(rttm).split('-')[3] in valid_lst["rm_session_mic"].values]

        for rttm in rttm_files:
            speakers_file = get_speakers_file(rttm)
            basename = os.path.splitext(os.path.basename(rttm))[0]
            wav_duration = float(uem.loc[uem["filename"] == basename.replace(".rttm", ""), "end"])
            data = pyda.load_rttm(rttm)

            if data != {}:
                annotation = data[basename]
                for end in range(DURATION_TEST, int(wav_duration)+1, DURATION_TEST):
                    beg = end - DURATION_TEST
                    chunk = annotation.crop(Segment(beg, end))
                    targets = chunk.labels()

                    overlapping_chunk = utils.overlapping_annotation(chunk)
                    # A speaker is defined as a target speaker for a chunk c,
                    # when he/she is speaking in c.
                    for target in targets:
                        tot_speech = chunk.label_duration(target)
                        overlapping_speech = overlapping_chunk.label_duration(target)
                        test_segments_txt += "%s\t%s\t%d\t%d\t%.3f\t%.3f\n" % (target, basename, beg, end, tot_speech, overlapping_speech)

                    non_targets = list(set(speakers) - set(speakers_file))

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