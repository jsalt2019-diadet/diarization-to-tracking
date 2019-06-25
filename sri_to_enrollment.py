#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""Usage: diarization_to_enrollment.py <path>

Arguments:
    path                folder path containing dev and test folders,
                        themselves containing a gold folder containing .rttm files

Use case:
    python sri_to_enrollment.py sri_diarization
"""

import os
from docopt import docopt
from utils import utils


def generate_enrollment(folder_path, fold, min_duration="filedur"):
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

    full_text = "speaker\tmodel_number\tfilename\tonset\toffset\n"
    rttm_files = utils.get_rttm(folder_path, fold)
    rttm_files = [rttm for rttm in rttm_files if len(rttm.split('-')) == 2]

    model_number = {}
    for rttm in rttm_files:
        with open(rttm) as rttm_data:
            # Should contain only one line
            for line in rttm_data:
                splitted = line.split(' ')
                onset, duration, speaker = float(splitted[3]), float(splitted[4]), splitted[7]

                if speaker not in model_number.keys():
                    model_number[speaker] = 0
                else:
                    model_number[speaker] += 1

                full_text += '\t'.join([speaker, str(model_number[speaker]),
                              os.path.basename(rttm).replace(".rttm", ""),
                              str(onset), str(onset+duration)])
                full_text += "\n"

    with open(os.path.join(folder_path, fold, "%s_enrollments_%s.txt" % (fold, min_duration)), "w") as f:
        f.write(full_text[:-1])


def main(args):
    folder = args["<path>"]
    min_dur = "filedur"

    for fold in ["dev", "test"]:
        generate_enrollment(folder, fold, min_dur)
        print("%s_enrollments_%s.txt generated in %s" % (fold, min_dur, folder))


if __name__ == '__main__':
    args = docopt(__doc__)
    main(args)
