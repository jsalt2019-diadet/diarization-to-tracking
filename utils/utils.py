import glob, os
from pyannote.core import Segment, Annotation


def get_dev_test_rttm(db_path):
    """
    Return all the .rttm files belonging to the db_path/dev/gold and db_path/test/gold directories.
    """
    folds = ["dev", "test"]
    rttm_files = []
    for fold in folds:
        rttm_files.extend(glob.glob(os.path.join(db_path, fold, "gold/*.rttm")))
    return rttm_files

def overlapping_annotation(annotation):
    """
    Given an annotation, returns the same annotation by keeping only the overlapping segments
    """
    overlap = Annotation()

    for track1, track2 in annotation.co_iter(annotation):

        if track1 == track2:
            continue
        # Get speakers of track1 and track2
        speakers = annotation.get_labels(track1[0]) | annotation.get_labels(track2[0])

        # Get intersection of track1 and track2
        intersection = track1[0] & track2[0]

        # For each speaker, add its overlapping segment
        for speaker in speakers:
            # a track is uniquely identified by the couple
            # (Segment, Identifier), here we need to use the speaker as a
            # new identifier
            overlap[intersection, speaker] = speaker
    return overlap
