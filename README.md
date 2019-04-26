If the application is running, you can connect to it on : 

https://marvinlvn.shinyapps.io/tracking/


# Regenerating the speaker tracking task

## Requirements

Clone the repo :

```bash
git clone https://github.com/jsalt2019-diadet/diarization-to-tracking.git
cd diarization-to-tracking

```

Install dependencies (**pyannote.core** and **pyannote.database**) :

```bash
conda create --name pyannote python=3.6
pip install pyannote.core pyannote.database
```

Download the necessary .rttm files :

```bash
scp -r <username>@login.clsp.jhu.edu:/export/fs01/jsalt19/databases/rttm/* .
```

where \<username\> needs to be replaced by your username on the JHU cluster.

## Commands

```bash
# Generate test
python diarization_to_test_segments.py babytrain_diarization/ --bbt --cross-file
python diarization_to_test_segments.py chime5_diarization/
python diarization_to_test_segments.py ami_diarization/

# Generate enrollment
python diarization_to_enrollment.py babytrain_diarization/ --bbt --cross-file
python diarization_to_enrollment.py chime5_diarization/
python diarization_to_enrollment.py ami_diarization/
```

The **--bbt** flag indicates if it is BabyTrain that needs to be processed. 
In which case, the scripts will treat only the uniquely identified speakers (i.e. the speakers whose name starts by **!**).

If you want to change the duration, you can add **-d** followed by the duration you want to use :

```bash
# To generate 15 seconds long enrollment (default to 30 seconds)
python diarization_to_enrollment.py chime5_diarization/ -d 15

# To generate 2 mn long test_segments (default to 60 seconds)
python diarization_to_enrollment.py chime5_diarization/ -d 120
```
