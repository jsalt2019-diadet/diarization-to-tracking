
# Running the shiny application

After having asked for a R session :

Firstly, install the possibly missing dependencies:

```R
install.packages('shinydashboard')
install.packages('shiny')
install.packages('shinyWidgets')
install.packages('ggplot2')
```

Secondly, run the app :

```R
library('shiny')
runGitHub("jsalt2019-diadet/diarization-to-tracking","<My git username>", subdir="sunshine/tracking")
```

The needed enrollment.txt and trials.txt are stored on GitHub, so that you don't need them to run the application. 
Otherwise, if you want so, you can follow the instructions in the next part.

# Regenerating the speaker tracking task

## Requirements

**pyannote.core** and **pyannote.database**

```bash
conda create --name pyannote python=3.6
pip install pyannote.core pyannote.database
```

To get the necessary .rttm files :

```bash
scp -r <username>@login.clsp.jhu.edu:/export/fs01/jsalt19/databases/rttm/* .
```

where \<username\> needs to be replaced by your username on the JHU cluster.

## Commands

```bash
# Generate trials
python diarization_to_trials.py babytrain_diarization/ --bbt
python diarization_to_trials.py chime5_diarization/
python diarization_to_trials.py ami_diarization/

# Generate enrollment
python diarization_to_enrollment.py babytrain_diarization/ --bbt
python diarization_to_enrollment.py chime5_diarization/
python diarization_to_enrollment.py ami_diarization/
```

The **--bbt** flag indicates if it is BabyTrain that needs to be processed. 
In which case, the scripts will treat only the uniquely identified speakers (i.e. the speakers whose name starts by **!**).

If you want to change the duration, you can add **-d** followed by the duration you want to use :

```bash
# To generate 15 seconds long enrollment (default to 30 seconds)
python diarization_to_enrollment.py chime5_diarization/ -d 15

# To generate 2 mn long trials (default to 60 seconds)
python diarization_to_enrollment.py chime5_diarization/ -d 120
```
