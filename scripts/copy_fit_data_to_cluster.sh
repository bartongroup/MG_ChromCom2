scp ./scripts/fit_chromcom.R cluster:/cluster/gjb_lab/mgierlinski/projects/chromcom2/scripts/
rsync -rvm --delete --compress ./fit_data/* cluster:/cluster/gjb_lab/mgierlinski/projects/chromcom2/fit_data
