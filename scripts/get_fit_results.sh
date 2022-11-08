scp cluster:/cluster/gjb_lab/mgierlinski/projects/chromcom2/Snakefile .
scp cluster:/cluster/gjb_lab/mgierlinski/projects/chromcom2/config.yaml .
rsync -rvm --delete --compress cluster:/cluster/gjb_lab/mgierlinski/projects/chromcom2/fits .
rsync -rvm --delete --compress cluster:/cluster/gjb_lab/mgierlinski/projects/chromcom2/bootstraps .
