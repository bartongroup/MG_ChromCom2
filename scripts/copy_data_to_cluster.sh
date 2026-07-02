#rsync -rvm --delete --compress ./data/* cluster:/cluster/gjb_lab/mgierlinski/projects/chromcom2/data
rsync -rvm --delete --compress ./data/* shiny:/home/mgierlinski/ShinyApps/private/chromcom2/data
rsync -rvm ./shiny/parsing_states/cache/* shiny:/home/mgierlinski/ShinyApps/private/chromcom2/parsing_states/cache
