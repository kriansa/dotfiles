# Fortunes plugin
PLUGIN_PATH=$0:A:h

# 1. Run cowsay on every new terminal spawn
[[ $- = *i* ]] && cat $PLUGIN_PATH/data/*.txt | sort --random-sort | head -1 | cowsay

# 2. Disable cowsay for ansible
export ANSIBLE_NOCOWS=1
