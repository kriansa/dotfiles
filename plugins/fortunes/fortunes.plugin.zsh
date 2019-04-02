# 1. Run cowsay on every new terminal spawn
[[ $- = *i* ]] && cat $HOME/Dropbox/Fortunes/*.txt | sort --random-sort | head -1 | cowsay

# 2. Disable cowsay for ansible
export ANSIBLE_NOCOWS=1
