# 1. Enable autocomplete
#
if [[ "$OSTYPE" == "darwin"* ]] && type brew &> /dev/null; then
  source $(brew --prefix awscli)/libexec/bin/aws_zsh_completer.sh
else
  source $(which aws_zsh_completer.sh)
fi
