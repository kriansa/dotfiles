complete --command a --no-files --description="Project dir" \
  --arguments="(complete --do-complete=\"'' $PROJECTS/\" | string replace '$PROJECTS/' '' | string replace --regex '/\$' '')" \
  --condition="__fish_is_first_token"
