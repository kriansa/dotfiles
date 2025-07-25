complete --command c --no-files --description="Project dir" \
  --arguments="(complete --do-complete=\"'' $PROJECTS/\" | string replace '$PROJECTS/' '')" \
  --condition="__fish_is_first_token"
