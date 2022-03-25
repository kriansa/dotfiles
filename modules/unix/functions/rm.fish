# Safe removal from terminal so we can save ourselves from accidents or mistypings
function rm
  echo "Moving to trash. Use `command rm` if you want to remove permanently."
  gio trash $argv
end
