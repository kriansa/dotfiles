# Safe removal from terminal so we can save ourselves from accidents or mistypings
function rm
  echo "This is not the actual rm command. Use `command rm` if you want to remove permanently."
  gio trash $argv
end
