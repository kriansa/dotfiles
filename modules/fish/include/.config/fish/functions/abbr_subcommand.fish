# Abbreviate a subcommand (i.e. a command at position 2) of a main command.
# TODO: Remove when abbr works with specific commands.
# See: https://github.com/fish-shell/fish-shell/issues/9411
#
# Usage: abbr-subcommand <main_command> <expansion> <abbreviation>
# Example: abbr-subcommand npm install i
function abbr_subcommand
  set main_command $argv[1]
  set expansion $argv[2]
  set sub_command_abbreviation $argv[3]

  function _abbr_subcommand_{$main_command}_{$sub_command_abbreviation} \
    -V main_command -V expansion -V sub_command_abbreviation
    _abbr_subcommand_func $main_command $expansion $sub_command_abbreviation
  end

  abbr --global --add _{$main_command}_{$sub_command_abbreviation} \
    --position anywhere \
    --regex $sub_command_abbreviation \
    --function _abbr_subcommand_{$main_command}_{$sub_command_abbreviation}
end

function _abbr_subcommand_func
  set main_command $argv[1]
  set expansion $argv[2]
  set sub_command_abbreviation $argv[3]

  set -l cmd (commandline -op)
  if [ "$cmd[1]" = $main_command -a "$cmd[2]" = $sub_command_abbreviation ]
    echo $expansion
    return 0
  end
  return 1
end
