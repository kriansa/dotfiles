if status is-interactive
  # Load theme
  source (/usr/bin/starship init fish --print-full-init | psub)
  # _pure_set_default pure_show_system_time true
  # _pure_set_default pure_threshold_command_duration 2
  # _pure_set_default pure_color_system_time grey --reverse

  # Set this helper variables used across my scripts
  set --global OS (uname)
end
