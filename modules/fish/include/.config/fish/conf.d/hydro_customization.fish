status is-interactive || return

# Customization on top of Hydro theme
# See: https://github.com/jorgebucaran/hydro
#
# IMPORTANT: This file has to be named alphabetically after `hydro.fish` so it overrides the
# functions that have already been defined by it.

# Allow the prompt to have a blank line between commands
set --global _hydro_prompt_fresh_session true

# Set the value of $_hydro_color_jobs
function hydro_color_jobs --on-variable hydro_color_jobs --inherit-variable color
  set --query hydro_color_jobs && set --global _hydro_color_jobs (set_color $hydro_color_jobs)
end && hydro_color_jobs

function _hydro_prompt_new_line --description "Adds a new line after commands" --on-event fish_prompt
  set --local clear_line "\r\033[K"
  set --local new_line ''
  if test $hydro_multiline = true; and test "$_hydro_prompt_fresh_session" = false
    set new_line "\n"
  end

  echo -ne "$clear_line$new_line"
  set --global _hydro_prompt_fresh_session false
end

function _hydro_prompt_jobs --description "Sets the prompt segment for background jobs" --on-event fish_prompt
  set --local jobs (jobs -p)
  set --local njobs (count $jobs)
  contains $_hydro_last_pid $jobs; and set njobs (math $njobs - 1)

  if test $njobs -ge 1
    set --global _hydro_jobs "[$njobs] "
  else
    set --global _hydro_jobs ""
  end
end

function fish_prompt --description Hydro
  echo -e "$_hydro_color_start$hydro_symbol_start$hydro_color_normal$_hydro_color_pwd$_hydro_pwd$hydro_color_normal $_hydro_color_git$$_hydro_git$hydro_color_normal$_hydro_color_duration$_hydro_cmd_duration$_hydro_color_jobs$_hydro_jobs$hydro_color_normal$_hydro_status$hydro_color_normal "
end
