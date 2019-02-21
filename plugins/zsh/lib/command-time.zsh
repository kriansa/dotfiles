# This module hooks into the ZSH's command execution to provide a total execution time for commands
# that take more than $ZSH_COMMAND_TIME_MIN_SECONDS seconds.

_command_time_preexec() {
  _timer=${_timer:-$SECONDS}
  ZSH_COMMAND_TIME_MSG=${ZSH_COMMAND_TIME_MSG-"Time: %s"}
  ZSH_COMMAND_TIME_COLOR=${ZSH_COMMAND_TIME_COLOR-"cyan"}
  ZSH_COMMAND_TIME_MIN_SECONDS=${ZSH_COMMAND_TIME_MIN_SECONDS:-3}
  ZSH_COMMAND_TIME=""
}

_command_time_precmd() {
  if [ $_timer ]; then
    timer_show=$(($SECONDS - $_timer))
    if [ -n "$TTY" ] && [ $timer_show -ge ${ZSH_COMMAND_TIME_MIN_SECONDS:-3} ]; then
      ZSH_COMMAND_TIME="$timer_show"
      if [ ! -z ${ZSH_COMMAND_TIME_MSG} ]; then
        zsh_command_time
      fi
    fi
    unset _timer
  fi
}

zsh_command_time() {
  if [ -n "$ZSH_COMMAND_TIME" ]; then
    timer_show=$(printf '%dh:%02dm:%02ds\n' $(($ZSH_COMMAND_TIME/3600)) $(($ZSH_COMMAND_TIME%3600/60)) $(($ZSH_COMMAND_TIME%60)))
  fi
}

precmd_functions+=(_command_time_precmd)
preexec_functions+=(_command_time_preexec)
