status is-interactive || exit

# The background job that reads the repository hands its result back through this
# file rather than a universal variable, because fish saves universal variables to
# fish_variables and every open shell would keep that file changing.
set --global _hydro_git_value ""
set --global _hydro_git_file $XDG_RUNTIME_DIR
test -z "$_hydro_git_file" && set _hydro_git_file $TMPDIR
test -z "$_hydro_git_file" && set _hydro_git_file /tmp
set _hydro_git_file (path normalize $_hydro_git_file/hydro-$fish_pid)

# An earlier shell may have died holding this same process id, so never inherit the
# branch it left in the file.
printf '' >$_hydro_git_file

function _hydro_git_receive --on-signal SIGUSR1
    test -r $_hydro_git_file || return
    read --local value <$_hydro_git_file
    set --global _hydro_git_value $value
    commandline --function repaint
end

# The stock fish_title runs `prompt_pwd` on every prompt, and prompt_pwd assigns
# fish_prompt_pwd_dir_length as a local, which fish reports to handlers exactly like
# a real change. Watching that variable here would rebuild the path on every prompt,
# and the rebuild runs `git rev-parse`, so the prompt would wait on a git process
# before it could appear. A local assignment cannot be told apart from a global one,
# so the setting is read when the path is built instead of watched. Changing it in a
# running shell needs `cd .` to take effect.
function _hydro_pwd --on-variable PWD --on-variable hydro_ignored_git_paths
    set --local git_root (command git --no-optional-locks rev-parse --show-toplevel 2>/dev/null)
    set --local git_base (string replace --all --regex -- "^.*/" "" "$git_root")
    set --local path_sep /

    test "$fish_prompt_pwd_dir_length" = 0 && set path_sep

    if set --query git_root[1] && ! contains -- $git_root $hydro_ignored_git_paths
        set --erase _hydro_skip_git_prompt
    else
        set --global _hydro_skip_git_prompt
    end

    set --global _hydro_pwd (
        string replace --ignore-case --regex -- (string join "" "^" ~) \~ $PWD |
        string replace -- "/$git_base/" /\x1e/ |
        string replace --regex --all -- "(\.?[^/]{"(
            string replace --regex --all -- '^$' 1 "$fish_prompt_pwd_dir_length"
        )"})[^/]*/" "\$1$path_sep" |
        string replace -- \x1e "$git_base" |
        string replace --regex -- '([^/]+)$' "\x1b[1m\$1\x1b[22m" |
        string replace --regex --all -- '(?!^/$)/|^$' "\x1b[2m/\x1b[22m"
    )
end

function _hydro_postexec --on-event fish_postexec
    set --local last_status $pipestatus
    set --global _hydro_status "$_hydro_newline$_hydro_color_prompt$hydro_symbol_prompt"

    for code in $last_status
        if test $code -ne 0
            set --global _hydro_status "$_hydro_color_error| "(echo $last_status)" $_hydro_newline$_hydro_color_prompt$_hydro_color_error$hydro_symbol_prompt"
            break
        end
    end

    test "$CMD_DURATION" -lt $hydro_cmd_duration_threshold && set _hydro_cmd_duration && return

    set --local secs (math --scale=1 $CMD_DURATION/1000 % 60)
    set --local mins (math --scale=0 $CMD_DURATION/60000 % 60)
    set --local hours (math --scale=0 $CMD_DURATION/3600000)

    set --local out

    test $hours -gt 0 && set --local --append out $hours"h"
    test $mins -gt 0 && set --local --append out $mins"m"
    test $secs -gt 0 && set --local --append out $secs"s"

    set --global _hydro_cmd_duration "$out "
end

function _hydro_prompt --on-event fish_prompt
    set --query _hydro_status || set --global _hydro_status "$_hydro_newline$_hydro_color_prompt$hydro_symbol_prompt"
    set --query _hydro_pwd || _hydro_pwd

    command kill $_hydro_last_pid 2>/dev/null

    # Emptying the file makes the next repository publish its branch immediately
    # rather than waiting for the full segment.
    if set --query _hydro_skip_git_prompt
        set --global _hydro_git_value ""
        printf '' >$_hydro_git_file
        return
    end

    # This shell expands $_hydro_git_file and $fish_pid while building the string, so
    # the job writes to the right file and signals back to here.
    fish --private --command "
        set branch (
            command git branch --show-current 2>/dev/null ||
            command git describe --tags --exact-match HEAD 2>/dev/null ||
            command git rev-parse --short HEAD 2>/dev/null |
                string replace --regex -- '(.+)' '@\$1'
        )

        function _hydro_publish
            printf '%s' \"\$argv[1]\" >\"$_hydro_git_file.new\"
            command mv -f \"$_hydro_git_file.new\" \"$_hydro_git_file\"
            command kill -USR1 $fish_pid 2>/dev/null
        end

        test -s \"$_hydro_git_file\" || _hydro_publish \"\$branch \"

        command git diff-index --quiet HEAD 2>/dev/null
        test \$status -eq 1 ||
            count (command git ls-files --others --exclude-standard (command git rev-parse --show-toplevel)) >/dev/null && set info \"$hydro_symbol_git_dirty\"

        for fetch in $hydro_fetch false
            command git rev-list --count --left-right @{upstream}...@ 2>/dev/null |
                read behind ahead

            switch \"\$behind \$ahead\"
                case \" \" \"0 0\"
                case \"0 *\"
                    set upstream \" $_hydro_color_git_ahead$hydro_symbol_git_ahead\$ahead$_hydro_color_git\"
                case \"* 0\"
                    set upstream \" $_hydro_color_git_behind$hydro_symbol_git_behind\$behind$_hydro_color_git\"
                case \*
                    set upstream \" $_hydro_color_git_ahead$hydro_symbol_git_ahead\$ahead $_hydro_color_git_behind$hydro_symbol_git_behind\$behind$_hydro_color_git\"
            end

            _hydro_publish \"\$branch\$info\$upstream \"

            test \$fetch = true && command git fetch --no-tags 2>/dev/null
        end
    " &

    set --global _hydro_last_pid $last_pid
end

function _hydro_fish_exit --on-event fish_exit
    command kill $_hydro_last_pid 2>/dev/null
    command rm -f $_hydro_git_file $_hydro_git_file.new
end

function _hydro_prompt_jobs --on-event fish_prompt
    set --local jobs (jobs -p)
    set --local njobs (count $jobs)

    # The job that reads the repository is one of ours, not something the user started.
    contains "$_hydro_last_pid" $jobs && set njobs (math $njobs - 1)

    if test $njobs -ge 1
        set --global _hydro_jobs "[$njobs] "
    else
        set --global _hydro_jobs ""
    end
end

# Keeps a blank line between commands, skipping it for the first prompt of the shell
# so the session does not open with a stray empty line.
set --global _hydro_first_prompt true

function _hydro_blank_line --on-event fish_prompt
    set --local clear_line "\r\033[K"
    set --local blank_line ""

    test "$hydro_multiline" = true && test "$_hydro_first_prompt" = false
    and set blank_line "\n"

    echo -ne "$clear_line$blank_line"
    set --global _hydro_first_prompt false
end

function _hydro_uninstall --on-event hydro_uninstall
    command rm -f $_hydro_git_file $_hydro_git_file.new
    set --names |
        string replace --filter --regex -- "^(_?hydro_)" "set --erase \$1" |
        source
    functions --erase (functions --all | string match --entire --regex "^_?hydro_")
end

set --global hydro_color_normal (set_color normal)

for color in hydro_color_{pwd,git,error,prompt,duration,start,jobs,git_ahead,git_behind}
    function $color --on-variable $color --inherit-variable color
        set --query $color && set --global _$color (set_color $$color)
    end && $color
end

function hydro_multiline --on-variable hydro_multiline
    if test "$hydro_multiline" = true
        set --global _hydro_newline "\n"
    else
        set --global _hydro_newline ""
    end
end && hydro_multiline

set --query hydro_color_error || set --global hydro_color_error $fish_color_error
set --query hydro_color_git_ahead || set --global hydro_color_git_ahead green
set --query hydro_color_git_behind || set --global hydro_color_git_behind red
set --query hydro_symbol_prompt || set --global hydro_symbol_prompt ❱
set --query hydro_symbol_git_dirty || set --global hydro_symbol_git_dirty •
set --query hydro_symbol_git_ahead || set --global hydro_symbol_git_ahead ↑
set --query hydro_symbol_git_behind || set --global hydro_symbol_git_behind ↓
set --query hydro_multiline || set --global hydro_multiline false
set --query hydro_cmd_duration_threshold || set --global hydro_cmd_duration_threshold 1000
