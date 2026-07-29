function fish_prompt --description Hydro
    echo -e -n "$_hydro_color_start$hydro_symbol_start$hydro_color_normal$_hydro_color_pwd$_hydro_pwd$hydro_color_normal $_hydro_color_git$_hydro_git_value$hydro_color_normal$_hydro_color_duration$_hydro_cmd_duration$_hydro_color_jobs$_hydro_jobs$hydro_color_normal$_hydro_status$hydro_color_normal "
end
