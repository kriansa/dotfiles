#!/usr/bin/env bash
#
# Color codes for herbstluftwm + polybar themes

# Outputs the color as alpha + RGB (#AARRGGBB)
# Used by herbstluftwm
argb() {
  color_variable="color_$1"
  alpha=${2:-100}

  dec=$(( alpha * 255 / 100 ))
  printf "%s%02x" "${!color_variable}" "$dec"
}

# Outputs the color as RGB + alpha (#RRGGBBAA)
# Used by polybar & dunst
rgba() {
  color_variable="color_$1"
  alpha=${2:-100}

  dec=$(( alpha * 255 / 100 ))
  printf "#%02x%s" "${dec}" "${!color_variable:1}"
}

# Simply translate the variable name to the color
rgb() {
  color_variable="color_$1"
  echo "${!color_variable}"
}

color_white=#fafafa
color_black=#383a42
color_red=#ca1243
color_green=#50a14f
color_yellow=#c18401
color_cyan=#4078f2
color_blue=#0184bc
color_magenta=#a626a4

# extras
color_carol_blue=#019DDF
color_lightblue=#34C1FE
color_orange=#FFA630
color_pink=#e75a7c

# dark tones: purple-ish
color_violet=#493548
color_english_violet=#3F2E56
color_dark_purple=#401F3E
color_coffee=#3C3744

# dark tones: blue-ish
color_oxford=#0A2342
color_midnight_blue=#0E0E52
color_darkblue=#453F78
color_dark_jungle_green=#002029
color_dark_slate_gray=#2E5460

# light tones
color_ivory=#F2F5EA
color_gray=#EDF2F4
color_light_coffee=#A29C90
color_beige=#EDEAD0
color_champagne_pink=#E9DCD8

# light tones: purple-ish
color_african_violet=#A67DB8
color_purple_plum=#9361A8

# ===========
# color usage

# herbstluftwm
color_transparent=#00000000
color_primary=$color_oxford
color_secondary=$color_dark_slate_gray
color_text_primary_bg=$color_ivory
color_text_secondary_bg=$color_champagne_pink
color_text_grayed_out_fg=$color_dark_slate_gray

# polybar
color_bar_bg=$(rgba dark_jungle_green 80)
color_bar_hover_bg=$(rgba oxford 60)
color_bar_fg=$color_text_primary_bg
color_bar_fg_alt=$color_african_violet
color_bar_warning=$color_pink
color_bar_alert=$color_red
color_bar_underline=$color_text_secondary_bg
color_bar_disabled=$color_text_grayed_out_fg

# dunst
color_notification_bg=$(rgba oxford 87)
color_notification_bg_alert=$color_red
color_notification_frame=$color_text_primary_bg
color_notification_text=$color_text_primary_bg

# rofi

# zathura
color_zathura_bg=$color_gray
color_zathura_text_fg=$color_ivory
color_zathura_statusbar_bg=$color_primary
