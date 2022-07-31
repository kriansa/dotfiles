#!/usr/bin/env bash
#
# Color codes for herbstluftwm + polybar themes

# Outputs the color as alpha + RGB (#AARRGGBB)
# Used by herbstluftwm
argb() {
  color_variable="pallete_$1"
  alpha=${2:-100}

  dec=$(( alpha * 255 / 100 ))
  printf "%s%02x" "${!color_variable}" "$dec"
}

# Outputs the color as RGB + alpha (#RRGGBBAA)
# Used by polybar & dunst
rgba() {
  color_variable="pallete_$1"
  alpha=${2:-100}

  dec=$(( alpha * 255 / 100 ))
  printf "#%02x%s" "${dec}" "${!color_variable:1}"
}

# Simply translate the variable name to the color
rgb() {
  color_variable="color_$1"
  echo "${!color_variable}"
}

pallete_white=#fafafa
pallete_black=#383a42
pallete_red=#ca1243
pallete_green=#50a14f
pallete_yellow=#c18401
pallete_cyan=#4078f2
pallete_blue=#0184bc
pallete_magenta=#a626a4

# extras
pallete_carol_blue=#019DDF
pallete_lightblue=#34C1FE
pallete_orange=#FFA630
pallete_pink=#e75a7c

# dark tones: purple-ish
pallete_violet=#493548
pallete_english_violet=#3F2E56
pallete_dark_purple=#401F3E
pallete_coffee=#3C3744

# dark tones: blue-ish
pallete_oxford=#0A2342
pallete_midnight_blue=#0E0E52
pallete_darkblue=#453F78
pallete_dark_jungle_green=#002029
pallete_dark_slate_gray=#2E5460

# light tones
pallete_ivory=#F2F5EA
pallete_gray=#EDF2F4
pallete_light_coffee=#A29C90
pallete_beige=#EDEAD0
pallete_champagne_pink=#E9DCD8

# light tones: purple-ish
pallete_african_violet=#A67DB8
pallete_purple_plum=#9361A8

# ===========
# color usage

# herbstluftwm
color_transparent=#00000000
color_primary=$pallete_oxford
color_secondary=$pallete_darkblue
color_text_primary_bg=$pallete_ivory
color_text_secondary_bg=$pallete_champagne_pink
color_text_grayed_out_fg=$pallete_dark_slate_gray

# polybar
color_bar_bg=$(rgba dark_jungle_green 80)
color_bar_hover_bg=$(rgba primary 60)
color_bar_fg=$color_text_primary_bg
color_bar_fg_alt=$pallete_african_violet
color_bar_warning=$pallete_pink
color_bar_alert=$pallete_red
color_bar_underline=$color_text_secondary_bg
color_bar_disabled=$color_text_grayed_out_fg

# dunst
color_notification_bg=$(rgba primary 87)
color_notification_bg_alert=$pallete_red
color_notification_frame=$color_text_primary_bg
color_notification_text=$color_text_primary_bg
