local wezterm = require 'wezterm'
local act = wezterm.action
local config = wezterm.config_builder()

config.default_prog = { '/bin/sh', '-lc', 'tmux new-session -A' }
config.term = "wezterm"
local font = function(attributes)
  font = { family = "Iosevka Fixed" }
  if attributes then
    for k, v in pairs(attributes) do
      font[k] = v
    end
  end

  return wezterm.font_with_fallback({
    font,
    -- Explicitly set the fallback font to Nerd Font Symbols so it picks up the installed font
    -- instead of the built in one, which may have different glyphs and sizes
    "Symbols Nerd Font",
  })
end
config.font = font()
config.font_rules = {
  -- This is the variant of the normal "font" attribute, but for italic effect (\033[3m)
  { intensity = "Normal", italic = true, font = font({ style = "Oblique" }) },

  -- This is for when we specify the font effect "Faint" (decreased intensity) (\033[2m)
  { intensity = "Half", italic = false, font = font({ weight = "Light" }) },
  { intensity = "Half", italic = true, font = font({ weight = "Light", style = "Oblique" }) },

  -- And this is for when specified the font effect "Bold" (\033[1m)
  { intensity = "Bold", italic = false, font = font({ weight = "Bold" }) },
  { intensity = "Bold", italic = true, font = font({ weight = "Bold", style = "Oblique" }) },
}
config.window_close_confirmation = 'NeverPrompt'
config.window_decorations = "RESIZE"
config.cell_width = 0.9
if wezterm.target_triple == "aarch64-apple-darwin" then
  config.font_size = 23.0
else
  config.font_size = 17.0
end
config.harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0', 'VSAB=9' }
config.color_scheme = 'One Light (base16)'
config.disable_default_mouse_bindings = true
config.disable_default_quick_select_patterns = true
config.enable_tab_bar = false
config.enable_scroll_bar = false
config.default_cursor_style = 'BlinkingBlock'
config.audible_bell = 'Disabled'
config.cursor_blink_ease_in = 'Constant'
config.cursor_blink_ease_out = 'Constant'
config.cursor_blink_rate = 500
config.window_padding = {
  left = 4,
  right = 0,
  top = 0,
  bottom = 0,
}

-- Only on nightly builds
config.window_content_alignment = {
  horizontal = 'Center',
  vertical = 'Center',
}

config.disable_default_key_bindings = true

local pkey = function(str)
  return act.SendString("\x1C" .. str)
end

config.keys = {
  { key = 't', mods = 'SUPER', action = pkey("t") },
  { key = 'd', mods = 'SUPER', action = pkey("d") },
  { key = 'd', mods = 'SUPER|SHIFT', action = pkey("D") },
  { key = 'Enter', mods = 'SUPER|SHIFT', action = pkey("\r") },

  { key = 'h', mods = 'SUPER|SHIFT', action = pkey("H") },
  { key = 'j', mods = 'SUPER|SHIFT', action = pkey("J") },
  { key = 'k', mods = 'SUPER|SHIFT', action = pkey("K") },
  { key = 'l', mods = 'SUPER|SHIFT', action = pkey("L") },

  { key = 'h', mods = 'SUPER', action = pkey("h") },
  { key = 'j', mods = 'SUPER', action = pkey("j") },
  { key = 'k', mods = 'SUPER', action = pkey("k") },
  { key = 'l', mods = 'SUPER', action = pkey("l") },

  { key = '1', mods = 'SUPER', action = pkey("1") },
  { key = '2', mods = 'SUPER', action = pkey("2") },
  { key = '3', mods = 'SUPER', action = pkey("3") },
  { key = '4', mods = 'SUPER', action = pkey("4") },
  { key = '5', mods = 'SUPER', action = pkey("5") },
  { key = '6', mods = 'SUPER', action = pkey("6") },
  { key = '7', mods = 'SUPER', action = pkey("7") },
  { key = '8', mods = 'SUPER', action = pkey("8") },
  { key = '9', mods = 'SUPER', action = pkey("9") },
  { key = '0', mods = 'SUPER', action = pkey("0") },

  { key = ']', mods = 'SUPER', action = pkey("]") },
  { key = '[', mods = 'SUPER', action = pkey("[") },

  -- Default keybindings
  { key = 'q', mods = 'SUPER', action = act.QuitApplication },
  -- { key = 'c', mods = 'SHIFT|CTRL', action = act.CopyTo 'Clipboard' },
  -- { key = 'c', mods = 'SUPER', action = act.CopyTo 'Clipboard' },
  -- { key = 'C', mods = 'CTRL', action = act.CopyTo 'Clipboard' },
  -- { key = 'C', mods = 'SHIFT|CTRL', action = act.CopyTo 'Clipboard' },
  -- { key = 'Insert', mods = 'SHIFT', action = act.PasteFrom 'PrimarySelection' },
  -- { key = 'Insert', mods = 'CTRL', action = act.CopyTo 'PrimarySelection' },
  { key = 'Copy', mods = 'NONE', action = act.CopyTo 'Clipboard' },
  { key = 'Paste', mods = 'NONE', action = act.PasteFrom 'Clipboard' },

  -- { key = 'V', mods = 'CTRL', action = act.PasteFrom 'Clipboard' },
  -- { key = 'V', mods = 'SHIFT|CTRL', action = act.PasteFrom 'Clipboard' },
  { key = 'v', mods = 'SHIFT|CTRL', action = act.PasteFrom 'Clipboard' },
  { key = 'v', mods = 'SUPER', action = act.PasteFrom 'Clipboard' },

  { key = '=', mods = 'CTRL', action = act.IncreaseFontSize },
  { key = '+', mods = 'CTRL', action = act.IncreaseFontSize },
  { key = '-', mods = 'CTRL', action = act.DecreaseFontSize },
  { key = '_', mods = 'CTRL', action = act.DecreaseFontSize },
  { key = '0', mods = 'CTRL', action = act.ResetFontSize },
  { key = ')', mods = 'CTRL', action = act.ResetFontSize }, -- This is the number 0
}

return config
