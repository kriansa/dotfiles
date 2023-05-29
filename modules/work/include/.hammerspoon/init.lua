-- Ctrl+Option+R = Reload config
hs.hotkey.bind({"alt", "ctrl"}, "R", function()
  hs.reload()
end)

-- Global shortcut for DeepL
hs.hotkey.bind({"cmd"}, "Escape", function()
  local app_name = hs.window.focusedWindow():application():title()

  if app_name == "DeepL" then
    hs.eventtap.keyStroke({"cmd"}, "q")
  else
    hs.application.open("DeepL")
  end
end)

-- No window animations
hs.window.animationDuration = 0

-- Cache the window sizes for using with maximized toggler
local frameCache = {}

-- Toggle a window between its normal size, and being maximized
hs.hotkey.bind({"shift", "alt"}, "Return", function()
   local win = hs.window.focusedWindow()
   if frameCache[win:id()] then
      win:setFrame(frameCache[win:id()])
      frameCache[win:id()] = nil
   else
      frameCache[win:id()] = win:frame()
      win:maximize()
   end
end)

-- Alt+shift+H - Move window to the left
hs.hotkey.bind({"shift", "alt"}, "H", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = (max.w / 2) - 1
  f.h = max.h
  win:setFrame(f)
end)

-- Alt+shift+L - Move window to the right
hs.hotkey.bind({"shift", "alt"}, "L", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + (max.w / 2)
  f.y = max.y
  f.w = (max.w / 2) - 1
  f.h = max.h
  win:setFrame(f)
end)

-- Alt+[hjkl] - Move focus
hs.hotkey.bind({"alt"}, "L", function()
  hs.window.focusedWindow():focusWindowEast()
end)

hs.hotkey.bind({"alt"}, "H", function()
  hs.window.focusedWindow():focusWindowWest()
end)

hs.hotkey.bind({"alt"}, "J", function()
  hs.window.focusedWindow():focusWindowSouth()
end)

hs.hotkey.bind({"alt"}, "K", function()
  hs.window.focusedWindow():focusWindowNorth()
end)

hs.alert.show("Loaded")
