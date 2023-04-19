-- Ctrl+Option+R = Reload config
hs.hotkey.bind({"alt", "ctrl"}, "R", function()
  hs.reload()
end)

-- No window animations
hs.window.animationDuration = 0

-- Alt+shift+H - Move window to the left
hs.hotkey.bind({"shift", "alt"}, "H", function()
  local win = hs.window.focusedWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x
  f.y = max.y
  f.w = max.w / 2
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
  f.w = max.w / 2
  f.h = max.h
  win:setFrame(f)
end)

-- Stop hiding windows
hs.hotkey.bind({"cmd"}, "H", function() end)

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
