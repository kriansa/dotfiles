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
hs.hotkey.bind({"cmd", "shift"}, "Up", function()
  local win = hs.window.focusedWindow()
  local isMaximized = win:frame() == win:screen():frame()

  -- If the window is not maximized but had been previously maximized, it means it has been resized
  -- since its last maximization. In that case, we erase the cache.
  if not isMaximized and frameCache[win:id()] then
    frameCache[win:id()] = nil
  end

  if frameCache[win:id()] then
    win:setFrame(frameCache[win:id()])
    frameCache[win:id()] = nil
  else
    frameCache[win:id()] = win:frame()
    win:maximize()
  end
end)

-- Move window to the left
hs.hotkey.bind({"cmd", "shift"}, "Left", function()
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

-- Move window to the right
hs.hotkey.bind({"cmd", "shift"}, "Right", function()
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
-- hs.hotkey.bind({"alt"}, "L", function()
--   hs.window.focusedWindow():focusWindowEast()
-- end)
--
-- hs.hotkey.bind({"alt"}, "H", function()
--   hs.window.focusedWindow():focusWindowWest()
-- end)
--
-- hs.hotkey.bind({"alt"}, "J", function()
--   hs.window.focusedWindow():focusWindowSouth()
-- end)
--
-- hs.hotkey.bind({"alt"}, "K", function()
--   hs.window.focusedWindow():focusWindowNorth()
-- end)

hs.alert.show("Loaded")
