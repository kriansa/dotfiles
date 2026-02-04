-- Ctrl+Option+R = Reload config
hs.hotkey.bind({"alt", "ctrl"}, "R", function()
  hs.reload()
end)

-- Enable IPC
require("hs.ipc")

-- No window animations
hs.window.animationDuration = 0

-- Gap configuration
local gap = {
  outer = 10,  -- gap from screen edges
  inner = 10,  -- gap between windows
}
local fullscreenUseGaps = false  -- toggle for fullscreen gaps

-- Cache the window sizes for using with maximized toggler
local frameCache = {}

-- Toggle a window between its normal size, and being maximized
hs.hotkey.bind({"cmd", "shift"}, "Up", function()
  local win = hs.window.focusedWindow()
  local max = win:screen():frame()

  -- Calculate the maximized frame (with or without gaps)
  local maximizedFrame
  if fullscreenUseGaps then
    maximizedFrame = {
      x = max.x + gap.outer,
      y = max.y + gap.outer,
      w = max.w - (2 * gap.outer),
      h = max.h - (2 * gap.outer),
    }
  else
    maximizedFrame = max
  end

  local currentFrame = win:frame()
  local isMaximized = (currentFrame.x == maximizedFrame.x and
                       currentFrame.y == maximizedFrame.y and
                       currentFrame.w == maximizedFrame.w and
                       currentFrame.h == maximizedFrame.h)

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
    win:setFrame(maximizedFrame)
  end
end)

-- Move window to the left
hs.hotkey.bind({"cmd", "shift"}, "Left", function()
  local win = hs.window.frontmostWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + gap.outer
  f.y = max.y + gap.outer
  f.w = (max.w / 2) - gap.outer - (gap.inner / 2)
  f.h = max.h - (2 * gap.outer)
  win:setFrame(f)
end)

-- Move window to the right
hs.hotkey.bind({"cmd", "shift"}, "Right", function()
  local win = hs.window.frontmostWindow()
  local f = win:frame()
  local screen = win:screen()
  local max = screen:frame()

  f.x = max.x + (max.w / 2) + (gap.inner / 2)
  f.y = max.y + gap.outer
  f.w = (max.w / 2) - gap.outer - (gap.inner / 2)
  f.h = max.h - (2 * gap.outer)
  win:setFrame(f)
end)

hs.alert.show("Loaded")
