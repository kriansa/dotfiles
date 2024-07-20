-- This effectively replaces the `set clipboard=unnamedplus` option, which sets every yanked text to
-- the system clipboard. The problem with that is that when doing several deletes or changes in a
-- row, the clipboard will keep getting overwritten, as well as calling the system clipboard app
-- every time. On GNOME Wayland, this is particularly slow, because it blocks the UI and steals the
-- focus, causing events such as `FocusLost` and `FocusGained` to be triggered constantly, which is
-- not what we want.
--
-- This way, it will only copy to the system clipboard the last yanked text when the editor loses
-- focus, which is usually when you switch to another application. We also set a timer so we avoid
-- conflicts while alt-tabbing, especially on GNOME where the wl-clipboard steals the focus, causing
-- an infinite loop.
--
-- See: https://github.com/neovim/neovim/issues/11804
-- See: https://github.com/neovim/neovim/issues/24470
-- See: https://github.com/bugaevc/wl-clipboard/issues/90
function with_locked_clipboard(fn)
  return function()
    -- Only sets the register if the timer is not running
    if vim.g._clipboard_lock then
      return
    end

    fn()
    vim.g._clipboard_lock = true

    -- Starts the timer
    vim.defer_fn(function()
      vim.g._clipboard_lock = false
    end, 500)
  end
end

vim.api.nvim_create_autocmd({"FocusLost", "VimLeave"}, {
  pattern = "*",
  callback = with_locked_clipboard(function()
    local current_yank = vim.fn.getreg(0)

    -- Avoid touching the system clipboard register if it hasn't been changed. It helps with not
    -- spamming wl-clipboard on GNOME Wayland after we LostFocus
    if vim.g._last_yank ~= current_yank then
      vim.fn.setreg("+", current_yank)
      vim.g._last_yank = current_yank
    end
  end)
})

vim.api.nvim_create_autocmd("FocusGained", {
  pattern = "*",
  callback = function()
    -- Sometimes it may take a while before the + register reflects the content on system clipboard,
    -- so we need to compensate for that by rewriting the @ register 100ms after getting focused.
    vim.defer_fn(function()
      vim.fn.setreg("@", vim.fn.getreg("+"))
      vim.g._last_yank = vim.fn.getreg("+")
    end, 100)

    -- Always reset last yank cache when returning to vim, so next time we lost the focus we ensure
    -- that we always copy whatever is in the clipboard
    vim.g._last_yank = nil
  end
})
