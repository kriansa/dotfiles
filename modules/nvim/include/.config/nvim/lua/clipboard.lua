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
vim.api.nvim_create_autocmd({"FocusLost", "VimLeave"}, {
  pattern = "*",
  callback = function()
    -- Ensure this function is called at most once every 500ms
    vim.uv.update_time()
    local now = vim.uv.now()
    if vim.g._last_unfocus_time and (now - vim.g._last_unfocus_time) < 500 then
      return
    end
    vim.g._last_unfocus_time = now

    local current_yank = vim.fn.getreg(0)
    vim.g._is_focused = false

    -- Avoid touching the system clipboard register if it hasn't been changed. It helps with not
    -- spamming wl-clipboard on GNOME Wayland after FocusLost
    if vim.g._last_yank ~= current_yank then
      vim.fn.setreg("+", current_yank)
      vim.g._last_yank = current_yank
    end
  end
})

vim.api.nvim_create_autocmd({ "FocusGained", "VimEnter" }, {
  pattern = "*",
  callback = function()
    -- Ensure this function is called at most once every 500ms
    vim.uv.update_time()
    local now = vim.uv.now()
    if vim.g._last_focus_time and (now - vim.g._last_focus_time) < 500 then
      return
    end
    vim.g._last_focus_time = now

    vim.g._is_focused = true

    -- It may take a while before the + register reflects the contents of system clipboard, so we
    -- need to compensate for that by rewriting the @ register 300ms after getting focused.
    vim.defer_fn(function()
      if vim.g._is_focused == false then
        return
      end

      vim.g._last_yank = vim.fn.getreg("+")
      vim.fn.setreg("@", vim.g._last_yank)
    end, 300)
  end
})
