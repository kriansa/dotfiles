local M = {}

function M.is_buffer_writable()
  local bufname = vim.api.nvim_buf_get_name(0)
  local fugitive_status = not not vim.regex("\\.git/index$"):match_str(bufname)
  local fugitive_diff = not not vim.regex("^fugitive://"):match_str(bufname)
  local blank_buffer = bufname == ""

  return not vim.o.diff and vim.o.modifiable and vim.o.modified and
    vim.o.buftype ~= "nofile" and vim.o.buftype ~= "terminal" and vim.o.buftype ~= "prompt" and
    not vim.o.readonly and not fugitive_status and not fugitive_diff and not blank_buffer
end

function M.write_buffer()
  if not M.is_buffer_writable() then
    return
  end

  -- Preserve marks that are used to remember start and
  -- end position of the last changed or yanked text (`:h '[`).
  local first_char_pos = vim.fn.getpos("'[")
  local last_char_pos = vim.fn.getpos("']")

  vim.cmd("write")

  vim.fn.setpos("'[", first_char_pos)
  vim.fn.setpos("']", last_char_pos)
end

function M.auto_save()
  if not vim.g.auto_save then
    return
  end

  M.write_buffer()
end

function M.auto_save_toggle()
  if vim.g.auto_save then
    vim.g.auto_save = false
    print("Auto save: OFF")
  else
    vim.g.auto_save = true
    print("Auto save: ON")
  end
end

function M.setup(opts)
  opts = opts or {}

  if opts.enable then
    vim.g.auto_save = true
  end

  vim.cmd [[autocmd BufLeave,FocusLost * silent! lua require('custom_plugins.auto-save').auto_save()]]
  vim.cmd [[command! AutoSaveToggle lua require('custom_plugins.auto-save').auto_save_toggle()]]
end

return M
