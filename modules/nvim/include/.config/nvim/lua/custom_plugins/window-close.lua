-- This module is just a smart way of closing a buffer by just using a single command.
-- Some buffers should have their splits closed as well, so we just save an extra cmd.

local M = {}
function should_close_split()
  local bufname = vim.api.nvim_buf_get_name(0)

  local fugitive_status = vim.o.filetype == "fugitive"
  local editing_commit_message = vim.o.filetype == "gitcommit" or
    vim.o.filetype == "NeogitCommitMessage"
  local fugitive_diff = not not vim.regex("^fugitive://"):match_str(bufname)

  return vim.o.diff or vim.o.buftype == "help" or vim.o.buftype == "quickfix" or
    vim.o.buftype == 'nofile' or editing_commit_message or fugitive_diff or
    fugitive_status
end

function M.close()
  require("custom_plugins.auto-save").write_buffer()

  if should_close_split() then
    vim.cmd("bd")
  else
    Snacks.bufdelete.delete()
  end
end

function M.setup()
  vim.cmd [[command! Close lua require('custom_plugins.window-close').close()]]
end

return M
