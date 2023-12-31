-- This module is a collection of functions to copy the filename/line to the clipboard

local M = {}

function M.yank_filename()
  vim.fn.setreg(0, vim.fn.expand("%"))
  print("Filename copied to clipboard")
end

function M.yank_filename_line()
  vim.fn.setreg(0, table.concat({ vim.fn.expand("%"), vim.fn.line(".") }, ":"))
  print("Filename and line number copied to clipboard")
end

function M.setup()
  vim.cmd [[command! YankFilename lua require('custom_plugins.yank-filename').yank_filename()]]
  vim.cmd [[command! YankFilenameLine lua require('custom_plugins.yank-filename').yank_filename_line()]]
end

return M
