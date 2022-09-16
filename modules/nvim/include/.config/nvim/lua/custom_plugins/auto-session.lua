local M = {}

-- Path, relative to the project where `session.vim` will be located at.
-- Typically we want that inside `.git` metadata directory so that it doesn't get versioned.
local metadir = ".git"
local metafile = metadir .. "/session.vim"

function M.load_session()
  local metafile_path = vim.fn.getcwd() .. "/" .. metafile

  if vim.fn.filereadable(metafile_path) == 0 then
    return
  end

  vim.cmd("source " .. metafile_path)

  if vim.fn.bufnr("$") <= 1 then
    return
  end

  -- Remove the file explorer buffer, as when we want to open a folder that has a session, we
  -- tipically want to see all files previously open, not the tree view.
  --
  -- The reason we attempt to remove both two variants of the explorer buffer is that sometimes when
  -- you open a directory, vim creates a buffer named "~/Project/etc" instead using the full path as
  -- the buffer name. So if you want to detect if there is an open buffer for your project folder,
  -- you have to check for both absolute path and the home-relative path.
  local explorer_buffer_tilde = vim.fn.bufnr(vim.fn.fnamemodify(vim.fn.getcwd(), ':~'))
  local explorer_buffer_absolute = vim.fn.bufnr(vim.fn.getcwd())

  if explorer_buffer_tilde ~= -1 then
    vim.cmd("bd " .. explorer_buffer_tilde)
  elseif explorer_buffer_absolute ~= -1 then
    vim.cmd("bd " .. explorer_buffer_absolute)
  end
end

function M.auto_load_session()
  if vim.fn.isdirectory(vim.v.argv[2]) == 0 then
    return
  end

  vim.cmd("cd " .. vim.v.argv[2])
  M.load_session()
end

function M.save_session()
  local meta_dir = vim.fn.getcwd() .. "/" .. metadir

  if vim.fn.isdirectory(meta_dir) == 1 then
    vim.cmd("Obsession " .. metafile)
  else
    print("This directory doesn't have a metadir (" .. meta_dir .. ") !")
  end
end

function M.setup()
  vim.cmd [[autocmd VimEnter * nested lua require('custom_plugins.auto-session').auto_load_session()]]
  vim.cmd [[command! SaveSession lua require('custom_plugins.auto-session').save_session()]]
  vim.cmd [[command! LoadSession lua require('custom_plugins.auto-session').load_session()]]
end

return M
