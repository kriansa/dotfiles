-- Path, relative to the project where `session.vim` will be located at.
-- Typically we want that inside `.git` metadata directory so that it doesn't get versioned.
local metadir = ".git"
local metafile = metadir .. "/session.vim"

function load_session()
  local path_arg = vim.v.argv[3]
  if vim.fn.isdirectory(path_arg) == 0 then
    return
  end

  local metafile_path = path_arg .. "/" .. metafile

  if vim.fn.filereadable(metafile_path) == 0 then
    return
  end

  vim.cmd("source " .. metafile_path)
end

function save_session()
  local meta_dir = vim.fn.getcwd() .. "/" .. metadir

  if vim.fn.isdirectory(meta_dir) == 1 then
    vim.cmd("Obsession " .. metafile)
  else
    print("This directory doesn't have the folder '" .. meta_dir .. "'")
  end
end

return {
  setup = function()
    vim.api.nvim_create_autocmd({ "VimEnter" }, { callback = load_session, nested = true })
    vim.api.nvim_create_user_command("SaveSession", save_session, {})
  end
}
