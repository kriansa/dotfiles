-- Path, relative to the project where `session.vim` will be located at.
-- Typically we want that inside `.git` metadata directory so that it doesn't get versioned.
local metadir = ".git"
local metafile = metadir .. "/session.vim"
local shadafile = metadir .. "/session.shada"

function load_shada(path)
  vim.opt.shadafile = path
  vim.cmd("silent! rshada!")
end

function load_default_shada()
  load_shada(vim.fn.stdpath("state") .. "/shada/main.shada")
end

function load_session()
  local path_arg = vim.v.argv[3]

  -- If we're opening a specific file, then no need to load the session
  if vim.fn.isdirectory(path_arg) == 0 then
    return load_default_shada()
  end

  local metadir_path = path_arg .. "/" .. metadir
  local metafile_path = path_arg .. "/" .. metafile
  local shadafile_path = path_arg .. "/" .. shadafile

  -- If we don't have the metadir, then we can't save the session there
  if vim.fn.isdirectory(metadir_path) == 0 then
    return load_default_shada()
  end

  -- If we're loading a path that doesn't have a session yet, create it
  if vim.fn.filereadable(metafile_path) == 0 then
    return save_session()
  end

  -- Load the session
  load_shada(shadafile_path)
  vim.cmd("source " .. metafile_path)

  -- If after loading the session, any of these buffers exist, we can close them.
  buffers_to_close = {
    vim.fn.getcwd(),
    "oil://" .. vim.fn.getcwd() .. "/",
  }

  for _, buf in ipairs(buffers_to_close) do
    if vim.fn.bufexists(buf) == 1 then
      vim.cmd("bd " .. buf)
    end
  end
end

function save_session()
  local meta_dir = vim.fn.getcwd() .. "/" .. metadir

  if vim.fn.isdirectory(meta_dir) == 1 then
    load_shada(shadafile_path)
    vim.cmd("Obsession " .. metafile)
  else
    print("This directory doesn't have the folder '" .. meta_dir .. "'")
  end
end

return {
  setup = function()
    -- This ensures that once we load Neovim, shada is turned off completely and we only set it
    -- after we know what is the correct file to be loaded. This prevents that the `main.shada` gets
    -- loaded before the project's, thus making it inherit whatever was in the default shada.
    vim.opt.shadafile = "NONE"
    vim.api.nvim_create_autocmd({ "VimEnter" }, { callback = load_session, nested = true })
    vim.api.nvim_create_user_command("SaveSession", save_session, {})
  end,
}
