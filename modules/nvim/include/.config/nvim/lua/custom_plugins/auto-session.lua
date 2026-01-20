-- Session files are stored inside the git directory so they don't get versioned.
local metafile = "session.vim"
local shadafile = "session.shada"

-- Returns the absolute path to the git directory, or nil if not in a git repo.
-- This handles both regular repos (where .git is a directory) and worktrees (where .git is a file).
local function get_git_dir(cwd)
  local result = vim.fn.system("git -C " .. vim.fn.shellescape(cwd) .. " rev-parse --absolute-git-dir 2>/dev/null")
  if vim.v.shell_error ~= 0 then
    return nil
  end
  return vim.trim(result)
end

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

  local git_dir = get_git_dir(path_arg)

  -- If we're not in a git repo, we can't save the session there
  if git_dir == nil then
    return load_default_shada()
  end

  local metafile_path = git_dir .. "/" .. metafile
  local shadafile_path = git_dir .. "/" .. shadafile

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
  local cwd = vim.fn.getcwd()
  local git_dir = get_git_dir(cwd)

  if git_dir ~= nil then
    local shadafile_path = git_dir .. "/" .. shadafile
    local metafile_path = git_dir .. "/" .. metafile
    load_shada(shadafile_path)
    vim.cmd("Obsession " .. metafile_path)
  else
    print("Not in a git repository")
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
