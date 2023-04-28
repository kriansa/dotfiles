local M = {}

function get_search_dirs()
  local cwd = vim.fn.getcwd()

  -- `dirs` is a key-valued table and we add values as keys so they aren't duplicated
  local dirs = {}
  for _, node in ipairs(require("nvim-tree.api").marks.list()) do
    local abspath

    -- If the selected node is a file, then we use its parent directory in the search
    if node.fs_stat.type == "file" then
      abspath = node.parent.absolute_path
    else
      abspath = node.absolute_path
    end

    -- If the selected directory is not under CWD, let's add CWD to the search path instead
    if abspath:sub(1, #cwd) ~= cwd then
      dirs[cwd] = true
    else
      dirs[abspath] = true
    end
  end

  -- transform `dirs` keys back into values
  local dirs_values = {}
  for dirname, _ in pairs(dirs) do
    dirs_values[#dirs_values + 1] = dirname
  end

  return dirs_values
end

function M.find_buffers()
  require("telescope.builtin").buffers()
end

function M.find_files()
  require("telescope.builtin").find_files({ search_dirs = get_search_dirs() })
end

function M.live_grep()
  require("telescope.builtin").live_grep({ search_dirs = get_search_dirs() })
end

function M.live_grep_current_word()
  require("telescope.builtin").live_grep({
    default_text=vim.fn.expand("<cword>"),
    search_dirs = get_search_dirs(),
  })
end

function M.live_grep_current_selection()
	vim.cmd('noau normal! "vy"')
	local selection = vim.fn.getreg('v'):gsub("^%s*(.-)%s*$", "%1")
	vim.fn.setreg('v', {})

  require("telescope.builtin").live_grep({
    default_text=selection,
    search_dirs = get_search_dirs(),
  })
end


-- This is an empty function that exists to fulfill the custom plugin interface
function M.setup()
end

return M
