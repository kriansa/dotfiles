local M = {}

function get_search_dirs()
  local cwd = vim.fn.getcwd()

  -- `dirs` is a key-valued table and we add values as keys so they aren't duplicated
  local dirs = {}
  for _, node in ipairs(require("nvim-tree.api").marks.list() or {}) do
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

function M.find_lines()
  Snacks.picker.lines()
end

function M.find_buffers()
  Snacks.picker.buffers()
end

function M.find_files()
  Snacks.picker.files({ dirs = get_search_dirs() })
end

function M.live_grep()
  Snacks.picker.grep({ dirs = get_search_dirs() })
end

function M.live_grep_current_word()
  Snacks.picker.grep({ dirs = get_search_dirs(), search = vim.fn.expand("<cword>"), regex = false })
end

function M.live_grep_current_selection()
  vim.cmd('noau silent! normal! "vy"')
  local selection = vim.fn.getreg('v'):gsub("^%s*(.-)%s*$", "%1")
  vim.fn.setreg('v', {})

  -- If the selection contains multiple lines, we only take the first line
  selection = selection:match("([^\n]+)")

  Snacks.picker.grep({ dirs = get_search_dirs(), search = selection, regex = false })
end


-- This is an empty function that exists to fulfill the custom plugin interface
function M.setup()
end

return M
