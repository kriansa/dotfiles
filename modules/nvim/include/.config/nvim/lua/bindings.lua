local g = vim.g

local function nmap(lhs, rhs, opts)
  return vim.api.nvim_set_keymap('n', lhs, rhs, opts or {})
end
local function imap(lhs, rhs, opts)
  return vim.api.nvim_set_keymap('i', lhs, rhs, opts or {})
end
local function tmap(lhs, rhs, opts)
  return vim.api.nvim_set_keymap('t', lhs, rhs, opts or {})
end
local function vmap(lhs, rhs, opts)
  return vim.api.nvim_set_keymap('v', lhs, rhs, opts or {})
end
local function nnoremap(lhs, rhs, opts)
  return nmap(lhs, rhs, vim.tbl_extend("keep", opts or {}, { noremap = true }))
end
local function inoremap(lhs, rhs, opts)
  return imap(lhs, rhs, vim.tbl_extend("keep", opts or {}, { noremap = true }))
end
local function tnoremap(lhs, rhs, opts)
  return tmap(lhs, rhs, vim.tbl_extend("keep", opts or {}, { noremap = true }))
end
local function vnoremap(lhs, rhs, opts)
  return vmap(lhs, rhs, vim.tbl_extend("keep", opts or {}, { noremap = true }))
end
local function cnoremap(lhs, rhs, opts)
  return vim.api.nvim_set_keymap('c', lhs, rhs, vim.tbl_extend("keep", opts or {}, { noremap = true }))
end

-- Declare all global variables in Lua that are needed for other plugins to work
mappings = {}

-- Leader/local leader
g.mapleader = [[ ]]
g.maplocalleader = [[ ]]

-- Save a shift press for commands
nnoremap(';', ':')

-- Leader + o creates a blank line above
nmap('<Leader>o', 'o<Esc>')
nmap('<Leader>O', 'O<Esc>')

-- Select last yanked text
nnoremap('<Leader>v', '`[v`]')

-- Make n always search forward and N backward
nnoremap("n", "'Nn'[v:searchforward]", { expr = true })
nnoremap("N", "'nN'[v:searchforward]", { expr = true })

-- Makes Ctrl+/ clear the last search
nnoremap('<C-_>', '<cmd>let @/=""<CR>', { silent = true })

-- Closes current selected window with <Leader>q
nnoremap("<Leader>q", "<cmd>wincmd q<CR>", { silent = true })

-- Quit using Q
nmap('Q', '<cmd>wqa<CR>')

-- Stay in visual mode while indenting
vmap("<", "<gv")
vmap(">", ">gv")

-- Use CTRL-h,j,k,l to move between splits
nnoremap("<C-h>", "<cmd>wincmd h<CR>", { silent = true })
nnoremap("<C-j>", "<cmd>wincmd j<CR>", { silent = true })
nnoremap("<C-k>", "<cmd>wincmd k<CR>", { silent = true })
nnoremap("<C-l>", "<cmd>wincmd l<CR>", { silent = true })

-- Use CTRL-h,j,k to move up and down when in a terminal (keep Ctrl-L to clear terminal)
tnoremap("<C-h>", "<cmd>wincmd h<CR>", { silent = true })
tnoremap("<C-j>", "<cmd>wincmd j<CR>", { silent = true })
tnoremap("<C-k>", "<cmd>wincmd k<CR>", { silent = true })

-- Use ]t and [t for tab navigation
nnoremap("]t", "<cmd>tabnext<CR>", { silent = true })
nnoremap("[t", "<cmd>tabprevious<CR>", { silent = true })

-- Make all splits with equal size
nnoremap("<Leader>=", "<cmd>wincmd =<CR>", { silent = true })

-- Shift + J join lines. Shift + K should split lines
nnoremap("<S-K>", "i<CR><ESC>", { silent = true })

-- Exit insert mode from terminal
tnoremap('<ESC><ESC>', '<C-\\><C-n>')

-- Exit insert mode by mistyping C-[
inoremap("<C-]>", "<ESC>")

-- Command mode: M-Left/Right to jump words
cnoremap('<M-Left>', '<S-Left>')
cnoremap('<M-Right>', '<S-Right>')

--
-- Plugin-related bindings
--

-- Closes current buffer with <Leader>w
nnoremap("<Leader>w", "<cmd>Close<CR>", { silent = true })

-- Copy filepath + line to clipboard
nmap("<leader>yf", "<cmd>YankFilename<CR>", { silent = true })
nmap("<leader>yl", "<cmd>YankFilenameLine<CR>", { silent = true })

-- Quickfix
nnoremap(']q', '<cmd>QNext<CR>', { silent = true })
nnoremap('[q', '<cmd>QPrev<CR>', { silent = true })
nnoremap('<C-q>', '<cmd>QToggle<CR>', { silent = true })

-- Lazy.nvim (package manager)
nmap("<leader>ps", "<cmd>Lazy sync<CR>", { silent = true })
nmap("<leader>pr", "<cmd>Lazy restore<CR>", { silent = true })

-- Toggles zoom between the current buffer
nnoremap("<Leader>tt", "<cmd>ZoomWinTabToggle<CR>", { silent = true })

-- Winresizer starts with <Leader>+e
g.winresizer_start_key = '<Leader>e'

-- Git (Neogit AND fugitive)
nmap("<leader>gs", "<cmd>Neogit<CR>", { silent = true })
nmap("<leader>gS", "<cmd>tab Git<CR>", { silent = true })

-- Leap
vim.keymap.set({"n", "x", "o"}, "s", "<Plug>(leap-forward)", { silent = true, desc = "Leap forward to" })
vim.keymap.set({"n", "x", "o"}, "S", "<Plug>(leap-backward)", { silent = true, desc = "Leap backward to" })
vim.keymap.set({"x", "o"}, "z", "<Plug>(leap-forward-till)", { silent = true, desc = "Leap forward till" })
vim.keymap.set({"x", "o"}, "Z", "<Plug>(leap-backward-till)", { silent = true, desc = "Leap backward till" })

-- Nvim-tree
nmap("\\", "<cmd>NvimTreeFindFileToggle<CR>")
mappings.nvim_tree = function(bufnr)
  local api = require('nvim-tree.api')
  local default_size = 30

  local function current_treeview_size()
    local tree_id = api.tree.winid()
    return vim.fn.winwidth(tree_id)
  end

  local function opts(desc)
    return { desc = 'nvim-tree: ' .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
  end

  local function toggle_expand()
    local max = vim.o.columns
    api.tree.resize({ width = current_treeview_size() == default_size and max or default_size })
  end

  -- Open a file but resize the tree view to the default width if expanded
  local function open(open_function)
    return function(...)
      open_function(...)

      if current_treeview_size() ~= default_size then
        api.tree.resize({ width = default_size })
      end
    end
  end

  -- Opening files
  vim.keymap.set('n', '<CR>', open(api.node.open.edit), opts('Open'))
  vim.keymap.set('n', '<2-LeftMouse>', open(api.node.open.edit), opts('Open'))
  vim.keymap.set('n', '<Tab>', open(api.node.open.preview), opts('Open Preview'))
  vim.keymap.set('n', 'o', open(api.node.open.no_window_picker), opts('Open: No Window Picker'))
  vim.keymap.set('n', 'S', api.node.run.system, opts('Run System'))

  -- Navigation
  vim.keymap.set('n', 'X', api.tree.collapse_all, opts('Collapse'))
  vim.keymap.set('n', 'x', api.node.navigate.parent_close, opts('Close Directory'))
  vim.keymap.set('n', 'U', api.tree.change_root_to_parent, opts('CD to parent'))
  vim.keymap.set('n', 'C', api.tree.change_root_to_node, opts('CD'))

  -- Copy/paste operations
  vim.keymap.set('n', 'm', api.fs.cut, opts('Cut'))
  vim.keymap.set('n', 'c', api.fs.copy.node, opts('Copy'))
  vim.keymap.set('n', 'p', api.fs.paste, opts('Paste'))

  -- Basic
  vim.keymap.set('n', 'q', api.tree.close, opts('Close'))
  vim.keymap.set('n', '?', api.tree.toggle_help, opts('Help'))
  vim.keymap.set('n', 'R', api.tree.reload, opts('Refresh'))
  vim.keymap.set('n', 'A', toggle_expand, opts('Toggle tree view size'))
  vim.keymap.set('n', 'E', api.node.open.toggle_group_empty, opts('Toggle grouping empty directories'))

  -- Manipulation
  vim.keymap.set('n', 'a', api.fs.create, opts('Create'))
  vim.keymap.set('n', 'd', api.fs.trash, opts('Trash'))
  vim.keymap.set('n', 'r', api.fs.rename, opts('Rename'))
  vim.keymap.set('n', '.', api.node.run.cmd, opts('Run Command'))

  -- Other
  vim.keymap.set('n', '<C-v>', open(api.node.open.vertical), opts('Open: Vertical Split'))
  vim.keymap.set('n', '<C-s>', open(api.node.open.horizontal), opts('Open: Horizontal Split'))
  vim.keymap.set('n', 'y', api.fs.copy.filename, opts('Copy Name'))
  vim.keymap.set('n', 'Y', api.fs.copy.relative_path, opts('Copy Relative Path'))
  vim.keymap.set('n', 'gy', api.fs.copy.absolute_path, opts('Copy Absolute Path'))
  vim.keymap.set('n', 'M', api.marks.toggle, opts('Toggle mark'))
end

-- Mini diff
mappings.minidiff = {
  -- Apply hunks inside a visual/operator region
  apply = '<Leader>hs',

  -- Reset hunks inside a visual/operator region
  reset = '<Leader>hu',

  -- Hunk range textobject to be used inside operator
  -- Works also in Visual mode if mapping differs from apply and reset
  textobject = 'ac',

  -- Go to hunk range in corresponding direction
  goto_first = '[C',
  goto_prev = '[c',
  goto_next = ']c',
  goto_last = ']C',
}

-- Git blame
nmap('<Leader>gb', "<cmd>GitBlameToggle<CR>", {silent=true, desc="Toggle git blame"})

-- Oil.nvim
vim.keymap.set("n", "-", "<cmd>Oil<CR>", { desc = "Open parent directory" })
mappings.oil = {
  ["?"] = { "actions.show_help", mode = "n" },
  ["<CR>"] = "actions.select",
  ["<C-v>"] = { "actions.select", opts = { vertical = true } },
  ["<C-s>"] = { "actions.select", opts = { horizontal = true } },
  ["<C-t>"] = { "actions.select", opts = { tab = true } },
  ["gp"] = "actions.preview",
  ["gr"] = "actions.refresh",
  ["-"] = { "actions.parent", mode = "n" },
  ["_"] = { "actions.open_cwd", mode = "n" },
  ["gs"] = { "actions.change_sort", mode = "n" },
  ["gx"] = "actions.open_external",
  ["q"] = { "actions.close", mode = "n" },
}

-- Snacks
mappings.snacks_general = {
  -- Close the picker instead of going to normal mode
  ["<Esc>"] = { "close", mode = { "n", "i" } },

  ["<C-f>"] = { "toggle_follow", mode = { "i", "n" } },
  ["<C-h>"] = { "toggle_ignored", mode = { "i", "n" } },
  ["<C-r>"] = { "toggle_regex", mode = { "i", "n" } },
  ["<C-l>"] = { "toggle_live", mode = { "i", "n" } },
  ["<C-_>"] = { "toggle_help_input", mode = { "i", "n" } }, -- Equivalent to C-?
}

mappings.snacks_buffers = {
  ["<C-d>"] = { "delete_buffer", mode = { "i", "n" }, desc = "Delete buffer" },
  ["<Tab>"] = false,
  ["<S-Tab>"] = false,
}

-- File finder
nmap('<Leader><Leader>', '<cmd>lua require("custom_plugins.better-file-finder").find_buffers()<CR>')
nmap('<C-p>', '<cmd>lua require("custom_plugins.better-file-finder").find_files()<CR>')
nmap('<Leader>a', '<cmd>lua require("custom_plugins.better-file-finder").live_grep()<CR>')
nmap('<Leader>s', '<cmd>lua require("custom_plugins.better-file-finder").live_grep_current_word()<CR>')
vmap('<Leader>s', '<cmd>lua require("custom_plugins.better-file-finder").live_grep_current_selection()<CR>')
nmap('<C-s>', '<cmd>lua require("custom_plugins.better-file-finder").find_lines()<CR>')

-- Vim diagnostics
-- See `:help vim.lsp.*` for documentation on any of the below functions
-- ]d and [d navigates between diagnostic entries
vim.keymap.set('n', '<leader>dq', vim.diagnostic.setloclist)

mappings.lsp = function(bufnr)
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  local bufopts = { noremap=true, silent=true, buffer=bufnr }
  -- vim.keymap.set('n', 'gri', vim.lsp.buf.implementation, bufopts)
  -- vim.keymap.set('n', 'grr', vim.lsp.buf.references, bufopts)
  -- vim.keymap.set('n', 'grn', vim.lsp.buf.rename, bufopts)
  -- vim.keymap.set('n', 'gra', vim.lsp.buf.code_action, bufopts)
  vim.keymap.set('n', 'grD', vim.lsp.buf.declaration, bufopts)
  vim.keymap.set('n', 'grd', vim.lsp.buf.definition, bufopts)
  vim.keymap.set('n', 'gry', vim.lsp.buf.type_definition, bufopts)
  -- vim.keymap.set('i', '<C-S>', vim.lsp.buf.signature_help, bufopts)
  vim.keymap.set('n', '?', vim.lsp.buf.hover, bufopts)
end

-- Conform to format code (mnemonic: Linter Format)
vim.keymap.set({'n', 'v'}, '<leader>lf', function()
  require("conform").format({ async = true })
  vim.notify("Code formatted")
end)

-- Toggle linting (mnemonic: Linter Toggle)
vim.keymap.set({'n', 'v'}, '<leader>lt', function()
  vim.cmd("LintToggle")
end)

local ai_prefix = "<leader>u"

-- Toggle copilot (mnemonic: Co[p]ilot)
vim.keymap.set({'n', 'v'}, ai_prefix .. "p", "<cmd>CopilotToggle<CR>", { silent = true })

-- CodeCompanion Chat (mnemonic: [C]hat)
vim.keymap.set('n', ai_prefix .. "C", "<cmd>CodeCompanionChat<CR>", { silent = true })

-- CodeCompanion Inline (mnemonic: [c]ompanion)
vim.keymap.set({'n', 'v'}, ai_prefix .. "c", "<cmd>CodeCompanion<CR>", { silent = true })

mappings.code_companion = {
  accept = "ga",
  reject = "gr",
}
