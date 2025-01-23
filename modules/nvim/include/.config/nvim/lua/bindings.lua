local g = vim.g

local function set_keymap(mode, lhs, rhs, opts)
  return vim.api.nvim_set_keymap(mode, lhs, rhs, opts or {})
end
local function nmap(lhs, rhs, opts)
  return set_keymap('n', lhs, rhs, opts)
end
local function imap(lhs, rhs, opts)
  return set_keymap('i', lhs, rhs, opts)
end
local function tmap(lhs, rhs, opts)
  return set_keymap('t', lhs, rhs, opts)
end
local function vmap(lhs, rhs, opts)
  return set_keymap('v', lhs, rhs, opts)
end
local function nnoremap(lhs, rhs, opts)
  silent = (opts or {}).silent
  expr = (opts or {}).expr
  return nmap(lhs, rhs, { noremap = true, silent = silent, expr = expr })
end
local function inoremap(lhs, rhs, opts)
  return imap(lhs, rhs, { noremap = true })
end
local function tnoremap(lhs, rhs, opts)
  return tmap(lhs, rhs, { noremap = true })
end
local function vnoremap(lhs, rhs, opts)
  return vmap(lhs, rhs, { noremap = true })
end

-- Declare all global variables in Lua that are needed for other plugins to work
mappings = {}

-- Leader/local leader
g.mapleader = [[ ]]
g.maplocalleader = [[ ]]

-- Save a shift press for commands
nnoremap(';', ':')

-- Save a shift press for commands
nmap('j', 'gj')
nmap('k', 'gk')

-- Leader + o creates a blank line above
nmap('<Leader>o', 'o<Esc>')
nmap('<Leader>O', 'O<Esc>')

-- Select last yanked text
nnoremap('<Leader>v', '`[v`]')

-- Make n always search forward and N backward
nnoremap("n", "'Nn'[v:searchforward]", { expr = true })
nnoremap("N", "'nN'[v:searchforward]", { expr = true })

-- Makes Ctrl+/ clear the last search
nnoremap('<C-_>', ':let @/=""<CR>', { silent = true })

-- Closes current selected window with <Leader>q
nnoremap("<Leader>q", ":wincmd q<CR>", { silent = true })

-- Quit using Q
nmap('Q', ':wqa<CR>')

-- Stay in visual mode while indenting
vmap("<", "<gv")
vmap(">", ">gv")

-- Use CTRL-h,j,k,l to move between splits
nnoremap("<C-h>", ":wincmd h<CR>", { silent = true })
nnoremap("<C-j>", ":wincmd j<CR>", { silent = true })
nnoremap("<C-k>", ":wincmd k<CR>", { silent = true })
nnoremap("<C-l>", ":wincmd l<CR>", { silent = true })

-- Use ]t and [t for tab navigation
nnoremap("]t", ":tabnext<CR>", { silent = true })
nnoremap("[t", ":tabprevious<CR>", { silent = true })

-- Make all splits with equal size
nnoremap("<Leader>=", ":wincmd =<CR>", { silent = true })

-- Shift + J join lines. Shift + K should split lines
nnoremap("<S-K>", "i<CR><ESC>", { silent = true })

-- Exit insert mode from terminal
tnoremap('<ESC><ESC>', '<C-\\><C-n>')

-- Exit insert mode by mistyping C-[
inoremap("<C-]>", "<ESC>")

--
-- Plugin-related bindings
--

-- Use CTRL-S to save
nnoremap('<C-S>', '<cmd>write<CR>')

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
nnoremap("<Leader>tz", "<cmd>ZenMode<CR>", { silent = true })

-- Winresizer starts with <Leader>+e
g.winresizer_start_key = '<Leader>e'

-- Git (Neogit AND fugitive)
nmap("<leader>gs", "<cmd>Neogit<CR>", { silent = true })
nmap("<leader>gS", "<cmd>tab Git<CR>", { silent = true })

-- Github Copilot
mappings.gh_copilot = {
  suggestion = {
    accept = "<Tab>",
    accept_word = false,
    accept_line = false,
    next = "<C-Space>",
    prev = false,
    dismiss = "<C-]>",
  },
}

-- Leap
vim.keymap.set({"n", "x", "o"}, "s", "<Plug>(leap-forward-to)", { silent = true, desc = "Leap forward to" })
vim.keymap.set({"n", "x", "o"}, "S", "<Plug>(leap-backward-to)", { silent = true, desc = "Leap backward to" })
vim.keymap.set({"x", "o"}, "z", "<Plug>(leap-forward-till)", { silent = true, desc = "Leap forward till" })
vim.keymap.set({"x", "o"}, "Z", "<Plug>(leap-backward-till)", { silent = true, desc = "Leap backward till" })

-- Nvim-tree
nmap("\\", "<cmd>NvimTreeFindFileToggle<CR>")
mappings.nvim_tree = function(bufnr)
  local api = require('nvim-tree.api')

  local function opts(desc)
    return { desc = 'nvim-tree: ' .. desc, buffer = bufnr, noremap = true, silent = true, nowait = true }
  end

  local function toggle_expand()
    local default = 30
    local max = vim.o.columns
    local current = require('nvim-tree.view').View.width

    require('nvim-tree.view').resize(current == default and max or default)
  end

  -- Open a file but resize the tree view to the default width if expanded
  local function open(open_function)
    return function(...)
      open_function(...)

      local default = 30
      local current = require('nvim-tree.view').View.width

      if current ~= default then
        require('nvim-tree.view').resize(default)
      end
    end
  end

  -- Opening files
  vim.keymap.set('n', '<CR>', open(api.node.open.edit), opts('Open'))
  vim.keymap.set('n', 'O', open(api.node.open.edit), opts('Open'))
  vim.keymap.set('n', '<2-LeftMouse>', open(api.node.open.edit), opts('Open'))
  vim.keymap.set('n', '<Tab>', open(api.node.open.preview), opts('Open Preview'))
  vim.keymap.set('n', 'o', open(api.node.open.no_window_picker), opts('Open: No Window Picker'))
  vim.keymap.set('n', 'U', api.tree.change_root_to_parent, opts('CD to parent'))
  vim.keymap.set('n', 'C', api.tree.change_root_to_node, opts('CD'))
  vim.keymap.set('n', 'S', api.node.run.system, opts('Run System'))

  -- Navigation
  vim.keymap.set('n', 'X', api.tree.collapse_all, opts('Collapse'))
  vim.keymap.set('n', 'x', api.node.navigate.parent_close, opts('Close Directory'))

  -- Copy/paste operations
  vim.keymap.set('n', 'm', api.fs.cut, opts('Cut'))
  vim.keymap.set('n', 'c', api.fs.copy.node, opts('Copy'))
  vim.keymap.set('n', 'p', api.fs.paste, opts('Paste'))

  -- Basic
  vim.keymap.set('n', 'q', api.tree.close, opts('Close'))
  vim.keymap.set('n', 'g?', api.tree.toggle_help, opts('Help'))
  vim.keymap.set('n', '?', api.tree.toggle_help, opts('Help'))
  vim.keymap.set('n', 'R', api.tree.reload, opts('Refresh'))
  vim.keymap.set('n', 'A', toggle_expand, opts('Toggle tree view size'))

  -- Manipulation
  vim.keymap.set('n', 'a', api.fs.create, opts('Create'))
  vim.keymap.set('n', 'd', api.fs.trash, opts('Trash'))
  vim.keymap.set('n', 'r', api.fs.rename, opts('Rename'))
  vim.keymap.set('n', '.', api.node.run.cmd, opts('Run Command'))

  -- Other
  vim.keymap.set('n', '<C-v>', open(api.node.open.vertical), opts('Open: Vertical Split'))
  vim.keymap.set('n', '<C-s>', open(api.node.open.horizontal), opts('Open: Horizontal Split'))
  vim.keymap.set('n', 'K', api.node.navigate.parent, opts('Parent Directory'))
  vim.keymap.set('n', 'I', api.tree.toggle_gitignore_filter, opts('Toggle Git Ignore'))
  vim.keymap.set('n', 'y', api.fs.copy.filename, opts('Copy Name'))
  vim.keymap.set('n', 'Y', api.fs.copy.relative_path, opts('Copy Relative Path'))
  vim.keymap.set('n', 'gy', api.fs.copy.absolute_path, opts('Copy Absolute Path'))
  vim.keymap.set('n', 'M', api.marks.toggle, opts('Toggle mark'))
end

-- Cmp (autocompletion)
mappings.cmp_insert = function()
  local cmp = package.loaded.cmp
  return cmp.mapping.preset.insert({
    ['<CR>'] = cmp.mapping.confirm({ select = false }),
  })
end

mappings.cmp_cmdline = function()
  local cmp = package.loaded.cmp
  return cmp.mapping.preset.cmdline({
    ['<CR>'] = cmp.mapping.confirm({ select = false }),
  })
end

-- Gitgutter
mappings.gitgutter = function()
  local function map(mode, l, r, opts)
    vim.keymap.set(mode, l, r, opts or {})
  end

  -- Navigation
  map('n', '[c', "<Plug>(GitGutterPrevHunk)", {silent=true, desc="Previous diff"})
  map('n', ']c', "<Plug>(GitGutterNextHunk)", {silent=true, desc="Next diff"})

  -- Text objects
  map({'o', 'x'}, 'ic', "<Plug>(GitGutterTextObjectInnerPending)", {noremap=true, silent=true, desc="Inner diff"})
  map({'o', 'x'}, 'ac', "<Plug>(GitGutterTextObjectOuterPending)", {noremap=true, silent=true, desc="Around diff"})

  -- Operations
  map('n', '<Leader>hs', "<Plug>(GitGutterStageHunk)", {silent=true, desc="Stage hunk"})
  map('n', '<Leader>hu', "<Plug>(GitGutterUndoHunk)", {silent=true, desc="Undo hunk"})
  map('n', '<Leader>hp', "<Plug>(GitGutterPreviewHunk)", {silent=true, desc="Preview hunk"})
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

mappings.git_blame = function()
  local function map(mode, l, r, opts)
    vim.keymap.set(mode, l, r, opts or {})
  end

  -- Navigation
  map('n', '<Leader>gb', "<cmd>GitBlameToggle<CR>", {silent=true, desc="Toggle git blame"})
end

-- Gitsigns
mappings.gitsigns = function(bufnr)
  local gs = package.loaded.gitsigns

  local function map(mode, l, r, opts)
    opts = opts or {}
    opts.buffer = bufnr
    vim.keymap.set(mode, l, r, opts)
  end

  -- Navigation
  map('n', ']c', function()
    if vim.wo.diff then return ']c' end
    vim.schedule(function() gs.next_hunk() end)
    return '<Ignore>'
  end, {expr=true})

  map('n', '[c', function()
    if vim.wo.diff then return '[c' end
    vim.schedule(function() gs.prev_hunk() end)
    return '<Ignore>'
  end, {expr=true})

  -- Actions
  map('n', '<leader>hs', gs.stage_hunk)
  map('n', '<leader>hu', gs.reset_hunk)
  map('v', '<leader>hs', function() gs.stage_hunk {vim.fn.line('.'), vim.fn.line('v')} end)
  map('v', '<leader>hu', function() gs.reset_hunk {vim.fn.line('.'), vim.fn.line('v')} end)
  map('n', '<leader>hp', gs.preview_hunk)

  -- This has very similar behavior as `hp` above
  map('n', '<leader>gb', gs.toggle_current_line_blame)
  map('n', '<leader>gB', function() gs.blame_line{full=true} end)

  map('n', '<leader>gd', gs.diffthis)
  map('n', '<leader>gD', function() gs.diffthis('~') end)

  -- Text object
  map({'o', 'x'}, 'ih', ':<C-U>Gitsigns select_hunk<CR>')
end

-- Telescope
mappings.telescope_defaults = function()
  local actions = require("telescope.actions")
  local actions_set = require("telescope.actions.set")
  local transform_mod = require('telescope.actions.mt').transform_mod
  local custom_actions = transform_mod({
    open_qflist = function()
      vim.cmd("QFOpen")
    end,

    move_multiple_selection_next = function(prompt_bufnr)
      actions_set.shift_selection(prompt_bufnr, 5)
    end,

    move_multiple_selection_previous = function(prompt_bufnr)
      actions_set.shift_selection(prompt_bufnr, -5)
    end,
  })

  return {
    -- History
    ["<Down>"] = "cycle_history_next",
    ["<Up>"] = "cycle_history_prev",

    -- Movement
    ["<C-j>"] = "move_selection_next",
    ["<C-k>"] = "move_selection_previous",
    ["<C-n>"] = "move_selection_next",
    ["<C-p>"] = "move_selection_previous",
    ["<C-d>"] = custom_actions.move_multiple_selection_next,
    ["<C-u>"] = custom_actions.move_multiple_selection_previous,

    -- Opening
    ["<CR>"] = "select_default",
    ["<C-v>"] = "select_vertical",
    ["<C-s>"] = "select_horizontal",

    -- Basic
    ["<Tab>"] = actions.toggle_selection + actions.move_selection_worse,
    ["<S-Tab>"] = actions.toggle_selection + actions.move_selection_better,
    ["<ESC>"] = "close",
    ["<C-c>"] = "close",
    ["<C-_>"] = "which_key", -- equivalent to C-/
    ["<C-w>"] = { "<c-s-w>", type = "command" },
    ["<C-q>"] = actions.smart_send_to_qflist + custom_actions.open_qflist,
  }
end

mappings.telescope_buffers = {
  ["<C-d>"] = "delete_buffer",
}

-- Telescope
nmap('<Leader><Leader>', '<cmd>lua require("custom_plugins.better-file-finder").find_buffers()<CR>')
nmap('<C-P>', '<cmd>lua require("custom_plugins.better-file-finder").find_files()<CR>')
nmap('<Leader>a', '<cmd>lua require("custom_plugins.better-file-finder").live_grep()<CR>')
nmap('<Leader>s', '<cmd>lua require("custom_plugins.better-file-finder").live_grep_current_word()<CR>')
vmap('<Leader>s', '<cmd>lua require("custom_plugins.better-file-finder").live_grep_current_selection()<CR>')

-- Vim diagnostics
-- See `:help vim.lsp.*` for documentation on any of the below functions
-- ]d and [d navigates between diagnostic entries
vim.keymap.set('n', '<leader>dq', vim.diagnostic.setloclist)

mappings.lsp = function(bufnr)
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  local bufopts = { noremap=true, silent=true, buffer=bufnr }
  vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
  vim.keymap.set('n', 'gy', vim.lsp.buf.type_definition, bufopts)
  vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
  vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
  vim.keymap.set('n', '?', vim.lsp.buf.hover, bufopts)
  vim.keymap.set('n', '<leader>ls', vim.lsp.buf.signature_help, bufopts)
  vim.keymap.set('n', '<leader>lwa', vim.lsp.buf.add_workspace_folder, bufopts)
  vim.keymap.set('n', '<leader>lwr', vim.lsp.buf.remove_workspace_folder, bufopts)
  vim.keymap.set('n', '<leader>lwl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, bufopts)
  vim.keymap.set('n', '<leader>lr', vim.lsp.buf.rename, bufopts)
  vim.keymap.set('n', '<leader>la', vim.lsp.buf.code_action, bufopts)
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

-- Toggle copilot (mnemonic: Copilot Toggle)
vim.keymap.set({'n', 'v'}, '<leader>ct', function()
  vim.cmd("Copilot toggle")
end)

-- CodeCompanion (mnemonic: Code Companion)
vim.keymap.set({'n', 'v'}, '<leader>cc', function()
  vim.cmd("CodeCompanionChat")
end)

-- Aider
mappings.aider = {
  toggle = "<C-Space>"
}

-- Snippy is a snippet manager
mappings.snippy = {
  is = {
    ['<Tab>'] = 'expand_or_advance',
    ['<S-Tab>'] = 'previous',
  },
}
