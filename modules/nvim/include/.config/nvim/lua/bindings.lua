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

-- Leader/local leader
g.mapleader = [[ ]]
g.maplocalleader = [[ ]]

-- Declare all global variables in Lua that are needed for other plugins to work
mappings = {}

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

-- Make all splits with equal size
nnoremap("<Leader>=", ":wincmd =<CR>", { silent = true })

-- Shift + J join lines. Shift + K should split lines
nnoremap("<S-K>", "i<CR><ESC>", { silent = true })

--
-- Plugin-related bindings
--

-- Use CTRL-S to save
nnoremap('<C-S>', '<cmd>WriteBuffer<CR>')

-- Closes current buffer with <Leader>w
nnoremap("<Leader>w", "<cmd>Close<CR>", { silent = true })

-- Copy filepath + line to clipboard
nmap("<leader>yf", "<cmd>YankFilename<CR>", { silent = true })
nmap("<leader>yl", "<cmd>YankFilenameLine<CR>", { silent = true })

-- Quickfix
nnoremap(']q', '<cmd>QNext<CR>', { silent = true })
nnoremap('[q', '<cmd>QPrev<CR>', { silent = true })
nnoremap('<C-q>', '<cmd>QFToggle<CR>', { silent = true })

-- Packer
nmap("<leader>pc", "<cmd>PackerCompile<CR>", { silent = true })
nmap("<leader>ps", "<cmd>PackerSync<CR>", { silent = true })

-- Toggles zoom between the current buffer
nnoremap("<Leader>tt", "<cmd>ZoomWinTabToggle<CR>", { silent = true })

-- Winresizer starts with <Leader>+e
g.winresizer_start_key = '<Leader>e'

-- Neogit
nmap("<leader>gs", "<cmd>Neogit<CR>", { silent = true })

-- Fugitive
-- nmap("<leader>gs", ":Git<CR>", { silent = true })
-- nmap("<leader>gp", ":Git push<CR>", { silent = true })
-- nmap("<leader>gP", ":Git push<CR>", { silent = true })
-- nmap("<leader>gb", ":Git blame<CR>", { silent = true })

-- Dirvish
nmap('-', "<Plug>(dirvish_up)")

-- Nvim-tree
nmap("\\", "<cmd>NvimTreeFindFileToggle<CR>")
mappings.nvim_tree = {
  -- Opening files
  { key = {"<CR>", "o", "<2-LeftMouse>"}, action = "edit" },
  { key = "<Tab>",                        action = "preview" },
  { key = "O",                            action = "edit_no_picker" },
  { key = "C",                            action = "cd" },
  { key = "S",                            action = "system_open" },

  -- Navigation
  { key = "X",                            action = "collapse_all" },
  { key = "x",                            action = "close_node" },

  -- Copy/paste operations
  { key = "m",                            action = "cut" },
  { key = "c",                            action = "copy" },
  { key = "p",                            action = "paste" },

  -- Basic
  { key = "q",                            action = "close" },
  { key = "g?",                           action = "toggle_help" },
  { key = "R",                            action = "refresh" },
  {
    key = "A",
    action = "toggle_size",
    action_cb = function()
      local default = 30
      local max = vim.o.columns
      local current = require('nvim-tree.view').View.width

      require('nvim-tree').resize(current == default and max or default)
    end
  },

  -- Manipulation
  { key = "a",                            action = "create" },
  { key = "d",                            action = "trash" },
  { key = "r",                            action = "rename" },
  { key = ".",                            action = "run_file_command" },

  -- Other
  { key = "<C-v>",                        action = "vsplit" },
  { key = "<C-s>",                        action = "split" },
  { key = "K",                            action = "parent_node" },
  { key = "I",                            action = "toggle_ignored" },
  { key = "y",                            action = "copy_name" },
  { key = "Y",                            action = "copy_path" },
  { key = "gy",                           action = "copy_absolute_path" },
}

-- Gitsigns
mappings.gitsigns = function(bufnr)
  -- Replace this mapping Neovim 0.7 gets released
  -- See: https://github.com/lewis6991/gitsigns.nvim
  local map = function(mode, lhs, rhs, opts)
    opts = vim.tbl_extend('force', {noremap = true, silent = true}, opts or {})
    vim.api.nvim_buf_set_keymap(bufnr, mode, lhs, rhs, opts)
  end

  -- Navigation
  map('n', ']c', "&diff ? ']c' : '<cmd>Gitsigns next_hunk<CR>'", {expr=true})
  map('n', '[c', "&diff ? '[c' : '<cmd>Gitsigns prev_hunk<CR>'", {expr=true})

  -- Actions
  map('n', '<leader>hs', ':Gitsigns stage_hunk<CR>')
  map('v', '<leader>hs', ':Gitsigns stage_hunk<CR>')
  map('n', '<leader>hu', ':Gitsigns reset_hunk<CR>')
  map('v', '<leader>hu', ':Gitsigns reset_hunk<CR>')
  map('n', '<leader>gb', '<cmd>lua require("gitsigns").blame_line{full=true}<CR>')

  -- This has very similar behavior as `gb` above
  map('n', '<leader>hp', '<cmd>Gitsigns preview_hunk<CR>')

  map('n', '<leader>gd', '<cmd>Gitsigns diffthis<CR>')
  map('n', '<leader>gD', '<cmd>lua require("gitsigns").diffthis("~")<CR>')

  -- Not very useful for my liking
  -- map('n', '<leader>gb', '<cmd>Gitsigns toggle_current_line_blame<CR>')

  -- Text object
  map('o', 'ih', ':<C-U>Gitsigns select_hunk<CR>')
  map('x', 'ih', ':<C-U>Gitsigns select_hunk<CR>')
end

-- Telescope
function mappings.telescope_defaults()
  local actions = require("telescope.actions")
  local transform_mod = require('telescope.actions.mt').transform_mod
  local custom_actions = transform_mod({ open_qflist = function()
    vim.cmd("QFOpen")
  end})

  return {
    -- History
    ["<Down>"] = "cycle_history_next",
    ["<Up>"] = "cycle_history_prev",

    -- Movement
    ["<C-j>"] = "move_selection_next",
    ["<C-k>"] = "move_selection_previous",
    ["<C-n>"] = "move_selection_next",
    ["<C-p>"] = "move_selection_previous",

    -- Opening
    ["<CR>"] = "select_default",
    ["<C-v>"] = "select_vertical",
    ["<C-s>"] = "select_horizontal",

    -- Scroll preview
    ["<C-u>"] = "preview_scrolling_up",
    ["<C-d>"] = "preview_scrolling_down",

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
nmap('<Leader><Space>', '<cmd>Telescope buffers<CR>')
nmap('<C-P>', '<cmd>Telescope find_files<CR>')
nmap('<Leader>a', '<cmd>Telescope live_grep<CR>')
nmap('<Leader>s', '<cmd>Telescope grep_string<CR>')

-- ALE
-- nnoremap <silent> gd :ALEGoToDefinition<CR>
-- nmap <silent> [l <Plug>(ale_previous_wrap)
-- nmap <silent> ]l <Plug>(ale_next_wrap)
-- nmap <Leader>ff <Plug>(ale_fix)
--
-- CoC
-- nmap <silent> gd <Plug>(CoC-definition)
-- nmap <silent> gy <Plug>(coc-type-definition)
-- nmap <silent> gi <Plug>(coc-implementation)
-- nmap <silent> gr <Plug>(coc-references)
-- nmap <leader>fc <Plug>(coc-fix-current)
-- nmap <leader>ac <Plug>(coc-codeaction)
