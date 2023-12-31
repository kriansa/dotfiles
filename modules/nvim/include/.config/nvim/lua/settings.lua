local g = vim.g
local o = vim.opt
local cmd = vim.cmd

-- Skip all remote provider loading
g.loaded_python_provider = 0
g.loaded_python3_provider = 0
g.loaded_ruby_provider = 0
g.loaded_perl_provider = 0
g.loaded_node_provider = 0

-- Disable some built-in plugins we don't want
g.loaded_man = 1
g.loaded_matchit = 1
g.loaded_matchparen = 1
g.loaded_shada_plugin = 1
g.loaded_2html_plugin = 1
g.loaded_getscript = 1
g.loaded_getscriptPlugin = 1
g.loaded_gzip = 1
g.loaded_logipat = 1
g.loaded_netrw = 1
g.loaded_netrwFileHandlers = 1
g.loaded_netrwPlugin = 1
g.loaded_netrwSettngs = 1
g.loaded_remote_plugins = 1
g.loaded_tar = 1
g.loaded_tarPlugin = 1
g.loaded_zip = 1
g.loaded_zipPlugin = 1
g.loaded_vimball = 1
g.loaded_vimballPlugin = 1
g.zipPlugin = 1

-- Auto resize splits
vim.api.nvim_create_autocmd("VimResized", {
  pattern = "*",
  command = ":wincmd =",
})

-- Highlight when yanking
vim.api.nvim_create_autocmd("TextYankPost", {
  pattern = "*",
  callback = function()
    vim.highlight.on_yank({ timeout = 80 })
  end
})

-- This effectively replaces the `set clipboard=unnamedplus` option, which sets every yanked text to
-- the system clipboard. The problem with that is that when doing several deletes or changes in a
-- row, the clipboard will keep getting overwritten, as well as calling the system clipboard app
-- every time. On GNOME, this is particularly slow, blocks the UI and steals the focus, causing
-- events such as `FocusLost` and `FocusGained` to be triggered, which is not what we want.
--
-- This way, it will only copy to the system clipboard when the editor loses focus, which is
-- usually when you switch to another application. We also set a timer so we avoid conflicts while
-- alt-tabbing, especially on GNOME where the wl-clipboard steals the focus, causing an infinite
-- loop.
--
-- See: https://github.com/neovim/neovim/issues/11804
-- See: https://github.com/neovim/neovim/issues/24470
-- See: https://github.com/bugaevc/wl-clipboard/issues/90
function with_locked_clipboard(fn)
  return function()
    -- Only sets the register if the timer is not running
    if vim.g._clipboard_lock then
      return
    end

    fn()
    vim.g._clipboard_lock = true

    -- Starts the timer
    vim.defer_fn(function()
      vim.g._clipboard_lock = false
    end, 500)
  end
end

vim.api.nvim_create_autocmd("FocusLost", {
  pattern = "*",
  callback = with_locked_clipboard(function()
    vim.fn.setreg("+", vim.fn.getreg(0))
  end)
})

vim.api.nvim_create_autocmd("FocusGained", {
  pattern = "*",
  callback = with_locked_clipboard(function()
    vim.fn.setreg(0, vim.fn.getreg("+"))
  end)
})

-- Add existing filetypes
vim.filetype.add({
  extension = {
    jbuilder = "ruby",
  },
  filename = {
    Dangerfile = "ruby",
    [".env"] = "sh",
    [".nycrc"] = "javascript",
  },
  pattern = {
    ["^Dockerfile[-.].*"] = "dockerfile",
  },
})

-- Settings
cmd("lang en_US.UTF-8")         -- Fix encoding issues on MacOS
o.encoding = "utf-8"            -- Enables utf8 encoding
o.backspace = "indent,eol"      -- Enables backspace on indentation and end of lines
o.hidden = true                 -- This allows buffers to be hidden if you've modified a buffer.
o.number = true                 -- Display line numbers
o.wrap = false                  -- Disable word-wrap
o.wildmenu = true               -- Helps command-line completion menu
o.cursorline = true             -- Highlight the line the cursor is in
o.showmode = false              -- Disable showing the mode (such as -- INSERT --) in the bottom
o.swapfile = false              -- Never create swap files
o.backup = false                -- Disable usage of backup files (~)
o.writebackup = false           -- Disable creation of backup files
o.mouse = "nvi"                 -- Enable the mouse
o.laststatus = 2                -- Always enable bottom status line
o.hlsearch = true               -- Enable search highlight
o.autoread = true               -- Enable auto-read of files edited outside vim
o.synmaxcol = 300               -- Limit syntax highlighting for long lines
o.colorcolumn = "+1"            -- Set a width to show a column after texwidth
o.foldenable = false            -- Disable folding
-- o.lazyredraw = true             -- Do not redraw screen in the middle of a macro.
o.signcolumn = "yes"            -- Always show the sign column
o.list = true                        -- Show hidden chars
o.listchars = "tab:▸ ,eol:¬,space:·,trail:+" -- chars to be displayed
o.showbreak = "↪"                    -- char to be displayed on wraped lines
o.termguicolors = true               -- Use truebit on terminal
o.updatetime = 250                   -- Time to trigger CursorHold

-- Spend extra time to generate the smallest possible diff
o.diffopt:append("algorithm:patience")
o.diffopt:append("linematch:60")

-- Indentation settings
o.expandtab = true          -- Convert tabs into spaces
o.autoindent = true         -- always set autoindenting on
o.copyindent = true         -- copy indentation on new lines
o.smartindent = true        -- indent on new blocks
o.preserveindent = true     -- When reindenting a line, tries to preserve the indent-style
o.shiftwidth = 2            -- Number of spaces to use for autoindenting
o.tabstop = 2               -- Size of the tabs by default
o.smarttab = true           -- Insert tabs on the start of a line according to shiftwidth, not tabstop
o.shiftround = true         -- Use multiple of shiftwidth when indenting with '<' and '>'
o.relativenumber = true     -- Show relative numbers

o.complete:append("kspell") -- Use spell completion when spell check is enabled

-- Search
o.ignorecase = true     -- Make search case insensitive
o.smartcase = true      -- When searching with a uppercase letter, enable case-sensitive
o.inccommand = "split"
o.redrawtime = 300

-- Open new split panes to right and bottom, which feels more natural
o.splitbelow = true
o.splitright = true

-- Disable vim autocompletion for these files below
o.wildignore = "node_modules,vendor/bundle,.git,.DS_Store"

-- Consider dashes as keywords so we can use autocompletion
o.iskeyword:append("-")

-- Set autoformatting options
-- See help in :h fo-table
-- o.formatoptions:remove("tcq")

-- Ctrl-a and Ctrl-x will ignore the sign of the number under the cursor
o.nrformats:append("unsigned")

-- Use rg over grep
o.grepprg = "rg --no-heading -M 120 --color=never --hidden --ignore-file=$DOTFILES_PATH/.rgignore"
