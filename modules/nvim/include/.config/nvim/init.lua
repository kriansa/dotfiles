-- Enables the experimental Lua module loader
vim.loader.enable()

-- Load files on `lua` folder
require('settings')
require('clipboard')
require('bindings')
require('custom_plugins')

-- Load plugin manager (lazy.nvim)
require('plugins')
