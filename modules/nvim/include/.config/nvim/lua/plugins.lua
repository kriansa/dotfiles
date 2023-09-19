-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Now load our plugins in a single table
local plugin_list = {
  'editor-features',
  'file-finder',
  'file-manager',
  'editing',
  'completion',
  'git',
  'ui',
}

local plugins_table = {}
for _, plugin_name in pairs(plugin_list) do
  loaded_plugin = require("plugins." .. plugin_name)
  for _, v in pairs(loaded_plugin) do table.insert(plugins_table, v) end
end

-- Declare lazy.nvim options
-- See: https://github.com/folke/lazy.nvim/tree/main#%EF%B8%8F-configuration
local opts = {}

require("lazy").setup(plugins_table, opts)
