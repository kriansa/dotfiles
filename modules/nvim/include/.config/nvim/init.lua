-- Impatient is a lua plugin that speeds up loading Lua modules in Neovim to improve startup time.
-- It does so by basically compiling the lua modules to bytecode and then loading them from cache on
-- later runs. Because of that, it needs to be loaded very early in the initialization process
pcall(require, "impatient")

-- Load files on `lua`
require('settings')
require('bindings')
require('custom_plugins')

-- Only load plugin manager stuff when necessary -- otherwise use the precompiled cache
vim.cmd([[
  command! PackerInstall lua require('plugins').install()
  command! PackerUpdate lua require('plugins').update()
  command! PackerSync lua require('plugins').sync()
  command! PackerClean lua require('plugins').clean()
  command! PackerCompile lua require('plugins').compile()
]])
