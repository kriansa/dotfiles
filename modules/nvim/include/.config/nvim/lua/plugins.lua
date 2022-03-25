-- This is the lazy load plugin loader, and is only ever required when we make changes to either of
-- the installed plugins or add a new one. Otherwise this file is never loaded (see init.lua)

local packer = nil
local function init()
  if packer == nil then
    -- Bootstrap packer.nvim if it hasn't been installed yet
    local install_path = vim.fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
    if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
      packer_bootstrap = vim.fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
    end

    vim.cmd([[packadd packer.nvim]])
    packer = require('packer')
    packer.init({ disable_commands = true })
  end

  -- Make sure we unload all required plugins first
  require('plenary.reload').reload_module('plugins')

  packer.reset()
  local use = packer.use

  -- Require plugins
  require('plugins.editor-features')(use)
  require('plugins.file-finder')(use)
  require('plugins.file-manager')(use)
  require('plugins.filetypes')(use)
  require('plugins.editing')(use)
  require('plugins.completion')(use)
  require('plugins.git')(use)
  require('plugins.ui')(use)
end

-- Create virtually the same exports as require(`packer`): install, update, sync, clean & compile
local plugins = setmetatable({}, {
  __index = function(_, key)
    init()
    return packer[key]
  end,
})

return plugins
