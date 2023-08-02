return function(use)
  use {
    'lewis6991/gitsigns.nvim',
    requires = { 'nvim-lua/plenary.nvim' },
    config = function()
      require('gitsigns').setup({
        signs = {
          -- add = { hl = 'GreenSign', text = '│', numhl = 'GitSignsAddNr' },
          -- change = { hl = 'BlueSign', text = '│', numhl = 'GitSignsChangeNr' },
          -- delete = { hl = 'RedSign', text = '│', numhl = 'GitSignsDeleteNr' },
          -- topdelete = { hl = 'RedSign', text = '│', numhl = 'GitSignsDeleteNr' },
          -- changedelete = { hl = 'PurpleSign', text = '│', numhl = 'GitSignsChangeNr' },
          add = { hl = 'GreenSign', text = '+', numhl = 'GitSignsAddNr' },
          change = { hl = 'BlueSign', text = '~', numhl = 'GitSignsChangeNr' },
          delete = { hl = 'RedSign', text = '_', numhl = 'GitSignsDeleteNr' },
          topdelete = { hl = 'RedSign', text = '‾', numhl = 'GitSignsDeleteNr' },
          changedelete = { hl = 'RedSign', text = '~', numhl = 'GitSignsChangeNr' },
        },
        on_attach = mappings.gitsigns,
      })
    end
  }

  use {
    'NeogitOrg/neogit',
    requires = 'nvim-lua/plenary.nvim',
    cmd = 'Neogit',
    config = function()
      local neogit = require('neogit')
      neogit.setup({})
    end,
  }
end
