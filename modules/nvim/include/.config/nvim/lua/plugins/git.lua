return function(use)
  use {
    'tpope/vim-fugitive', cmd = {
      'G', 'Git', 'Git!', 'Ggrep', 'Ggrep!', 'Glgrep', 'Glgrep!', 'Gclog', 'Gclog!', 'Gcd',
      'Glcd', 'Gedit', 'Gsplit', 'Gvsplit', 'Gtabedit', 'Gpedit', 'Gread', 'Gread!', 'Gwrite',
      'Gwq', 'Gwq!', 'Gdiffsplit', 'Gdiffsplit!', 'Gvdiffsplit', 'Ghdiffsplit', 'GMove',
      'GRename', 'GDelete', 'GRemove', 'GBrowse', 'GBrowse!',
    },
    config = function()
      -- Disable deprecated commands on fugitive
      vim.g.fugitive_legacy_commands = 0
    end,
    disable = true,
  }

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
    'TimUntersberger/neogit',
    requires = 'nvim-lua/plenary.nvim',
    cmd = 'Neogit'
  }
end
