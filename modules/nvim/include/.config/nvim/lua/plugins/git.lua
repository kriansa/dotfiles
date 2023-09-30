return {
  {
    'lewis6991/gitsigns.nvim',
    dependencies = { 'nvim-lua/plenary.nvim' },
    config = function()
      require('gitsigns').setup({
        signs = {
          add = { hl = 'GreenSign', text = '+', numhl = 'GitSignsAddNr' },
          change = { hl = 'BlueSign', text = '~', numhl = 'GitSignsChangeNr' },
          delete = { hl = 'RedSign', text = '_', numhl = 'GitSignsDeleteNr' },
          topdelete = { hl = 'RedSign', text = '‾', numhl = 'GitSignsDeleteNr' },
          changedelete = { hl = 'RedSign', text = '~', numhl = 'GitSignsChangeNr' },
          untracked = { hl = 'GreenSign', text = '┆', numhl = 'GitSignsAddNr' },
        },
        on_attach = mappings.gitsigns,
        current_line_blame_opts = {
          virt_text = true,
          virt_text_pos = 'right_align', -- 'eol' | 'overlay' | 'right_align'
          delay = 200,
          ignore_whitespace = false,
        },
      })
    end
  },

  {
    "sindrets/diffview.nvim",
    config = function()
      require("diffview").setup({
        use_icons = false,
      })
    end,
  },

  {
    "tpope/vim-fugitive",
    cmd = { "G", "Git" },
  },

  {
    "NeogitOrg/neogit",
    dependencies = { "nvim-lua/plenary.nvim", "sindrets/diffview.nvim" },
    cmd = "Neogit",
    config = function()
      local neogit = require("neogit")
      neogit.setup({
        auto_refresh = false,
        disable_commit_confirmation = true,
        integrations = {
          telescope = true,
          diffview = true,
        },
      })
    end,
  },
}
