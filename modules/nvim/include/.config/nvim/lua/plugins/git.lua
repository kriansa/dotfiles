return {
  {
    'echasnovski/mini.diff',
    config = function()
      require('mini.diff').setup({
        view = {
          style = 'sign',
          signs = { add = '+', change = '~', delete = '_' },
        },
        mappings = mappings.minidiff,
      })
    end
  },

  -- Gitsigns is showing some problems with slowness, so I'm using minidiff for now
  --[[ {
    'lewis6991/gitsigns.nvim',
    config = function()
      require('gitsigns').setup({
        signs = {
          add = { text = '+' },
          change = { text = '~' },
          delete = { text = '_' },
          topdelete = { text = '‾' },
          changedelete = { text = '~' },
          untracked = { text = '┆' },
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

  -- Alternatively, vim-gitgutter could also replace gitsigns
  --
  --[[ {
    "airblade/vim-gitgutter",
    config = function()
      vim.g.gitgutter_map_keys = 0
      vim.g.gitgutter_grep = "rg"

      vim.g.gitgutter_sign_added = "+"
      vim.g.gitgutter_sign_modified = "~"
      vim.g.gitgutter_sign_removed = "_"
      vim.g.gitgutter_sign_removed_first_line = "‾"
      vim.g.gitgutter_sign_removed_above_and_below = "‾"
      vim.g.gitgutter_sign_modified_removed = "~"

      mappings.gitgutter()
    end,
  }, ]]

  {
    "f-person/git-blame.nvim",
    config = function()
      require('gitblame').setup({
        enabled = false,
        virtual_text_column = 1,
        message_template = "  <author>, <date> - <summary>",
        date_format = "%r",
        message_when_not_committed = "  -- Uncommited --",
      })

      mappings.git_blame()
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
    dependencies = {
      "nvim-lua/plenary.nvim",
      "sindrets/diffview.nvim",
      "nvim-telescope/telescope.nvim"
    },
    cmd = "Neogit",
    config = function()
      local neogit = require("neogit")
      neogit.setup({
        auto_refresh = false,
        disable_commit_confirmation = true,
        disable_insert_on_commit = true,
        integrations = {
          telescope = true,
          diffview = true,
        },
        signs = {
          item = { "🞂", "🞃"},
          section = { "🞂", "🞃"},
        },
      })
    end,
  },
}
