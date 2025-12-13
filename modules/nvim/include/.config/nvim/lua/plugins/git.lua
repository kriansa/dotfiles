return {
  {
    'nvim-mini/mini.diff',
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

  {
    "f-person/git-blame.nvim",
    cmd = {
      "GitBlameToggle",
      "GitBlameEnable",
      "GitBlameDisable",
      "GitBlameOpenCommitURL",
      "GitBlameOpenFileURL",
      "GitBlameCopyCommitURL",
      "GitBlameCopyFileURL",
      "GitBlameCopyPRURL",
      "GitBlameCopySHA",
    },
    config = function()
      require('gitblame').setup({
        enabled = false,
        virtual_text_column = 1,
        message_template = "  <author>, <date> - <summary>",
        date_format = "%r",
        message_when_not_committed = "  -- Uncommited --",
      })
    end
  },

  {
    "sindrets/diffview.nvim",
    config = function()
      require("diffview").setup({
        use_icons = false,
        keymaps = {
          file_panel = {
            { "n", "q", function() vim.cmd("silent! tabc") end, { desc = "Quit" } },
          },
        },
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
      "folke/snacks.nvim",
    },
    cmd = "Neogit",
    config = function()
      local neogit = require("neogit")
      neogit.setup({
        auto_refresh = false,
        disable_commit_confirmation = true,
        disable_insert_on_commit = true,
        integrations = {
          snacks = true,
          diffview = true,
        },
        signs = {
          item = { "🞂", "🞃"},
          section = { "🞂", "🞃"},
        },
      })

      vim.api.nvim_create_autocmd("User", {
        pattern = { "NeogitStatusRefreshed" },
        callback = function()
          vim.opt_local.list = true
        end,
      })

      vim.api.nvim_create_user_command("GitBrowse", Snacks.gitbrowse.open, {})
    end,
  },
}
