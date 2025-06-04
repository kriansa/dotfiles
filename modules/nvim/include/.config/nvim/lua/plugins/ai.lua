return {
  {
    "zbirenbaum/copilot.lua",
    config = function()
      require("copilot").setup({
        panel = { enabled = false },
        suggestion = { enabled = false },
        filetypes = {
          yaml = true,
          markdown = false,
          help = false,
          text = false,
          gitcommit = false,
          gitrebase = false,
          hgcommit = false,
          svn = false,
          cvs = false,
          ["."] = false,
          sh = function ()
            return not string.match(vim.fs.basename(vim.api.nvim_buf_get_name(0)), '^%.env.*')
          end,
        },
      })
    end,
  },

  {
    "GeorgesAlkhouri/nvim-aider",
    dependencies = { "folke/snacks.nvim" },
    cmd = "Aider",
    config = function()
      require("nvim_aider").setup({
        aider_cmd = "aider",
        args = {"--architect"},
      })

      -- 1. Create mapping for normal mode
      -- vim.api.nvim_set_keymap('n', mappings.aider.toggle, "<cmd>Aider toggle<CR>", { silent = true })

      -- 2. From terminal on insert mode
      -- vim.api.nvim_create_autocmd("FileType", {
      --   pattern = "snacks_terminal",
      --   callback = function()
      --     vim.api.nvim_buf_set_keymap(0, "t", mappings.aider.toggle, "<cmd>Aider toggle<CR>", {noremap = true, desc = "Toggle Aider chat window"})
      --   end
      -- })
    end,
  },

  {
    "yetone/avante.nvim",
    event = "VeryLazy",
    version = false, -- Never set this value to "*"! Never!
    opts = {
      provider = "claude",
      auto_suggestions_provider = "copilot",
      providers = {
        claude = {
          api_key_name = "cmd:pass Anthropic/API-Key",
        },
      },
      selector = {
        provider = "telescope",
      },
      mappings = mappings.avante,
    },
    -- if you want to build from source then do `make BUILD_FROM_SOURCE=true`
    build = "make",
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "stevearc/dressing.nvim",
      "nvim-lua/plenary.nvim",
      "MunifTanjim/nui.nvim",
      --- The below dependencies are optional,
      "nvim-telescope/telescope.nvim", -- for file_selector provider telescope
      "nvim-tree/nvim-web-devicons", -- or echasnovski/mini.icons
      "zbirenbaum/copilot.lua", -- for providers='copilot'
    },
  },
}
