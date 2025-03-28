return {
  -- {
  --   "robitx/gp.nvim",
  --   config = function()
  --     local conf = {
  --       providers = {
  --         copilot = {
  --           endpoint = "https://api.githubcopilot.com/chat/completions",
  --           secret = {
  --             "bash",
  --             "-c",
  --             "cat ~/.config/github-copilot/apps.json | sed -e 's/.*oauth_token...//;s/\".*//'",
  --           },
  --         },
  --       }
  --     }
  --
  --     require("gp").setup(conf)
  --     -- Setup shortcuts here (see Usage > Shortcuts in the Documentation/Readme)
  --   end,
  -- },

  {
    "zbirenbaum/copilot.lua",
    config = function()
      require("copilot").setup({
        panel = {
          enabled = false,
          auto_refresh = false,
          keymap = {
            jump_prev = false,
            jump_next = false,
            accept = false,
            refresh = false,
            open = false,
          },
          layout = {
            position = "bottom", -- | top | left | right
            ratio = 0.4,
          },
        },
        suggestion = {
          enabled = false,
          auto_trigger = false,
          debounce = 75,
          keymap = mappings.gh_copilot.suggestion,
        },
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

  -- {
  --   "joshuavial/aider.nvim",
  --   config = function()
  --     require('aider').setup({
  --       auto_manage_context = true,
  --       default_bindings = false,
  --       debug = false,
  --       ignore_buffers = {'^term:', 'NeogitConsole', 'NvimTree_', 'neo-tree filesystem', '^fugitive://'}
  --     })
  --
  --     -- Setup a toggle keybinding
  --     -- 1. From normal mode
  --     vim.keymap.set({"n"}, mappings.aider.toggle, function()
  --       require("aider").AiderOpen("--architect")
  --
  --       vim.defer_fn(function()
  --         vim.cmd("startinsert")
  --       end, 100)
  --     end, {silent = true, desc = "Toggle Aider chat window"})
  --
  --     -- 2. From terminal on insert mode
  --     vim.api.nvim_set_keymap('t', mappings.aider.toggle, '<C-\\><C-n><C-w>q', {noremap = true})
  --
  --     -- 2. From terminal on normal mode
  --     vim.api.nvim_create_autocmd("FileType", {
  --       pattern = "AiderConsole",
  --       callback = function()
  --         vim.api.nvim_buf_set_keymap(0, "n", mappings.aider.toggle, "<C-w>q", {noremap = true, desc = "Toggle Aider chat window"})
  --       end
  --     })
  --   end
  -- },

  {
    "GeorgesAlkhouri/nvim-aider",
    -- keys = {
    --   { "<leader>At", "<cmd>AiderTerminalToggle<cr>", desc = "Open Aider" },
    --   { "<leader>As", "<cmd>AiderTerminalSend<cr>", desc = "Send to Aider", mode = { "n", "v" } },
    --   { "<leader>Ac", "<cmd>AiderQuickSendCommand<cr>", desc = "Send Command To Aider" },
    --   { "<leader>Ab", "<cmd>AiderQuickSendBuffer<cr>", desc = "Send Buffer To Aider" },
    --   { "<leader>A+", "<cmd>AiderQuickAddFile<cr>", desc = "Add File to Aider" },
    --   { "<leader>A-", "<cmd>AiderQuickDropFile<cr>", desc = "Drop File from Aider" },
    --   { "<leader>Ar", "<cmd>AiderQuickReadOnlyFile<cr>", desc = "Add File as Read-Only" },
    --   -- Example nvim-tree.lua integration if needed
    --   { "<leader>A+", "<cmd>AiderTreeAddFile<cr>", desc = "Add File from Tree to Aider", ft = "NvimTree" },
    --   { "<leader>A-", "<cmd>AiderTreeDropFile<cr>", desc = "Drop File from Tree from Aider", ft = "NvimTree" },
    -- },
    dependencies = { "folke/snacks.nvim" },
    config = function()
      require("nvim_aider").setup({
        aider_cmd = "aider",
        args = {"--architect"},
      })

      -- 1. Create mapping for normal mode
      vim.api.nvim_set_keymap('n', mappings.aider.toggle, "<cmd>AiderTerminalToggle<CR>", { silent = true })

      -- 2. From terminal on insert mode
      vim.api.nvim_create_autocmd("FileType", {
        pattern = "snacks_terminal",
        callback = function()
          vim.api.nvim_buf_set_keymap(0, "t", mappings.aider.toggle, "<cmd>AiderTerminalToggle<CR>", {noremap = true, desc = "Toggle Aider chat window"})
        end
      })
    end,
  },

  {
    "olimorris/codecompanion.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
    },
    config = function()
      require("codecompanion").setup({
        adapters = {
          anthropic = function()
            return require("codecompanion.adapters").extend("anthropic", {
              env = {
                api_key = "cmd:pass Anthropic/API-Key"
              },
            })
          end,

          copilot = function()
            return require("codecompanion.adapters").extend("copilot", {
              schema = {
                model = {
                  default = "claude-3.7-sonnet",
                },
              },
            })
          end,
        },
        strategies = {
          chat = {
            adapter = "copilot",
          },
          inline = {
            adapter = "copilot",
          },
        },
        display = {
          diff = {
            enabled = true,
            close_chat_at = 240, -- Close an open chat buffer if the total columns of your display are less than...
            layout = "vertical", -- vertical|horizontal split for default provider
            provider = "mini_diff", -- default|mini_diff
          },
        },
      })
    end,
  },
}
