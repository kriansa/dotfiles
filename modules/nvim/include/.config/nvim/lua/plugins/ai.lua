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

      vim.api.nvim_create_user_command('CopilotToggle', function()
        local cmd = require("copilot.command")
        if require("copilot.client").is_disabled() then
          cmd.enable()
        else
          cmd.disable()
        end
      end, { desc = "Toggle Github Copilot" })
    end,
  },

  {
    "GeorgesAlkhouri/nvim-aider",
    dependencies = { "folke/snacks.nvim" },
    cmd = "Aider",
    config = function()
      require("nvim_aider").setup({
        aider_cmd = "aider",
        args = {"--architect", "--no-gitignore", "--watch"},
        picker_cfg = {
          preset = "select",
          layout = {
            title = "",
            border = "single",
          },
        },
      })
    end,
  },

  {
    "ravitemer/mcphub.nvim",
    event = "VeryLazy",
    dependencies = {
      "nvim-lua/plenary.nvim",
    },
    build = "npm install -g mcp-hub@latest",
    config = function()
      require("mcphub").setup({
        -- auto_approve = true,
      })
    end
  },

  {
    "olimorris/codecompanion.nvim",
    cmd = { "CodeCompanion", "CodeCompanionChat", "CodeCompanionCmd", "CodeCompanionActions" },
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
    },
    opts = {
      strategies = {
        chat = {
          adapter = "anthropic",
        },
        inline = {
          adapter = "copilot",
          keymaps = {
            accept_change = {
              modes = { n = mappings.code_companion.accept },
              description = "Accept the suggested change",
            },
            reject_change = {
              modes = { n = mappings.code_companion.reject },
              description = "Reject the suggested change",
            },
          },
        },
        cmd = {
          adapter = "copilot",
        },
      },
      extensions = {
        mcphub = {
          callback = "mcphub.extensions.codecompanion",
          opts = {
            show_result_in_chat = true,  -- Show mcp tool results in chat
            make_vars = true,            -- Convert resources to #variables
            make_slash_commands = true,  -- Add prompts as /slash commands
          }
        }
      },
      display = {
        action_palette = {
          provider = "snacks",
        },
      },
    },
  },
}
