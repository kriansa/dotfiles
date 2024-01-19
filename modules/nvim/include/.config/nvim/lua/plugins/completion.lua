return {
  {
    "zbirenbaum/copilot.lua",
    cmd = "Copilot",
    event = "InsertEnter",
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
          enabled = true,
          auto_trigger = true,
          debounce = 75,
          keymap = mappings.gh_copilot.suggestion,
        },
        filetypes = {
          yaml = true,
          markdown = true,
          help = false,
          gitcommit = false,
          gitrebase = false,
          hgcommit = false,
          svn = false,
          cvs = false,
          ["."] = false,
        },
      })
    end,
  },

  {
    'hrsh7th/nvim-cmp',
    dependencies = {
      'hrsh7th/cmp-nvim-lsp', 'hrsh7th/cmp-buffer', 'hrsh7th/cmp-path', 'hrsh7th/cmp-cmdline',
      'dcampos/nvim-snippy', 'dcampos/cmp-snippy', 'honza/vim-snippets'
    },
    config = function()
      local cmp = require('cmp')

      cmp.setup({
        mapping = mappings.cmp_insert(),
        snippet = {
          expand = function(args)
            require('snippy').expand_snippet(args.body)
          end
        },
        sources = cmp.config.sources({
          { name = 'nvim_lsp' },
          { name = 'snippy' },
          { name = 'buffer' },
          { name = 'path' },
        })
      })

      cmp.setup.cmdline(':', {
        mapping = mappings.cmp_cmdline(),
        sources = cmp.config.sources({
          { name = 'path' },
          { name = 'cmdline' },
        })
      })
    end,
  },

  {
    "folke/which-key.nvim",
    config = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 300
      require("which-key").setup({
        -- your configuration comes here
        -- or leave it empty to use the default settings
        -- refer to the configuration section below
      })
    end
  },

  -- {
  --   "mfussenegger/nvim-lint",
  --   -- TODO: Evaluate this plugin OR https://github.com/nvimtools/none-ls.nvim/tree/main
  --   config = function()
  --     require('lint').linters_by_ft = {
  --       -- TODO: Create a single "linter" wrapper that runs all ruby linters according to the
  --       -- project config (e.g. .standardrb, .rubocop.yml, .reek.yml)
  --       -- Use: [ standard || rubocop ], reek
  --       -- ruby = {"rubocop"},
  --     }
  --
  --     vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost", "InsertLeave" }, {
  --       callback = function()
  --         require("lint").try_lint()
  --       end,
  --     })
  --   end
  -- },

  {
    'neovim/nvim-lspconfig',
    dependencies = { 'hrsh7th/nvim-cmp', 'hrsh7th/cmp-nvim-lsp' },
    config = function()
      -- Set the editor UI for vim diagnostics
      vim.diagnostic.config({
        virtual_text = false,
        signs = true,
        underline = true,
        update_in_insert = true,
        severity_sort = true,
        float = {
          border = "single",
          source = true,
          scope = "line",
          header = "",
          prefix = "",
        },
      })

      -- Set diagnostic signs
      local signs = { Error = "󰅚 ", Warn = "󰀪 ", Hint = "󰌶 ", Info = "󰋽 " }
      for type, icon in pairs(signs) do
        local hl = "DiagnosticSign" .. type
        vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
      end

      -- Enable opening float popup on hovering the line
      vim.api.nvim_create_autocmd("CursorHold", {
        callback = function()
          -- Don't open diagnostic if there's at least one float window open (we assume it's
          -- diagnostic float window)
          for _, win in ipairs(vim.api.nvim_list_wins()) do
            local is_floating_window = vim.api.nvim_win_get_config(win).relative ~= ""
            if is_floating_window then
              return
            end
          end

          vim.diagnostic.open_float({
            focus = false,
            close_events = { "BufLeave", "CursorMoved", "InsertEnter", "FocusLost" },
          })
        end
      })

      -- Use LspAttach autocommand to only map the following keys
      -- after the language server attaches to the current buffer
      vim.api.nvim_create_autocmd('LspAttach', {
        callback = function(ev)
          -- Enable completion triggered by <c-x><c-o>
          vim.bo[ev.buf].omnifunc = 'v:lua.vim.lsp.omnifunc'

          -- Apply buffer local mappings
          mappings.lsp(ev.buf)
        end,
      })

      -- Add additional capabilities supported by nvim-cmp
      local capabilities = require('cmp_nvim_lsp').default_capabilities()

      -- Setup each available language server
      local lspconfig = require('lspconfig')
      local lsp_flags = { debounce_text_changes = 150 }
      local servers = {
        'solargraph', 'tsserver', 'eslint', 'vls', 'cssls', 'html', 'pylsp', 'ansiblels', 'bashls',
        'gopls',
      }
      local server_settings = {
        pylsp = {
          pylsp = {
            plugins = {
              -- Use black for formatting
              black = {
                enabled = true,
              },
              -- Use pylint for linting
              pylint = {
                enabled = true,
              },
              -- Then disable all else
              yapf = {
                enabled = false,
              },
              autopep8 = {
                enabled = false,
              },
            },
          },
        },
      }

      for _, lsp in ipairs(servers) do
        settings = server_settings[lsp] or {}
        lspconfig[lsp].setup({
          capabilities = capabilities,
          flags = lsp_flags,
          settings = settings,
        })
      end
    end,
  },
}
