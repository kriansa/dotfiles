return function(use)
  use {
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
          markdown = false,
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
  }

  use {
    'hrsh7th/nvim-cmp',
    requires = {
      'hrsh7th/cmp-nvim-lsp', 'hrsh7th/cmp-buffer', 'hrsh7th/cmp-path', 'hrsh7th/cmp-cmdline',
      'dcampos/nvim-snippy', 'dcampos/cmp-snippy', 'honza/vim-snippets'
    },
    config = function()
      local cmp = require('cmp')

      cmp.setup({
        mapping = mappings.cmp_insert,
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
        mapping = mappings.cmp_cmdline,
        sources = cmp.config.sources({
          { name = 'path' },
          { name = 'cmdline' },
        })
      })
    end,
  }

  use {
    'neovim/nvim-lspconfig',
    after = 'nvim-cmp',
    config = function()
      -- Set the editor UI for LSP diagnostics
      vim.diagnostic.config({
        virtual_text = false,
        signs = true,
        underline = true,
        update_in_insert = false,
        severity_sort = true,
        float = {
          border = 'single',
          source = 'always',
          scope = 'line',
          header = "",
          prefix = ' ',
        },
      })

      -- Set left signs
      local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
      for type, icon in pairs(signs) do
        local hl = "DiagnosticSign" .. type
        vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
      end

      local on_attach = function(client, bufnr)
        vim.api.nvim_create_autocmd("CursorHold", {
          buffer = bufnr,
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
              close_events = { "BufLeave", "CursorMoved", "InsertEnter", "FocusLost" },
            })
          end
        })

        -- Apply keymap for lsp
        mappings.lsp(bufnr)
      end

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
              black = {
                enabled = true,
              },
              pylint = {
                enabled = true,
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
          on_attach = on_attach,
          capabilities = capabilities,
          flags = lsp_flags,
          settings = settings,
        })
      end
    end,
  }
end
