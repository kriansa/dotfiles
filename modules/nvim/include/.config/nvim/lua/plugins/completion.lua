return function(use)
  use {
    'hrsh7th/nvim-cmp',
    requires = { 'hrsh7th/cmp-nvim-lsp', 'hrsh7th/cmp-buffer', 'hrsh7th/cmp-path' },
    config = function()
      local cmp = require('cmp')
      cmp.setup({
        mapping = cmp.mapping.preset.insert({
          ['<CR>'] = cmp.mapping.confirm {
            behavior = cmp.ConfirmBehavior.Replace,
            select = true,
          },
        }),
        sources = {
          { name = 'nvim_lsp' },
          { name = 'buffer' },
          { name = 'path' },
        },
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
      local capabilities = vim.lsp.protocol.make_client_capabilities()
      capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

      -- Setup each available language server
      local lspconfig = require('lspconfig')
      local lsp_flags = { debounce_text_changes = 150 }
      local servers = {
        'solargraph', 'tsserver', 'eslint', 'vls', 'cssls', 'html', 'pylsp', 'ansiblels', 'bashls',
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
