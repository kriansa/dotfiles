function ruby_linter_command()
  local dirname = vim.fn.expand("%:p:h")

  local found_standard = vim.fs.find({".standard.yml", ".standard.yaml"}, { upward = true, path = dirname })[1]
  local found_rubocop = vim.fs.find({".rubocop.yml", ".rubocop.yaml"}, { upward = true, path = dirname })[1]

  if found_rubocop and not found_standard then
    return "rubocop"
  end

  -- By default we use standardrb
  return "standardrb"
end

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
      require("which-key").setup()
    end
  },

  {
    "stevearc/conform.nvim",
    config = function()
      -- vim.o.formatexpr = "v:lua.require('conform').formatexpr({'timeout_ms':2000})"

      require("conform").setup({
        formatters_by_ft = {
          -- A lot of languages are supported by prettier
          ["javascript"] = { "prettier" },
          ["javascriptreact"] = { "prettier" },
          ["typescript"] = { "prettier" },
          ["typescriptreact"] = { "prettier" },
          ["vue"] = { "prettier" },
          ["css"] = { "prettier" },
          ["scss"] = { "prettier" },
          ["less"] = { "prettier" },
          ["html"] = { "prettier" },
          ["json"] = { "prettier" },
          ["jsonc"] = { "prettier" },
          ["yaml"] = { "prettier" },
          ["markdown"] = { "prettier" },
          ["markdown.mdx"] = { "prettier" },
          ["graphql"] = { "prettier" },
          ["handlebars"] = { "prettier" },

          ["go"] = { "goimports", "gofmt" },
          ["python"] = { "isort", "black" },

          ["ruby"] = function() return { ruby_linter_command() } end,
        },
      })
    end
  },

  {
    "mfussenegger/nvim-lint",
    config = function()
      local sev = vim.diagnostic.severity

      local severity_map = {
        ['fatal'] = vim.diagnostic.severity.ERROR,
        ['error'] = vim.diagnostic.severity.ERROR,
        ['warning'] = vim.diagnostic.severity.WARN,
        ['convention'] = vim.diagnostic.severity.HINT,
        ['refactor'] = vim.diagnostic.severity.INFO,
        ['info'] = vim.diagnostic.severity.INFO,
      }

      -- Define a linter wrapper that will use standardrb or rubocop depending on the presence of a
      -- configuration file
      require('lint').linters.standardrb_or_rubocop = {
        cmd = ruby_linter_command,
        args = {"--force-exclusion", "--stdin", function() return vim.fn.expand("%:p") end, "--format", "json"},
        stdin = true,
        ignore_exitcode = true,
        parser = function(output)
          local diagnostics = {}
          local decoded = vim.json.decode(output)
          local offences = decoded.files[1].offenses

          for _, off in pairs(offences or {}) do
            table.insert(diagnostics, {
              lnum = off.location.start_line - 1,
              col = off.location.start_column - 1,
              end_lnum = off.location.last_line - 1,
              end_col = off.location.last_column,
              severity = severity_map[off.severity],
              message = off.message,
              code = off.cop_name,
              user_data = {
                lsp = {
                  code = off.cop_name,
                }
              }
            })
          end

          return diagnostics
        end,
      }

      -- function json_diagnostic(find_offenses, transform_offenses)
      --   return function(output)
      --     local diagnostics = {}
      --     local offenses = vim.json.decode(output)
      --     if json_entrypoint then offenses = json_entrypoint(offenses) end
      --
      --     for _, offense in pairs(offences or {}) do
      --       table.insert(diagnostics, transform_offenses(offense))
      --     end
      --
      --     return diagnostics
      --   end
      -- end
      --
      -- require('lint').linters.ansible_lint = {
      --   cmd = 'ansible-lint',
      --   args = { '--offline', '-f', 'json', function() return vim.fn.expand("%:p") end },
      --   ignore_exitcode = true,
      --   parser = json_diagnostic(nil, function(offense)
      --     return {
      --       lnum =
      --     }
      --   end)
      -- }

      require('lint').linters.vale.ignore_exitcode = true

      require('lint').linters_by_ft = {
        -- TODO: Add reek
        ruby = {"standardrb_or_rubocop"},
        markdown = {"vale"},
        python = {"pylint", "mypy"},

        -- Not working:
        -- ["yaml.ansible"] = {"ansible_lint"},

        javascript = {"eslint"},
        javascriptreact = {"eslint"},
        typescript = {"eslint"},
        typescriptreact = {"eslint"},
        vue = {"eslint"},

        go = {"golangcilint"},

        fish = {"fish"},
        sh = {"shellcheck"},
      }

      vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost", "InsertLeave", "TextChanged" }, {
        callback = function()
          require("lint").try_lint(nil, { ignore_errors = true })
        end,
      })
    end
  },

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
        'solargraph', 'tsserver', 'vuels', 'pylsp', 'gopls',
      }
      local server_settings = {
        pylsp = {
          pylsp = {
            plugins = {
              black = { enabled = false },
              pylint = { enabled = false },
              yapf = { enabled = false },
              autopep8 = { enabled = false },
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
