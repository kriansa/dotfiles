-- Prioritize standardrb when both rubocop and standardrb are installed
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
    "dcampos/nvim-snippy",
    config = function()
      require("snippy").setup({
        mappings = mappings.snippy,
      })
    end,
  },

  {
    'hrsh7th/nvim-cmp',
    dependencies = {
      'hrsh7th/cmp-nvim-lsp', 'hrsh7th/cmp-buffer', 'hrsh7th/cmp-path', 'hrsh7th/cmp-cmdline',
      'dcampos/nvim-snippy', 'dcampos/cmp-snippy', 'honza/vim-snippets', 'zbirenbaum/copilot-cmp',
      'mattn/emmet-vim', 'dcampos/cmp-emmet-vim',
    },
    config = function()
      local cmp = require('cmp')

      -- Enable copilot-cmp
      -- require("copilot_cmp").setup()

      cmp.setup({
        mapping = mappings.cmp_insert(),
        snippet = {
          expand = function(args)
            require('snippy').expand_snippet(args.body)
          end
        },
        sources = cmp.config.sources({
          -- { name = "copilot" },
          { name = 'nvim_lsp' },
          { name = 'buffer' },
          { name = 'path' },
          { name = 'snippy' },
          { name = "emmet_vim" },
        }),
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

  -- {
  --   "folke/which-key.nvim",
  --   config = function()
  --     vim.o.timeout = true
  --     vim.o.timeoutlen = 300
  --     require("which-key").setup()
  --   end
  -- },

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

          -- This encapsulates `ruby_linter_command` to wrap the result around a table
          ["ruby"] = function() return { ruby_linter_command() } end,
        },
      })
    end
  },

  {
    "mfussenegger/nvim-lint",
    config = function()
      -- Define a linter wrapper that will use standardrb or rubocop depending on the presence of a
      -- configuration file
      require('lint').linters.standardrb_or_rubocop = {
        cmd = ruby_linter_command,
        args = {
          "--force-exclusion",
          "--format", "json",
          "--stdin",
          "--server",
          function() return vim.fn.expand("%:p") end,
        },
        stdin = true,
        ignore_exitcode = true,
        parser = function(output)
          local diagnostics = {}
          local decoded = vim.json.decode(output)
          local offences = #decoded.files > 0 and decoded.files[1].offenses or {}
          local severity_map = {
            ['fatal'] = vim.diagnostic.severity.ERROR,
            ['error'] = vim.diagnostic.severity.ERROR,
            ['warning'] = vim.diagnostic.severity.WARN,
            ['convention'] = vim.diagnostic.severity.HINT,
            ['refactor'] = vim.diagnostic.severity.INFO,
            ['info'] = vim.diagnostic.severity.INFO,
          }

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

      require('lint').linters.reek = {
        cmd = 'reek',
        args = { '-f', 'json', '--stdin-filename', function() return vim.fn.expand("%:p") end },
        stdin = true,
        ignore_exitcode = true,
        parser = function(output)
          local diagnostics = {}
          local offenses = vim.json.decode(output)

          for _, entry in pairs(offenses or {}) do
            table.insert(diagnostics, {
              lnum = entry.lines[1] - 1,
              end_lnum = (entry.lines[2] or entry.lines[1]) - 1,
              col = 0,
              severity = vim.diagnostic.severity.INFO,
              message = entry.context .. " " .. entry.message,
              code = entry.smell_type,
            })
          end

          return diagnostics
        end,
      }

      -- Defines a linter for ansible_lint that uses the sarif output so we get more details on each
      -- linter run, as opposed to the default where all rules have the same severity
      require('lint').linters.ansible_lint = {
        cmd = 'ansible-lint',
        args = { '--offline', '-f', 'sarif', function() return vim.fn.expand("%:p") end },
        ignore_exitcode = true,
        parser = function(output)
          local diagnostics = {}
          local decoded = vim.json.decode(output)
          local offenses = decoded and decoded.runs and decoded.runs[1] and decoded.runs[1].results or {}
          local severity_map = {
            ['none'] = vim.diagnostic.severity.HINT,
            ['note'] = vim.diagnostic.severity.INFO,
            ['warning'] = vim.diagnostic.severity.WARN,
            ['error'] = vim.diagnostic.severity.ERROR,
          }
          local rules = {}
          local linter_rules = decoded and decoded.runs and decoded.runs[1] and decoded.runs[1].tool and decoded.runs[1].tool.driver and decoded.runs[1].tool.driver.rules or {}
          for _, rule in ipairs(linter_rules) do
            rules[rule.id] = rule.shortDescription.text
          end

          -- Loop through the offenses list
          for _, entry in pairs(offenses) do
            table.insert(diagnostics, {
              lnum = entry.locations[1].physicalLocation.region.startLine - 1,
              col = (entry.locations[1].physicalLocation.region.startColumn or 1) - 1,
              severity = severity_map[entry.level],
              message = rules[entry.ruleId],
              code = entry.ruleId,
            })
          end

          return diagnostics
        end,
      }

      require('lint').linters.vale.ignore_exitcode = true

      require('lint').linters_by_ft = {
        ruby = {"standardrb_or_rubocop", "reek"},
        markdown = {"vale"},
        python = {"pylint", "mypy"},

        ["yaml.ansible"] = {"ansible_lint"},

        javascript = {"eslint"},
        javascriptreact = {"eslint"},
        typescript = {"eslint"},
        typescriptreact = {"eslint"},
        vue = {"eslint"},

        go = {"golangcilint"},

        fish = {"fish"},
        sh = {"shellcheck"},
      }

      -- Set the linter to enabled by default
      vim.g._linter_enabled = true

      vim.api.nvim_create_user_command('LintToggle', function()
        vim.g._linter_enabled = not vim.g._linter_enabled

        local status
        if vim.g._linter_enabled then
          status = "enabled"
          require("lint").try_lint(nil, { ignore_errors = true })
        else
          status = "disabled"
          vim.diagnostic.reset()
        end

        print("Linter: " .. status)
      end, { desc = "Toggle the automatic linter execution on buffers" })

      vim.api.nvim_create_autocmd({ "BufEnter", "BufRead", "BufWritePost", "TextChanged" }, {
        callback = function()
          if vim.g._linter_enabled then
            require("lint").try_lint(nil, { ignore_errors = true })
          end
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
        signs = {
          text = {
            [vim.diagnostic.severity.ERROR] = "󰅚 ",
            [vim.diagnostic.severity.WARN] = "󰀪 ",
            [vim.diagnostic.severity.HINT] = "󰌶 ",
            [vim.diagnostic.severity.INFO] = "󰋽 ",
          },
        },
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
          mappings.lsp(ev.buf)
        end,
      })

      -- Add additional capabilities supported by nvim-cmp
      local capabilities = require('cmp_nvim_lsp').default_capabilities()

      -- Setup each available language server
      local lspconfig = require('lspconfig')
      local lsp_flags = { debounce_text_changes = 150 }
      local servers = { 'ts_ls', 'vuels', 'pylsp', 'gopls', 'ruby_lsp' }
      local server_config = {
        pylsp = {
          settings = {
            pylsp = {
              plugins = {
                black = { enabled = false },
                pylint = { enabled = false },
                yapf = { enabled = false },
                autopep8 = { enabled = false },
              },
            },
          },
        },

        ruby_lsp = {
          cmd_env = { BUNDLE_GEMFILE = vim.fn.expand("$DOTFILES_PATH" .. "/modules/ruby/data/ruby-lsp/Gemfile") },
          indexing = {
            excludedGems = { "rubocop", "rubocop-performance", "standard", "standard-rails" },
            excludedPatterns = { "**/test/**.rb", "**/spec/**/*_spec.rb", "**/activerecord-*/examples/**/*.rb" },
            excludedMagicComments = { "compiled:true" },
          },
        },
      }

      for _, lsp in ipairs(servers) do
        config = server_config[lsp] or {}
        lspconfig[lsp].setup({
          capabilities = capabilities,
          flags = lsp_flags,
          settings = config.settings,
          cmd = config.cmd,
          cmd_env = config.cmd_env,
        })
      end
    end,
  },
}
