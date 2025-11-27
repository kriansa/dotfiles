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
    'saghen/blink.cmp',
    dependencies = {
      'rafamadriz/friendly-snippets',
      'fang2hou/blink-copilot',
    },
    version = '1.*',

    opts = {
      -- 'default' (recommended) for mappings similar to built-in completions (C-y to accept)
      -- 'super-tab' for mappings similar to vscode (tab to accept)
      -- 'enter' for enter to accept
      -- 'none' for no mappings
      --
      -- All presets have the following mappings:
      -- C-space: Open menu or open docs if already open
      -- C-n/C-p or Up/Down: Select next/previous item
      -- C-e: Hide menu
      -- C-k: Toggle signature help (if signature.enabled = true)
      --
      -- See :h blink-cmp-config-keymap for defining your own keymap
      keymap = { preset = 'enter' },

      appearance = {
        -- 'mono' (default) for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
        -- Adjusts spacing to ensure icons are aligned
        nerd_font_variant = 'mono'
      },

      -- Don't enable on prompts
      enabled = function()
        return vim.bo.buftype ~= 'prompt' and vim.b.completion ~= false
      end,

      completion = {
        documentation = { auto_show = true },
        ghost_text = { enabled = true },
        list = {
          selection = {
            preselect = false,
          },
          max_items = 200,
        },
      },

      signature = { enabled = true },

      -- Default list of enabled providers defined so that you can extend it
      -- elsewhere in your config, without redefining it, due to `opts_extend`
      sources = {
        default = { 'copilot', 'lsp', 'path', 'snippets', 'buffer' },
        providers = {
          copilot = {
            name = "copilot",
            module = "blink-copilot",
            score_offset = 100,
            async = true,
          },
        },
      },

      -- (Default) Rust fuzzy matcher for typo resistance and significantly better performance
      -- You may use a lua implementation instead by using `implementation = "lua"` or fallback to the lua implementation,
      -- when the Rust fuzzy matcher is not available, by using `implementation = "prefer_rust"`
      --
      -- See the fuzzy documentation for more information
      fuzzy = { implementation = "prefer_rust_with_warning" }
    },
    opts_extend = { "sources.default" }
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

      -- Apply global settings to all LSP servers
      vim.lsp.config("*", {
        -- Add additional capabilities supported by blink.cmp
        capabilities = require('blink.cmp').get_lsp_capabilities(),
        flags = {
          debounce_text_changes = 150
        },
      })

      local util = require('lspconfig.util')

      -- Setup each available language server
      local server_configs = {
        ts_ls = {},
        vuels = {},
        gopls = {},

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
          -- Ruby LSP works by creating a new Gemfile that extends from the project's Gemfile and
          -- installs its own dependencies, then it uses that as a runtime of its own when invoking
          -- the ruby-lsp binary, thus having all your project's dependencies loaded. This way it
          -- can index the project dependencies more accurately.
          --
          -- For that to work as expected, it makes the assumption that your application
          -- dependencies (including the Ruby runtime) are installed correctly and can be executed
          -- by the same "runtime" as ruby-lsp -- I used the term "runtime" colloquially, but I
          -- meant that it expects you to run your application from the same shell and environment
          -- as ruby-lsp, so you're not able to containerize/virtualize your application and execute
          -- ruby-lsp from the outside. Unfortunately, there are several applications that are not
          -- that well maintained, and they will hardly install the dependencies outside of a very
          -- controlled environment such as a virtual machine or container with their old
          -- dependencies.
          --
          -- Legacy or not, these applications deserve the love of having proper code introspection.
          --
          -- The purpose of setting up ruby-lsp that way is to increase the interoperability of
          -- ruby-lsp by splitting the application runtime with the LSP server, making it agnostic
          -- to the application stack, such as Ruby and gem versions, so it can be used virtually
          -- with any codebase, even rotten ones.
          --
          -- Unfortunately, this is not without its shortcomings, and we will lose indexing
          -- project's specific dependencies, but for the most part where you just want simple
          -- navigation within the project root dir, it's super useful and works virtually across
          -- any project out of box.
          --
          -- ==> How it works
          --
          -- The way it works is by being executed on the same directory as your application root,
          -- so it can capture the Gemfile, then create a new folder (.ruby-lsp) that will serve as
          -- its own root. Then, it installs the dependencies with bundler, and when they're
          -- installed, it will start. After it start, it will re-invoke itself passing the bundler
          -- settings as env variables (such as BUNDLE_GEMFILE), which will finally start the lsp
          -- server. This process is described here:
          -- https://github.com/Shopify/ruby-lsp/blob/a300f56c5350756fbc4fc1855fba1e147ecf34a5/exe/ruby-lsp#L60-L89
          --
          -- ==> Workaround
          --
          -- We'll intercept this process and make ruby-lsp start on a "dummy" application that has
          -- a blank Gemfile, so there should be no incompatibility whatsoever when doing the
          -- dependency install. That's one part of it.
          --
          -- Then, we also make sure to call `ruby-lsp` from within this "dummy app", so it won't
          -- attempt to index the application specific dependencies.
          -- One last touch to avoid any application-specific dependency at the LSP runtime, we'll
          -- also turn off the project addons, a feature currently available at lsp-ruby so that it
          -- can load project's specific addons. It's currently not configurable, so one can't
          -- simply turn it off. That's why we have our own "entrypoint" to ruby-lsp, which
          -- overrides the existing addons loading process to disable the project's addons. This is
          -- currently set at `ruby-lsp-thin`.

          cmd = { "ruby-lsp-thin" },
          cmd_cwd = vim.fn.expand("$DOTFILES_PATH" .. "/modules/ruby/data/ruby-lsp"),

          init_options = {
            indexing = {
              excludedGems = { "rubocop", "rubocop-performance", "standard", "standard-rails" },
              excludedPatterns = { "**/test/**.rb", "**/spec/**/*_spec.rb", "**/activerecord-*/examples/**/*.rb" },
              excludedMagicComments = { "compiled:true" },
            },
            addonSettings = {
              ["Ruby LSP Rails"] = {
                enablePendingMigrationsPrompt = false,
              },
            },
            linters = {},
          },
        },
      }

      -- Enable this when debugging any LSP server
      -- vim.lsp.log.set_level(vim.lsp.log_levels.DEBUG)

      for name, config in pairs(server_configs) do
        if config ~= nil and next(config) ~= nil then
          vim.lsp.config(name, config)
        end

        vim.lsp.enable(name)
      end
    end,
  },
}
