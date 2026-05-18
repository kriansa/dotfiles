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
        ruff = {},
        gopls = {
          settings = {
            -- Gopls doesn't support dynamic workspace folders, so we need to force it to build
            -- the current package using the combination of tags for it to be effective, if the
            -- current file is in a package that has build tags.
            --
            -- golangci-lint for instance supports config files on parent dirs, so we can set
            -- build flags at the package level
            -- See: https://github.com/golang/go/issues/65757
            gopls = {
              buildFlags = { "-tags=integration" },
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
