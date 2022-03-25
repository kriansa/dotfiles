return function(use)
  -- Telescope
  use {
    'nvim-telescope/telescope.nvim',
    requires = 'nvim-lua/plenary.nvim',
    after = {
      'telescope-fzf-native.nvim',
      "telescope-frecency.nvim",
    },
    config = function()
      -- Customize the output of the Telescope buffers
      local buffer_formatter = function()
        local displayer = require("telescope.pickers.entry_display").create({
          separator = " ",
          items = {
            { remaining = true },
          },
        })

        return function(entry)
          local bufname = entry.info.name ~= "" and entry.info.name or "[No Name]"
          -- if bufname is inside the cwd, trim that part of the string
          bufname = require('plenary.path'):new(bufname):normalize(vim.loop.cwd())

          return {
            valid = true,
            value = bufname,
            ordinal = entry.bufnr .. " : " .. bufname,
            bufnr = entry.bufnr,
            display = function(entry)
              local display_bufname = require("telescope.utils")
              .transform_path({ cwd = vim.loop.cwd() }, entry.filename)
              return displayer({ display_bufname })
            end,
            -- This param is passed to the `display` callback above
            filename = bufname,
          }
        end
      end

      require('telescope').setup({
        defaults = {
          prompt_prefix = "❯ ",
          selection_caret = "> ",
          path_display = { "truncate" },
          scroll_strategy = 'cycle',
          default_mappings = {
            i = mappings.telescope_defaults(),
          },
        },
        pickers = {
          buffers = {
            theme = "dropdown",
            borderchars = {
              { '─', '│', '─', '│', '┌', '┐', '┘', '└'},
              prompt = {"─", "│", " ", "│", '┌', '┐', "│", "│"},
              results = {"─", "│", "─", "│", "├", "┤", "┘", "└"},
              preview = { '─', '│', '─', '│', '┌', '┐', '┘', '└'},
            },
            prompt_prefix = "Buffers ❯ ",
            prompt_title = "",
            sort_mru = true,
            sort_lastused = true,
            disable_devicons = true,
            previewer = false,
            ignore_current_buffer = true,
            entry_maker = buffer_formatter(),
            mappings = {
              i = mappings.telescope_buffers,
            },
          },

          find_files = {
            borderchars = { '─', '│', '─', '│', '┌', '┐', '┘', '└' },
            prompt_prefix = "Files ❯ ",
            results_title = "",
            preview_title = "",
            prompt_title = "",
            layout_strategy = "vertical",
            layout_config = {
              height = { padding = 1 },
            },
            disable_devicons = true,
            sorting_strategy = "descending",
            find_command = { "rg", "--files", "--hidden", "--color=never", "--no-heading" },
          },

          live_grep = {
            borderchars = { '─', '│', '─', '│', '┌', '┐', '┘', '└' },
            prompt_prefix = "Regexp ❯ ",
            results_title = "",
            preview_title = "",
            prompt_title = "",
            disable_coordinates = true,
            disable_devicons = true,
            vimgrep_arguments = {
              "rg", "--color=never", "--no-heading", "--hidden", "--column", "--smart-case",
              "--with-filename", "--line-number"
            },
          },

          grep_string = {
            borderchars = { '─', '│', '─', '│', '┌', '┐', '┘', '└' },
            prompt_prefix = "Filter ❯ ",
            results_title = "",
            preview_title = "",
            disable_coordinates = true,
            disable_devicons = true,
            vimgrep_arguments = {
              "rg", "--color=never", "--no-heading", "--hidden", "--column", "--smart-case",
              "--with-filename", "--line-number"
            },
          }
        },
      })

      require('telescope').load_extension('fzf')
      require("telescope").load_extension("frecency")
    end
  }

  -- Use same fuzzy finder algorithm as FZF
  use {
    'nvim-telescope/telescope-fzf-native.nvim',
    run = 'make',
  }

  -- Use a sqlite database to provide frecency (frequency + recency)
  use {
    "nvim-telescope/telescope-frecency.nvim",
    requires = { 'tami5/sqlite.lua' },
  }
end
