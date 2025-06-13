return {
  -- Telescope
  {
    'nvim-telescope/telescope.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-telescope/telescope-fzf-native.nvim',
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

      local live_grep_formatter = function()
        local Path = require("plenary.path")
        local utils = require("telescope.utils")

        function display_line_entry(entry)
          local display_filename = utils.transform_path({ cwd = vim.loop.cwd() }, entry.filename)
          local display = string.format("%s:%s:%s", display_filename, entry.lnum, entry.text)

          -- Set highlight delimiters for string text
          local text_start = string.len(display_filename) + string.len(entry.lnum) + 2
          local text_end = text_start + string.len(entry.text)

          local lnum_start = string.len(display_filename) + 1
          local lnum_end = lnum_start + string.len(entry.lnum)

          return display, {
            { { text_start, text_end }, "TelescopeResultsField" },
            { { lnum_start, lnum_end }, "TelescopeResultsNumber" }
          }
        end

        return function(line)
          local _, _, filename, lnum, col, text = string.find(line, [[(..-):(%d+):(%d+):(.*)]])
          local cwd = vim.loop.cwd()

          local ok
          ok, lnum = pcall(tonumber, lnum)
          if not ok then
            lnum = nil
          end

          ok, col = pcall(tonumber, col)
          if not ok then
            col = nil
          end

          if Path:new(filename):is_absolute() then
            path = filename
          else
            path = Path:new({ cwd, filename }):absolute()
          end

          return {
            value = line,
            ordinal = line,
            cwd = cwd,
            path = path,
            filename = filename,
            lnum = lnum,
            col = col,
            text = text,
            display = display_line_entry,
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
            -- Avoids cases where one can press a key too quickly
            n = {
              ["<ESC>"] = "close",
              ["<C-c>"] = "close",
            },
          },
          cache_picker = {
            num_pickers = 10,
          },
          dynamic_preview_title = true,
          results_title = false,
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
              preview_height = 0,
              height = { padding = 0 },
              width = { padding = 0 },
            },
            disable_devicons = true,
            sorting_strategy = "descending",
            find_command = { "rg", "--files", "--hidden", "--color=never", "--no-heading" },
          },

          live_grep = {
            borderchars = { '─', '│', '─', '│', '┌', '┐', '┘', '└' },
            prompt_prefix = "Search ❯ ",
            results_title = "",
            preview_title = "",
            prompt_title = "",
            layout_strategy = "vertical",
            layout_config = {
              preview_height = 0.4,
              prompt_position = "bottom",
              height = { padding = 0 },
              width = { padding = 0 },
            },
            disable_coordinates = true,
            disable_devicons = true,
            entry_maker = live_grep_formatter(),
            vimgrep_arguments = {
              "rg", "--color=never", "--no-heading", "--hidden", "--column", "--smart-case",
              "--with-filename", "--line-number", "--trim", "--fixed-strings"
            },
          },
        },
        extensions = {
          fzf = {
            fuzzy = true,                    -- false will only do exact matching
            override_generic_sorter = true,  -- override the generic sorter
            override_file_sorter = true,     -- override the file sorter
            case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
          },
        },
      })

      require('telescope').load_extension('fzf')
    end
  },

  -- Use same fuzzy finder algorithm as FZF
  {
    'nvim-telescope/telescope-fzf-native.nvim',
    build = 'make',
  },
}
