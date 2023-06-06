return function(use)
  use {
    'tamago324/lir.nvim',
    config = function()
      require('lir').setup({
        show_hidden_files = true,
        hide_cursor = true,
        ignore = {},
        devicons = {
          enable = false,
          highlight_dirname = false
        },
        mappings = mappings.lir,
        float = {
          winblend = 0,
          curdir_window = {
            enable = true,
            highlight_dirname = true,
          },

          -- You can define a function that returns a table to be passed as the third
          -- argument of nvim_open_win().
          win_opts = function()
            local width = math.floor(vim.o.columns * 0.8) - 3
            local height = math.floor(vim.o.lines * 0.8)
            return {
              border = {
                "┌", "─", "┐", "│", "┘", "─", "└", "│",
              },
              width = width,
              height = height,
              row = 3,
              col = math.floor((vim.o.columns - width) / 2),
            }
          end,
        },
      })
    end
  }

  -- Main tree-view
  use {
    'nvim-tree/nvim-tree.lua',
    requires = { 'nvim-tree/nvim-web-devicons' },
    config = function()
      -- Customize colors

      -- highlight NvimTreeSymlink guifg=blue gui=bold,underline
      -- highlight NvimTreeFolderName guifg=blue gui=bold,underline
      -- highlight NvimTreeEmptyFolderName guifg=blue gui=bold,underline
      -- highlight NvimTreeOpenedFolderName guifg=blue gui=bold,underline
      -- highlight NvimTreeRootFolder guifg=blue gui=bold,underline
      -- highlight NvimTreeExecFile guifg=blue gui=bold,underline

      require('nvim-tree').setup({
        -- Compatibility with Dirvish
        hijack_netrw = false,
        hijack_directories = { enable = false },

        git = { enable = false },
        diagnostics = { enable = false },
        update_focused_file = { enable = true },
        sync_root_with_cwd = true,
        trash = { cmd = "trash" },
        hijack_cursor = true,
        remove_keymaps = true,
        on_attach = mappings.nvim_tree,
        filters = {
          custom = {
            -- Sync this value with the one on .rgignore
            "^.git$"
          },
        },
        renderer = {
          root_folder_label = false,
          highlight_git = false,
          group_empty = true,
          indent_markers = {
            enable = false,
          },
          icons = {
            show = {
              git = false,
              folder = true,
              folder_arrow = true,
              file = false,
            },
            glyphs = {
              symlink = "",
            },
          },
          special_files = {},
        },
      })
    end,
  }
end
