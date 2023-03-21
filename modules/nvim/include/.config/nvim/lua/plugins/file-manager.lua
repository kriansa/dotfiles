return function(use)
  -- Think about lir.nvim: https://github.com/tamago324/lir.nvim
  use { 'justinmk/vim-dirvish' }

  -- Main tree-view
  use {
    'nvim-tree/nvim-tree.lua',
    requires = { 'nvim-tree/nvim-web-devicons' },
    cmd = { "NvimTreeToggle", "NvimTreeFocus", "NvimTreeFindFileToggle" },
    config = function(p, a)
      -- Customize colors

      -- highlight NvimTreeSymlink guifg=blue gui=bold,underline
      -- highlight NvimTreeFolderName guifg=blue gui=bold,underline
      -- highlight NvimTreeEmptyFolderName guifg=blue gui=bold,underline
      -- highlight NvimTreeOpenedFolderName guifg=blue gui=bold,underline
      -- highlight NvimTreeRootFolder guifg=blue gui=bold,underline
      -- highlight NvimTreeExecFile guifg=blue gui=bold,underline

      require('nvim-tree').setup({
        -- Ensure compatibility with Dirvish
        hijack_netrw = false,
        hijack_directories = { enable = false },

        git = { enable = false },
        diagnostics = { enable = false },
        update_focused_file = { enable = true },
        update_cwd = true,
        trash = { cmd = "gio trash" },
        hijack_cursor = true,
        hijack_unnamed_buffer_when_opening = false,
        view = {
          hide_root_folder = true,
        },
        remove_keymaps = true,
        on_attach = mappings.nvim_tree,
        filters = {
          custom = {
            "^.git$"
          },
        },
        renderer = {
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
          },
          special_files = {},
        },
      })
    end,
  }
end
