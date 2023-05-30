return function(use)
  -- Main tree-view
  use {
    'nvim-tree/nvim-tree.lua',
    requires = { 'nvim-tree/nvim-web-devicons' },
    config = function(p, a)
      -- Customize colors

      -- highlight NvimTreeSymlink guifg=blue gui=bold,underline
      -- highlight NvimTreeFolderName guifg=blue gui=bold,underline
      -- highlight NvimTreeEmptyFolderName guifg=blue gui=bold,underline
      -- highlight NvimTreeOpenedFolderName guifg=blue gui=bold,underline
      -- highlight NvimTreeRootFolder guifg=blue gui=bold,underline
      -- highlight NvimTreeExecFile guifg=blue gui=bold,underline

      require('nvim-tree').setup({
        git = { enable = false },
        diagnostics = { enable = false },
        update_focused_file = { enable = true },
        sync_root_with_cwd = true,
        trash = { cmd = "trash" },
        hijack_cursor = true,
        hijack_unnamed_buffer_when_opening = true,
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
