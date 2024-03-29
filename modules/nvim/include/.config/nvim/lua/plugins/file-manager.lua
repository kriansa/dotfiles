return {
  -- Dirvish
  { 'justinmk/vim-dirvish' },

  -- Main tree-view
  {
    'nvim-tree/nvim-tree.lua',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
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
              folder = {
                arrow_open = "▾",
                arrow_closed = "▸",
              },
            },
          },
          special_files = {},
        },
      })
    end,
  },
}
