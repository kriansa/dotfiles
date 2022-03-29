return function(use)
  -- Think about lir.nvim: https://github.com/tamago324/lir.nvim
  use { 'justinmk/vim-dirvish' }

  -- Main tree-view
  use {
    'kyazdani42/nvim-tree.lua',
    requires = { 'kyazdani42/nvim-web-devicons' },
    cmd = { "NvimTreeToggle", "NvimTreeFocus", "NvimTreeFindFileToggle" },
    config = function(p, a)
      -- These options are still on the old api and yet to be migrated to the new one
      vim.g.nvim_tree_git_hl = 0
      vim.g.nvim_tree_special_files = {}
      vim.g.nvim_tree_indent_markers = 0
      vim.g.nvim_tree_group_empty = 1
      vim.g.nvim_tree_create_in_closed_folder = 1
      vim.g.nvim_tree_show_icons = {
        git = 0,
        folders = 1,
        folder_arrows = 1,
        files = 0,
      }

      -- Customize colors
      -- highlight NvimTreeSymlink guifg=blue gui=bold,underline
      -- highlight NvimTreeFolderName guifg=blue gui=bold,underline
      -- highlight NvimTreeEmptyFolderName guifg=blue gui=bold,underline
      -- highlight NvimTreeOpenedFolderName guifg=blue gui=bold,underline
      -- highlight NvimTreeRootFolder guifg=blue gui=bold,underline
      -- highlight NvimTreeExecFile guifg=blue gui=bold,underline

      require('nvim-tree').setup({
        git = { enable = false },
        diagnostics = { enabled = false },
        hijack_directories = { enable = false },
        update_focused_file = { enable = true },
        trash = { cmd = "gio trash" },
        auto_close = false,
        hijack_cursor = true,
        hijack_netrw = false,
        hijack_unnamed_buffer_when_opening = false,
        view = {
          hide_root_folder = true,
          mappings = {
            custom_only = true,
            list = mappings.nvim_tree,
          },
        },
        filters = {
          custom = {
            ".git"
          },
        },
      })
    end,
  }
end
