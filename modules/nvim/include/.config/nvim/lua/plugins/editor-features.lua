return {
  -- Add file manipulation commands
  { 'tpope/vim-eunuch' },

  -- Make loading sessions seamless
  { "tpope/vim-obsession" },

  -- Window management
  { 'simeji/winresizer' },
  { 'troydm/zoomwintab.vim' },

  -- bigfile adds a new filetype bigfile to Neovim that triggers when the file is larger than the
  -- configured size. This automatically prevents things like LSP and Treesitter attaching to the
  -- buffer.
  {
    "folke/snacks.nvim",
    opts = {
      bigfile = {},
    }
  },
}
