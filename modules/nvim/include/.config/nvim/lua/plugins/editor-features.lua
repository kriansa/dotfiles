return {
  -- Automatically set shiftwidt and expandtab
  { 'tpope/vim-sleuth' },

  -- Add file manipulation commands
  { 'tpope/vim-eunuch' },

  -- Make loading sessions seamless
  { "tpope/vim-obsession" },

  -- Window management
  { 'mhinz/vim-sayonara' },
  { 'simeji/winresizer' },
  { 'troydm/zoomwintab.vim' },
  {
    "folke/zen-mode.nvim",
    config = function()
      require("zen-mode").setup({
        window = {
          options = {
            number = false,
            list = false,
          },
        },
        plugins = {
          gitsigns = { enabled = true },
          tmux = { enabled = false },
        },
        -- Disable indent-blankline.nvim
        on_open = function()
          require("ibl").update({ enabled = false })
        end,
        on_close = function()
          require("ibl").update({ enabled = true })
        end,
      })
    end
  },
}
