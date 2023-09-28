return {
  -- Automatically set shiftwidt and expandtab
  { 'tpope/vim-sleuth' },

  -- Add file manipulation commands
  {
    'tpope/vim-eunuch',
    cmd = {
      "Delete", "Unlink", "Move", "Rename", "Chmod",
      "Mkdir", "Cfind", "Lfind", "Clocate", "Llocate",
      "SudoEdit", "SudoWrite", "Wall", "W"
    },
  },

  -- Make loading sessions seamless
  { "tpope/vim-obsession" },

  -- See contents of registers
  {
    'tversteeg/registers.nvim',
    config = function()
		  require("registers").setup()
	  end
  },

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
