return function(use)
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  -- Speed up loading Lua modules in Neovim to improve startup time.
  use 'lewis6991/impatient.nvim'

  -- Automatically set shiftwidt and expandtab
  use 'tpope/vim-sleuth'

  -- Add file manipulation commands
  use {
    'tpope/vim-eunuch',
    cmd = {
      "Delete", "Delete!", "Unlink", "Unlink!", "Move", "Move!", "Rename", "Rename!", "Chmod",
      "Mkdir", "Mkdir!", "Cfind", "Cfind!", "Lfind", "Lfind!", "Clocate", "Clocate!", "Llocate",
      "Llocate!", "SudoEdit", "SudoWrite", "Wall", "W"
    },
  }

  -- Make loading sessions seamless
  use "tpope/vim-obsession"

  -- See contents of registers
  use {
    'tversteeg/registers.nvim',
    config = function()
		  require("registers").setup()
	  end
  }

  -- Window management
  use 'mhinz/vim-sayonara'
  use 'simeji/winresizer'
  use 'troydm/zoomwintab.vim'
  use {
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
          tmux = { enabled = true },
        },
        -- Disable indent-blankline.nvim
        on_open = function()
          require("indent_blankline.commands").disable()
        end,
        on_close = function()
          require("indent_blankline.commands").enable()
        end,
      })
    end
  }
end
