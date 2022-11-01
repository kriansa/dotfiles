return function(use)
  -- Packer can manage itself
  use { 'wbthomason/packer.nvim' }

  -- Speed up loading Lua modules in Neovim to improve startup time.
  use 'lewis6991/impatient.nvim'

  -- Fix CursorHold event (TODO: Remove on 0.8) - https://github.com/neovim/neovim/pull/20198
  -- See: https://github.com/neovim/neovim/issues/12587
  use 'antoinemadec/FixCursorHold.nvim'

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

  -- Add support to .editorconfig files from projects
  use "gpanders/editorconfig.nvim"

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
          options = {
            enabled = true,
            ruler = false,
            showcmd = false,
          },
          gitsigns = {
            enabled = true,
          },
        },
        -- Delay the execution of blankline toggling because it's buggy and somehow they don't apply
        -- instantaneously
        on_open = function()
          vim.defer_fn(function()
            require("indent_blankline.commands").disable()
          end, 100)
        end,
        on_close = function()
          vim.defer_fn(function()
            require("indent_blankline.commands").enable()
          end, 100)
        end,
      })
    end
  }
end
