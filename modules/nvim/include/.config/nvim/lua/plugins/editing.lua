return function(use)
  -- surrounds
  use {
    "kylechui/nvim-surround",
    config = function()
      require("nvim-surround").setup()
    end
  }
  use 'tpope/vim-repeat'

  -- Use gS and gJ to break and join multiline statements
  use 'AndrewRadev/splitjoin.vim'

  -- Move to two-character search patterns
  use {
    'ggandor/leap.nvim',
    config = function()
      require('leap').add_default_mappings()
    end
  }
  use {
    'ggandor/flit.nvim',
    requires = { "ggandor/leap.nvim" },
    config = function()
      require('flit').setup()
    end,
  }

  use {
    'andymass/vim-matchup',
    requires = { 'nvim-treesitter/nvim-treesitter' },
    config = function()
      require('nvim-treesitter.configs').setup({
        matchup = {
          enable = true,
          disable_virtual_text = true,
        },
      })

      vim.g.matchup_surround_enabled = 1
      vim.g.matchup_override_vimtex = 1
      vim.g.matchup_transmute_enabled = 1
      vim.g.matchup_matchparen_deferred = 1
      vim.g.matchup_matchparen_deferred_show_delay = 100
      vim.g.matchup_matchparen_offscreen = { method = "status_manual" }
    end
  }

  -- Commenter
  use {
    'numToStr/Comment.nvim',
    config = function()
      require('Comment').setup()
    end
  }

  use {
    'nvim-treesitter/nvim-treesitter',
    run = ':TSUpdate',
    config = function()
      require 'nvim-treesitter.configs'.setup({
        highlight = { enable = true },
        indent = { enable = true },
        incremental_selection = { enable = true },
        textobjects = { enable = true },
      })
    end
  }

  -- Extend textobjects
  -- This has integration with LSP: https://github.com/nvim-treesitter/nvim-treesitter-textobjects#textobjects-lsp-interop
  use {
    "nvim-treesitter/nvim-treesitter-textobjects",
    requires = { 'nvim-treesitter/nvim-treesitter' },
    config = function()
      require('nvim-treesitter.configs').setup({
        textobjects = {
          move = {
            enable = true,
            set_jumps = true,
            goto_next_start = {
              ["]m"] = "@function.outer",
              ["]]"] = "@class.outer",
            },
            goto_next_end = {
              ["]M"] = "@function.outer",
              ["]["] = "@class.outer",
            },
            goto_previous_start = {
              ["[m"] = "@function.outer",
              ["[["] = "@class.outer",
            },
            goto_previous_end = {
              ["[M"] = "@function.outer",
              ["[]"] = "@class.outer",
            },
          },

          select = {
            enable = true,

            -- Automatically jump forward to textobj, similar to targets.vim
            lookahead = true,

            keymaps = {
              -- You can use the capture groups defined in textobjects.scm
              ["am"] = "@function.outer",
              ["im"] = "@function.inner",
              ["ac"] = "@class.outer",
              ["ic"] = "@class.inner",
              ["ap"] = "@parameter.outer",
              ["ip"] = "@parameter.inner",
            },
          },
        },
      })
    end
  }

  -- Replace tpope's endwise
  -- See: https://github.com/nvim-treesitter/nvim-treesitter/issues/703
  use {
    'RRethy/nvim-treesitter-endwise',
    requires = { 'nvim-treesitter/nvim-treesitter' },
    config = function()
      require('nvim-treesitter.configs').setup {
        endwise = { enable = true },
      }
    end
  }

  -- This needs setup to work along with nvim-cmp
  use {
    "windwp/nvim-autopairs",
    requires = { 'nvim-treesitter/nvim-treesitter' },
    config = function()
      require('nvim-autopairs').setup({
        check_ts = true,
        enable_check_bracket_line = false,
      })
    end
  }

  -- Endwise for html, auto-close html tags
  use {
    "windwp/nvim-ts-autotag",
    requires = { 'nvim-treesitter/nvim-treesitter' },
    config = function()
      require('nvim-treesitter.configs').setup({
        autotag = {
          enable = true,
        }
      })
    end
  }

  -- Indentation lines
  use {
    "lukas-reineke/indent-blankline.nvim",
    requires = { 'nvim-treesitter/nvim-treesitter' },
    config = function()
      require("indent_blankline").setup({
        show_end_of_line = true,
        use_treesitter = true,
        space_char_highlight_list = { 'Whitespace' },
      })
    end
  }
end
