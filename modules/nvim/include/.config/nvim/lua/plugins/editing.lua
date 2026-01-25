return {
  -- surrounds
  {
    "kylechui/nvim-surround",
    config = function()
      require("nvim-surround").setup()
    end
  },

  -- Allow repeating supported plugin maps with "."
  -- { 'tpope/vim-repeat' },
  -- Automatically set shiftwidt and expandtab
  -- { 'tpope/vim-sleuth' },

  -- Use gS and gJ to break and join multiline statements
  { 'AndrewRadev/splitjoin.vim' },

  -- Move to two-character search patterns
  { "https://codeberg.org/andyg/leap.nvim" },

  {
    'andymass/vim-matchup',
    config = function()
      vim.g.matchup_surround_enabled = 1
      vim.g.matchup_override_vimtex = 1
      vim.g.matchup_transmute_enabled = 1
      vim.g.matchup_matchparen_deferred = 1
      vim.g.matchup_matchparen_deferred_show_delay = 100
      vim.g.matchup_matchparen_offscreen = { method = "status_manual" }

      -- Treesitter settings
      vim.g.matchup_treesitter_disable_virtual_text = true
    end
  },

  -- Commenter
  {
    'numToStr/Comment.nvim',
    config = function()
      require('Comment').setup()
    end
  },

  {
    'nvim-treesitter/nvim-treesitter',
    build = ':TSUpdate',
    config = function()
      require('nvim-treesitter.configs').setup({
        ensure_installed = {
          "awk", "bash", "c", "caddy", "comment", "cpp", "css", "csv", "diff", "dockerfile", "fish",
          "git_config", "git_rebase", "gitattributes", "gitcommit", "gitignore", "go", "gomod",
          "gosum", "graphql", "hcl", "html", "ini", "java", "javascript", "jq", "json", "jsonc",
          "json5", "kotlin", "latex", "lua", "luadoc", "make", "markdown", "markdown_inline",
          "passwd", "pem", "php", "promql", "python", "query", "ruby", "rbs", "regex", "rust",
          "scss", "sql", "toml", "tsx", "terraform", "typescript", "vim", "vimdoc", "vue", "yaml",
          "xml",
        },
        highlight = { enable = true, additional_vim_regex_highlighting = false },
        indent = { enable = true },
        incremental_selection = { enable = true },
        textobjects = { enable = true },
      })
    end
  },

  -- Extend textobjects
  -- This has integration with LSP: https://github.com/nvim-treesitter/nvim-treesitter-textobjects#textobjects-lsp-interop
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
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
  },

  -- Replace tpope's endwise
  -- See: https://github.com/nvim-treesitter/nvim-treesitter/issues/703
  {
    'RRethy/nvim-treesitter-endwise',
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
    event = "InsertEnter",
    config = function()
      require('nvim-treesitter.configs').setup {
        endwise = { enable = true },
      }
    end
  },

  {
    "windwp/nvim-autopairs",
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
    event = "InsertEnter",
    config = function()
      require('nvim-autopairs').setup({
        check_ts = true,
        enable_check_bracket_line = false,
      })
    end
  },

  -- Endwise for html, auto-close html tags
  {
    "windwp/nvim-ts-autotag",
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
    config = function()
      require('nvim-ts-autotag').setup({
        opts = {
          -- Defaults
          enable_close = true, -- Auto close tags
          enable_rename = true, -- Auto rename pairs of tags
          enable_close_on_slash = false -- Auto close on trailing </
        },
      })
    end
  },

  -- Indent guides
  {
    "folke/snacks.nvim",
    opts = {
      indent = {
        enabled = true,
        animate = {
          enabled = false,
        },
      },
    }
  },

  -- Markdown
  {
    'iamcco/markdown-preview.nvim',
    ft = "markdown",
    build = 'cd app && npm install',
  },

  {
    'MeanderingProgrammer/markdown.nvim',
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
    config = function()
      require('render-markdown').setup({
        file_types = { "markdown", "Avante" },
        heading = {
          icons = {},
        },
        sign = {
          enabled = false,
        },
        html = {
          comment = {
            conceal = false,
          },
        },
        code = {
          border = "thick",
        },
      })
    end,
  },

  -- CSS/HTML
  {
    'norcalli/nvim-colorizer.lua',
    ft = { 'css', 'javascript', 'vim', 'html', 'vue', 'jsx', 'tsx' },
    config = function()
      require('colorizer').setup({'css', 'javascript', 'vim', 'html', 'vue', 'jsx', 'tsx'})
    end
  },
}
