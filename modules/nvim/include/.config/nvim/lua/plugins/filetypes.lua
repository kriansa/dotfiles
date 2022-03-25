return function(use)
  -- Speed up loading filetype.vim native
  use {
    "nathom/filetype.nvim",
    config = function()
      require("filetype").setup({
          overrides = {
            complex = {
              ["^Dockerfile-.*"] = "dockerfile",
            },
            literal = {
              Dangerfile = "ruby",
              [".nycrc"] = "javascript",
            },
          }
      })
    end
  }

  -- Markdown
  use {
    'iamcco/markdown-preview.nvim',
    ft = "markdown",
    run = 'cd app && yarn install',
  }

  -- CSS/HTML
  use {
    'norcalli/nvim-colorizer.lua',
    ft = { 'css', 'javascript', 'vim', 'html', 'vue', 'jsx', 'tsx' },
    config = function()
      require('colorizer').setup({'css', 'javascript', 'vim', 'html', 'vue', 'jsx', 'tsx'})
    end
  }
end
