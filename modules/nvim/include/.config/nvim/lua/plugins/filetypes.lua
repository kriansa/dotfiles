return function(use)
  -- Add existing filetypes
  vim.filetype.add({
    extension = {
      jbuilder = "ruby",
    },
    filename = {
      Dangerfile = "ruby",
      [".env"] = "sh",
      [".nycrc"] = "javascript",
    },
    pattern = {
      ["^Dockerfile[-.].*"] = "dockerfile",
    },
  })

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
