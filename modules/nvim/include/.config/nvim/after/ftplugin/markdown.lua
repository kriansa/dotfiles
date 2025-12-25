-- Enable line-wrap for markdown
vim.opt_local.wrap = true

-- Adds a surround for bold text in markdown
require("nvim-surround").buffer_setup({
  surrounds = {
    ["*"] = {
      add = { "**", "**" },
      find = "%*%*.-%*%*",
      delete = "^(%*%*?)().-(%*%*?)()$",
      change = {
        target = "^(%*%*?)().-(%*%*?)()$",
      },
    },
  },
})
