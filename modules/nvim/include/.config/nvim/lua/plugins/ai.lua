return {
  {
    "zbirenbaum/copilot.lua",
    config = function()
      require("copilot").setup({
        panel = { enabled = false },
        suggestion = { enabled = false },
        filetypes = {
          yaml = true,
          markdown = false,
          help = false,
          text = false,
          gitcommit = false,
          gitrebase = false,
          hgcommit = false,
          svn = false,
          cvs = false,
          ["."] = false,
          sh = function ()
            return not string.match(vim.fs.basename(vim.api.nvim_buf_get_name(0)), '^%.env.*')
          end,
        },
      })

      vim.api.nvim_create_user_command('CopilotToggle', function()
        local cmd = require("copilot.command")
        if require("copilot.client").is_disabled() then
          cmd.enable()
        else
          cmd.disable()
        end
      end, { desc = "Toggle Github Copilot" })

      -- Start disabled
      require("copilot.command").disable()
    end,
  }
}
