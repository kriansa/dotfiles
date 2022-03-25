-- My own plugins
require("custom_plugins.auto-session").setup()
require("custom_plugins.auto-save").setup({ enable = true })
require("custom_plugins.window-close").setup()
require("custom_plugins.yank-filename").setup()
require("custom_plugins.paste-no-yank").setup()
require("custom_plugins.better-qf").setup({
  min_height = 1,
  max_height = 10,
  mappings = mappings.quickfix,
})
