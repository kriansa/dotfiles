local map = function(mode, lhs, rhs, opts)
  vim.api.nvim_buf_set_keymap(0, mode, lhs, rhs, opts or {})
end

map('n', '<Tab>', '=', { silent = true })
map('n', 'q', 'gq', { silent = true })
