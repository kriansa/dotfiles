-- This plugin avoids yanking text to a register when you paste on insert mode

local M = {
  registers = { a = "" }
}

function M.restore_registers()
  vim.fn.setreg("\"", M.registers.a)
end

function M.paste_over(cmd)
  M.registers.a = vim.fn.getreg("\"")
  return cmd .. ":lua require('custom_plugins.paste-no-yank').restore_registers()\n"
end

function M.setup()
  vim.cmd([[
    function! PasteOver(cmd)
      return luaeval("require('custom_plugins.paste-no-yank').paste_over('" . a:cmd . "')")
    endfunction

    xnoremap <silent> <expr> p PasteOver("p")
    xnoremap <silent> <expr> P PasteOver("P")
  ]])
end

return M
