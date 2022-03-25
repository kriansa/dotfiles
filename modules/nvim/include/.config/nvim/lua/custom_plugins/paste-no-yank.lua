-- This plugin avoids yanking text to a register when you paste on insert mode

local M = {
  registers = { a = "", b = "" }
}

function M.restore_registers()
  vim.fn.setreg("+", M.registers.a)
  vim.fn.setreg("\"", M.registers.b)
end

function M.paste_over()
  M.registers.a = vim.fn.getreg("+")
  M.registers.b = vim.fn.getreg("\"")
  return "p:lua require('custom_plugins.paste-no-yank').restore_registers()\n"
end

function M.setup()
  vim.cmd([[
    function! PasteOver()
      return luaeval("require('custom_plugins.paste-no-yank').paste_over()")
    endfunction
    xnoremap <silent> <expr> p PasteOver()
  ]])
end

return M
