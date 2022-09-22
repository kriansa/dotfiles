-- Enhances the experience with Quickfix/Locationlist
--
-- Mostly based on https://github.com/stevearc/qf_helper.nvim

local M = {
  settings = {
    min_height = 1,
    max_height = 10,
  }
}

local is_open = function(qftype)
  local winid
  if qftype == "l" then
    winid = vim.fn.getloclist(0, { winid = 0 }).winid
  else
    winid = vim.fn.getqflist({ winid = 0 }).winid
  end

  return winid ~= 0
end

local get_list = function(qftype)
  return qftype == "l" and vim.fn.getloclist(0) or vim.fn.getqflist()
end

local get_win_type = function(winid)
  winid = winid or vim.api.nvim_get_current_win()
  local info = vim.fn.getwininfo(winid)[1]

  if info.quickfix == 0 then
    return ""
  elseif info.loclist == 0 then
    return "c"
  else
    return "l"
  end
end

local get_active_list = function()
  local loclist = vim.fn.getloclist(0)

  -- If we're at the quickfix list, or if loclist is empty, or if it's open while loclist is closed
  -- then use quickfix list
  if get_win_type() == "c" or vim.tbl_isempty(loclist) or (is_open("c") and not is_open("l")) then
    return { qftype = "c", list = vim.fn.getqflist() }
  else
    return { qftype = "l", list = loclist }
  end
end

local get_pos = function(qftype)
  if qftype == "l" then
    return vim.fn.getloclist(0, { idx = 0 }).idx
  else
    return vim.fn.getqflist({ idx = 0 }).idx
  end
end

local set_list = function(qftype, items)
  if qftype == "l" then
    vim.fn.setloclist(0, items)
  else
    vim.fn.setqflist(items)
  end
end

local list_name = function(qftype)
  if qftype == "l" then
    return "Location list"
  else
    return "Quickfix"
  end
end

M.open = function(qftype, enter)
  enter = enter or false

  -- Focus if already open
  if is_open(qftype) then
    if enter and get_win_type() ~= qftype then
      vim.cmd(qftype .. "open")
    end
    return
  end

  local list = get_list(qftype)

  -- Don't open a blank list
  if #list == 0 then
    vim.api.nvim_echo({ { list_name(qftype) .. " is empty!", nil } }, false, {})
    return
  end

  local height = math.min(M.settings.max_height, math.max(M.settings.min_height, vim.tbl_count(list)))
  local winid = vim.api.nvim_get_current_win()
  local cmd = qftype .. "open " .. height

  if qftype == "c" then
    cmd = "botright " .. cmd
  end

  local ok, err = pcall(vim.cmd, cmd)
  if not ok then
    vim.api.nvim_err_writeln(err)
    return
  end

  -- Repeat the open command. First command will open and enter, but the height
  -- could be wrong b/c of autocmds. Second command will enforce the height.
  vim.cmd(cmd)
  if not enter then
    vim.api.nvim_set_current_win(winid)
  end
end

M.close = function(qftype)
  vim.cmd(qftype .. "close")
end

M.toggle = function(qftype, enter)
  if is_open(qftype) then
    M.close(qftype)
  else
    M.open(qftype, enter)
  end
end

M.toggle_all = function(enter)
  if is_open("l") then
    M.close("l")
  elseif is_open("c") then
    M.close("c")
  elseif #get_list('l') > 0 then
    M.open("l", enter)
  else
    M.open("c", enter)
  end
end

M.navigate = function(steps, opts)
  opts = vim.tbl_extend("keep", opts or {}, {
    qftype = nil, -- 'c', 'l' or nil
    bang = "",    -- use '!' to pass to cc/ll
  })

  local active_list
  if opts.qftype == nil then
    active_list = get_active_list()
  else
    active_list = { qftype = opts.qftype, list = get_list(opts.qftype) }
  end

  if vim.tbl_isempty(active_list.list) then
    return
  end

  local pos = get_pos(active_list.qftype) - 1 + steps
  pos = pos + 1
  local cmd = pos .. active_list.qftype .. active_list.qftype
  vim.cmd("silent! " .. cmd .. opts.bang)
  vim.cmd("normal! zv")

  -- Print out current position after jumping if quickfix isn't open
  if not is_open(active_list.qftype) then
    local newpos = get_pos(active_list.qftype)
    local text = active_list.list[newpos].text
    text = string.gsub(text, "^%s*", "")
    local line = string.format("(%d of %d) %s", newpos, #active_list.list, text)
    if string.find(vim.o.shortmess, "a") then
      local newline_idx = string.find(line, "\n")
      if newline_idx then
        line = string.sub(line, 1, newline_idx - 1)
      end
    end
    vim.api.nvim_echo({ { line, nil } }, false, {})
  end
end

M.delete_line = function(range_start, range_end)
  local qftype = get_win_type()
  if qftype == "" then
    vim.api.nvim_err_writeln("Can only use :DeleteLine inside a quickfix buffer")
    return
  end

  local list = get_list(qftype)
  local newlist = {}
  local cursor = vim.api.nvim_win_get_cursor(0)
  local cursor_line = cursor[1]

  for i, item in ipairs(list) do
    if i < range_start or i > range_end then
      table.insert(newlist, item)
    end
  end
  set_list(qftype, newlist)

  -- Close this list when empty
  if #newlist == 0 then
    M.close(qftype)
    return
  end

  -- Reset cursor position
  local delta = cursor[1] - range_start
  if delta > 0 then
    cursor_line = cursor[1] - delta
  end

  cursor_line = math.min(math.max(1, cursor_line), #newlist)
  vim.api.nvim_win_set_cursor(0, { cursor_line, cursor[2] })
end

-- Executed on every new qf buffer
M._set_qf_defaults = function()
  vim.cmd([[
    command! -buffer -range DeleteLine call luaeval("require('custom_plugins.better-qf').delete_line(unpack(_A))", [<line1>, <line2>])
    nnoremap <silent> <buffer> dd :DeleteLine<CR>
    vnoremap <silent> <buffer> d :DeleteLine<CR>
  ]])
end

-- Initialize the autocmds and configure the plugin
M.setup = function(opts)
  M.settings = vim.tbl_extend("keep", opts or {}, M.settings)
  vim.cmd([[
    " Navigate through qf
    command! -bang -count=1 QNext call Qf_navigate(1, expand('<bang>'), expand('<count>'), v:null)
    command! -bang -count=1 QPrev call Qf_navigate(-1, expand('<bang>'), expand('<count>'), v:null)
    command! -bang -count=1 QFNext call Qf_navigate(1, expand('<bang>'), expand('<count>'), 'c')
    command! -bang -count=1 QFPrev call Qf_navigate(-1, expand('<bang>'), expand('<count>'), 'c')
    command! -bang -count=1 LLNext call Qf_navigate(1, expand('<bang>'), expand('<count>'), 'l')
    command! -bang -count=1 LLPrev call Qf_navigate(-1, expand('<bang>'), expand('<count>'), 'l')

    " Open/close lists
    command! -bang QFOpen call luaeval("require('custom_plugins.better-qf').open('c', _A == '')", expand('<bang>'))
    command! -bang LLOpen call luaeval("require('custom_plugins.better-qf').open('l', _A == '')", expand('<bang>'))
    command! -bang QToggle call luaeval("require('custom_plugins.better-qf').toggle_all(_A == '')", expand('<bang>'))
    command! -bang QFToggle call luaeval("require('custom_plugins.better-qf').toggle('c', _A == '')", expand('<bang>'))
    command! -bang LLToggle call luaeval("require('custom_plugins.better-qf').toggle('l', _A == '')", expand('<bang>'))

    function! Qf_navigate(dir, bang, count, qftype) abort
      let l:opts = {
      \ 'bang': a:bang,
      \ 'qftype': a:qftype,
      \}
      call luaeval("require('custom_plugins.better-qf').navigate(_A[1], _A[2])", [a:dir*a:count, l:opts])
    endfunction

    augroup QFHelper
      autocmd!
      autocmd FileType qf lua require('custom_plugins.better-qf')._set_qf_defaults()
    augroup END
  ]])
end

return M
