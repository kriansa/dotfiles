-- Automatically generated packer.nvim plugin loader code

if vim.api.nvim_call_function('has', {'nvim-0.5'}) ~= 1 then
  vim.api.nvim_command('echohl WarningMsg | echom "Invalid Neovim version for packer.nvim! | echohl None"')
  return
end

vim.api.nvim_command('packadd packer.nvim')

local no_errors, error_msg = pcall(function()

  local time
  local profile_info
  local should_profile = false
  if should_profile then
    local hrtime = vim.loop.hrtime
    profile_info = {}
    time = function(chunk, start)
      if start then
        profile_info[chunk] = hrtime()
      else
        profile_info[chunk] = (hrtime() - profile_info[chunk]) / 1e6
      end
    end
  else
    time = function(chunk, start) end
  end
  
local function save_profiles(threshold)
  local sorted_times = {}
  for chunk_name, time_taken in pairs(profile_info) do
    sorted_times[#sorted_times + 1] = {chunk_name, time_taken}
  end
  table.sort(sorted_times, function(a, b) return a[2] > b[2] end)
  local results = {}
  for i, elem in ipairs(sorted_times) do
    if not threshold or threshold and elem[2] > threshold then
      results[i] = elem[1] .. ' took ' .. elem[2] .. 'ms'
    end
  end

  _G._packer = _G._packer or {}
  _G._packer.profile_output = results
end

time([[Luarocks path setup]], true)
local package_path_str = "/home/dpereira/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?.lua;/home/dpereira/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?/init.lua;/home/dpereira/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?.lua;/home/dpereira/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/home/dpereira/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

time([[Luarocks path setup]], false)
time([[try_loadstring definition]], true)
local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s), name, _G.packer_plugins[name])
  if not success then
    vim.schedule(function()
      vim.api.nvim_notify('packer.nvim: Error running ' .. component .. ' for ' .. name .. ': ' .. result, vim.log.levels.ERROR, {})
    end)
  end
  return result
end

time([[try_loadstring definition]], false)
time([[Defining packer_plugins]], true)
_G.packer_plugins = {
  ["Comment.nvim"] = {
    config = { "\27LJ\2\n5\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\fComment\frequire\0" },
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/Comment.nvim",
    url = "https://github.com/numToStr/Comment.nvim"
  },
  ["FixCursorHold.nvim"] = {
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/FixCursorHold.nvim",
    url = "https://github.com/antoinemadec/FixCursorHold.nvim"
  },
  edge = {
    after = { "lualine.nvim" },
    loaded = true,
    only_config = true
  },
  ["editorconfig.nvim"] = {
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/editorconfig.nvim",
    url = "https://github.com/gpanders/editorconfig.nvim"
  },
  ["emmet-vim"] = {
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/emmet-vim",
    url = "https://github.com/mattn/emmet-vim"
  },
  ["filetype.nvim"] = {
    config = { "\27LJ\2\n�\1\0\0\5\0\n\0\r6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\b\0005\3\4\0005\4\3\0=\4\5\0035\4\6\0=\4\a\3=\3\t\2B\0\2\1K\0\1\0\14overrides\1\0\0\fliteral\1\0\2\v.nycrc\15javascript\15Dangerfile\truby\fcomplex\1\0\0\1\0\1\19^Dockerfile-.*\15dockerfile\nsetup\rfiletype\frequire\0" },
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/filetype.nvim",
    url = "https://github.com/nathom/filetype.nvim"
  },
  ["gitsigns.nvim"] = {
    config = { "\27LJ\2\n�\3\0\0\5\0\18\0\0226\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\14\0005\3\4\0005\4\3\0=\4\5\0035\4\6\0=\4\a\0035\4\b\0=\4\t\0035\4\n\0=\4\v\0035\4\f\0=\4\r\3=\3\15\0026\3\16\0009\3\1\3=\3\17\2B\0\2\1K\0\1\0\14on_attach\rmappings\nsigns\1\0\0\17changedelete\1\0\3\ahl\fRedSign\nnumhl\21GitSignsChangeNr\ttext\6~\14topdelete\1\0\3\ahl\fRedSign\nnumhl\21GitSignsDeleteNr\ttext\b‾\vdelete\1\0\3\ahl\fRedSign\nnumhl\21GitSignsDeleteNr\ttext\6_\vchange\1\0\3\ahl\rBlueSign\nnumhl\21GitSignsChangeNr\ttext\6~\badd\1\0\0\1\0\3\ahl\14GreenSign\nnumhl\18GitSignsAddNr\ttext\6+\nsetup\rgitsigns\frequire\0" },
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/gitsigns.nvim",
    url = "https://github.com/lewis6991/gitsigns.nvim"
  },
  ["impatient.nvim"] = {
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/impatient.nvim",
    url = "https://github.com/lewis6991/impatient.nvim"
  },
  ["indent-blankline.nvim"] = {
    config = { "\27LJ\2\n�\1\0\0\4\0\6\0\t6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\3\0005\3\4\0=\3\5\2B\0\2\1K\0\1\0\30space_char_highlight_list\1\2\0\0\15Whitespace\1\0\2\19use_treesitter\2\21show_end_of_line\2\nsetup\21indent_blankline\frequire\0" },
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/indent-blankline.nvim",
    url = "https://github.com/lukas-reineke/indent-blankline.nvim"
  },
  ["lightspeed.nvim"] = {
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/lightspeed.nvim",
    url = "https://github.com/ggandor/lightspeed.nvim"
  },
  ["lualine.nvim"] = {
    config = { "\27LJ\2\n�\1\0\1\3\0\1\0\0065\1\0\0008\2\0\1\14\0\2\0X\3\1�\18\2\0\0L\2\2\0\1\0\17\fCOMMAND\6C\14O-PENDING\bO-P\nSHELL\aSH\vS-LINE\bS-L\fCONFIRM\fCONFIRM\tMORE\tMORE\vVISUAL\6V\14V-REPLACE\bV-R\vV-LINE\bV-L\vINSERT\6I\vNORMAL\6N\rTERMINAL\tTERM\aEX\aEX\fREPLACE\6R\fV-BLOCK\bV-B\fS-BLOCK\bS-B\vSELECT\6S�\1\0\0\6\0\n\0\0176\0\0\0'\2\1\0B\0\2\2\18\3\0\0009\1\2\0006\4\3\0009\4\4\0049\4\5\0049\4\6\4B\1\3\2\18\3\1\0009\1\a\0016\4\3\0009\4\b\0049\4\t\4B\4\1\0C\1\1\0\vgetcwd\afn\18make_relative\t_dir\fdirvish\6b\bvim\bnew\17plenary.path\frequire�\1\0\0\2\0\b\0 5\0\4\0006\1\0\0009\1\1\0019\1\2\1\15\0\1\0X\2\4�6\1\0\0009\1\1\0019\1\2\0019\1\3\1=\1\3\0006\1\0\0009\1\1\0019\1\2\1\15\0\1\0X\2\4�6\1\0\0009\1\1\0019\1\2\0019\1\5\1=\1\6\0006\1\0\0009\1\1\0019\1\2\1\15\0\1\0X\2\4�6\1\0\0009\1\1\0019\1\2\0019\1\a\1=\1\a\0L\0\2\0\fremoved\rmodified\fchanged\1\0\0\nadded\25gitsigns_status_dict\6b\bvimC\0\0\1\0\5\1\t6\0\0\0009\0\1\0009\0\2\0\t\0\0\0X\0\2�'\0\3\0L\0\2\0'\0\4\0L\0\2\0\5\t⮻ \15zoomwintab\6t\bvim\2�\6\1\0\v\0,\0E3\0\0\0003\1\1\0005\2\6\0005\3\2\0004\4\3\0>\1\1\4=\4\3\0035\4\4\0=\4\5\3=\3\a\0025\3\b\0=\3\t\0023\3\n\0003\4\v\0006\5\f\0'\a\r\0B\5\2\0029\5\14\0055\a\21\0005\b\15\0005\t\16\0=\t\17\b5\t\18\0=\t\19\b4\t\0\0=\t\20\b=\b\22\a5\b\25\0004\t\3\0005\n\23\0=\0\24\n>\n\1\t>\4\2\t=\t\3\b5\t\26\0005\n\27\0=\3\28\n>\n\2\t=\t\29\b5\t\30\0=\t\31\b5\t \0=\t!\b5\t\"\0=\t\5\b5\t#\0=\t$\b=\b\a\a5\b%\0004\t\0\0=\t\3\b4\t\0\0=\t\29\b5\t&\0=\t\31\b5\t'\0=\t!\b4\t\0\0=\t\5\b4\t\0\0=\t$\b=\b(\a4\b\0\0=\b)\a5\b*\0>\2\4\b=\b+\aB\5\2\1K\0\1\0\15extensions\1\4\0\0\14nvim-tree\rquickfix\rfugitive\ftabline\22inactive_sections\1\2\0\0\16bo:filetype\1\2\0\0\rfilename\1\0\0\14lualine_z\1\2\0\0\16diagnostics\1\2\0\0\16bo:filetype\14lualine_x\1\2\0\0\rencoding\14lualine_c\1\2\0\0\rfilename\14lualine_b\vsource\1\2\0\0\tdiff\1\2\0\0\vbranch\1\0\0\bfmt\1\2\0\0\tmode\foptions\1\0\0\23disabled_filetypes\23section_separators\1\0\2\nright\b\tleft\b\25component_separators\1\0\2\nright\b\tleft\b\1\0\4\25always_divide_middle\2\tpath\3\1\18icons_enabled\2\ntheme\tauto\nsetup\flualine\frequire\0\0\14filetypes\1\2\0\0\fdirvish\rsections\1\0\0\14lualine_y\1\2\0\0\16bo:filetype\14lualine_a\1\0\0\0\0\0" },
    load_after = {},
    loaded = true,
    needs_bufread = false,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/opt/lualine.nvim",
    url = "https://github.com/nvim-lualine/lualine.nvim"
  },
  ["markdown-preview.nvim"] = {
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/opt/markdown-preview.nvim",
    url = "https://github.com/iamcco/markdown-preview.nvim"
  },
  neogit = {
    commands = { "Neogit" },
    loaded = false,
    needs_bufread = true,
    only_cond = false,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/opt/neogit",
    url = "https://github.com/TimUntersberger/neogit"
  },
  ["nvim-autopairs"] = {
    config = { "\27LJ\2\nh\0\0\3\0\4\0\a6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\3\0B\0\2\1K\0\1\0\1\0\2\rcheck_ts\2\30enable_check_bracket_line\1\nsetup\19nvim-autopairs\frequire\0" },
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/nvim-autopairs",
    url = "https://github.com/windwp/nvim-autopairs"
  },
  ["nvim-colorizer.lua"] = {
    config = { "\27LJ\2\nc\0\0\3\0\4\0\a6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\3\0B\0\2\1K\0\1\0\1\b\0\0\bcss\15javascript\bvim\thtml\bvue\bjsx\btsx\nsetup\14colorizer\frequire\0" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/opt/nvim-colorizer.lua",
    url = "https://github.com/norcalli/nvim-colorizer.lua"
  },
  ["nvim-lspconfig"] = {
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/nvim-lspconfig",
    url = "https://github.com/neovim/nvim-lspconfig"
  },
  ["nvim-pqf"] = {
    config = { "\27LJ\2\ni\0\0\4\0\6\0\t6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0005\3\3\0=\3\5\2B\0\2\1K\0\1\0\nsigns\1\0\0\1\0\4\fwarning\6W\tinfo\6I\thint\6H\nerror\6E\nsetup\bpqf\frequire\0" },
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/nvim-pqf",
    url = "https://gitlab.com/yorickpeterse/nvim-pqf"
  },
  ["nvim-tree.lua"] = {
    commands = { "NvimTreeToggle", "NvimTreeFocus", "NvimTreeFindFileToggle" },
    config = { "\27LJ\2\n�\5\0\2\b\0\29\00006\2\0\0009\2\1\2)\3\0\0=\3\2\0026\2\0\0009\2\1\0024\3\0\0=\3\3\0026\2\0\0009\2\1\2)\3\0\0=\3\4\0026\2\0\0009\2\1\2)\3\1\0=\3\5\0026\2\0\0009\2\1\2)\3\1\0=\3\6\0026\2\0\0009\2\1\0025\3\b\0=\3\a\0026\2\t\0'\4\n\0B\2\2\0029\2\v\0025\4\r\0005\5\f\0=\5\14\0045\5\15\0=\5\16\0045\5\17\0=\5\18\0045\5\19\0=\5\20\0045\5\21\0=\5\22\0045\5\27\0005\6\23\0006\a\24\0009\a\25\a=\a\26\6=\6\24\5=\5\28\4B\2\2\1K\0\1\0\tview\1\0\0\tlist\14nvim_tree\rmappings\1\0\1\16custom_only\2\ntrash\1\0\1\bcmd\14gio trash\24update_focused_file\1\0\1\venable\2\23hijack_directories\1\0\1\venable\1\16diagnostics\1\0\1\fenabled\1\bgit\1\0\4'hijack_unnamed_buffer_when_opening\1\17hijack_netrw\1\18hijack_cursor\2\15auto_close\1\1\0\1\venable\1\nsetup\14nvim-tree\frequire\1\0\4\ffolders\3\1\bgit\3\0\nfiles\3\0\18folder_arrows\3\1\25nvim_tree_show_icons&nvim_tree_create_in_closed_folder\26nvim_tree_group_empty\29nvim_tree_indent_markers\28nvim_tree_special_files\21nvim_tree_git_hl\6g\bvim\0" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/opt/nvim-tree.lua",
    url = "https://github.com/kyazdani42/nvim-tree.lua"
  },
  ["nvim-treesitter"] = {
    config = { "\27LJ\2\n�\1\0\0\4\0\f\0\0156\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0005\3\3\0=\3\5\0025\3\6\0=\3\a\0025\3\b\0=\3\t\0025\3\n\0=\3\v\2B\0\2\1K\0\1\0\16textobjects\1\0\1\venable\2\26incremental_selection\1\0\1\venable\2\vindent\1\0\1\venable\2\14highlight\1\0\0\1\0\1\venable\2\nsetup\28nvim-treesitter.configs\frequire\0" },
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/nvim-treesitter",
    url = "https://github.com/nvim-treesitter/nvim-treesitter"
  },
  ["nvim-treesitter-endwise"] = {
    config = { "\27LJ\2\ng\0\0\4\0\6\0\t6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0005\3\3\0=\3\5\2B\0\2\1K\0\1\0\fendwise\1\0\0\1\0\1\venable\2\nsetup\28nvim-treesitter.configs\frequire\0" },
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/nvim-treesitter-endwise",
    url = "https://github.com/RRethy/nvim-treesitter-endwise"
  },
  ["nvim-treesitter-textobjects"] = {
    config = { "\27LJ\2\n�\4\0\0\6\0\20\0\0236\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\18\0005\3\f\0005\4\3\0005\5\4\0=\5\5\0045\5\6\0=\5\a\0045\5\b\0=\5\t\0045\5\n\0=\5\v\4=\4\r\0035\4\14\0005\5\15\0=\5\16\4=\4\17\3=\3\19\2B\0\2\1K\0\1\0\16textobjects\1\0\0\vselect\fkeymaps\1\0\6\aip\21@parameter.inner\aim\20@function.inner\aap\21@parameter.outer\aac\17@class.outer\aic\17@class.inner\aam\20@function.outer\1\0\2\venable\2\14lookahead\2\tmove\1\0\0\22goto_previous_end\1\0\2\a[]\17@class.outer\a[M\20@function.outer\24goto_previous_start\1\0\2\a[m\20@function.outer\a[[\17@class.outer\18goto_next_end\1\0\2\a][\17@class.outer\a]M\20@function.outer\20goto_next_start\1\0\2\a]m\20@function.outer\a]]\17@class.outer\1\0\2\venable\2\14set_jumps\2\nsetup\28nvim-treesitter.configs\frequire\0" },
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/nvim-treesitter-textobjects",
    url = "https://github.com/nvim-treesitter/nvim-treesitter-textobjects"
  },
  ["nvim-ts-autotag"] = {
    config = { "\27LJ\2\ng\0\0\4\0\6\0\t6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0005\3\3\0=\3\5\2B\0\2\1K\0\1\0\fautotag\1\0\0\1\0\1\venable\2\nsetup\28nvim-treesitter.configs\frequire\0" },
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/nvim-ts-autotag",
    url = "https://github.com/windwp/nvim-ts-autotag"
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/nvim-web-devicons",
    url = "https://github.com/kyazdani42/nvim-web-devicons"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/packer.nvim",
    url = "https://github.com/wbthomason/packer.nvim"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/plenary.nvim",
    url = "https://github.com/nvim-lua/plenary.nvim"
  },
  ["registers.nvim"] = {
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/registers.nvim",
    url = "https://github.com/tversteeg/registers.nvim"
  },
  ["splitjoin.vim"] = {
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/splitjoin.vim",
    url = "https://github.com/AndrewRadev/splitjoin.vim"
  },
  ["sqlite.lua"] = {
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/sqlite.lua",
    url = "https://github.com/tami5/sqlite.lua"
  },
  ["telescope-frecency.nvim"] = {
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/telescope-frecency.nvim",
    url = "https://github.com/nvim-telescope/telescope-frecency.nvim"
  },
  ["telescope-fzf-native.nvim"] = {
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/telescope-fzf-native.nvim",
    url = "https://github.com/nvim-telescope/telescope-fzf-native.nvim"
  },
  ["telescope.nvim"] = {
    config = { "\27LJ\2\n�\1\0\1\6\1\b\0\0166\1\0\0'\3\1\0B\1\2\0029\1\2\0015\3\6\0006\4\3\0009\4\4\0049\4\5\4B\4\1\2=\4\5\0039\4\a\0B\1\3\2-\2\0\0004\4\3\0>\1\1\4D\2\2\0\0\0\rfilename\1\0\0\bcwd\tloop\bvim\19transform_path\20telescope.utils\frequire�\2\1\1\a\1\19\0&9\1\0\0009\1\1\1\6\1\2\0X\1\4�9\1\0\0009\1\1\1\14\0\1\0X\2\1�'\1\3\0006\2\4\0'\4\5\0B\2\2\2\18\4\2\0009\2\6\2\18\5\1\0B\2\3\2\18\4\2\0009\2\a\0026\5\b\0009\5\t\0059\5\n\5B\5\1\0A\2\1\2\18\1\2\0005\2\v\0=\1\f\0029\3\r\0'\4\14\0\18\5\1\0&\3\5\3=\3\15\0029\3\r\0=\3\r\0023\3\16\0=\3\17\2=\1\18\0022\0\0�L\2\2\0\0�\rfilename\fdisplay\0\fordinal\b : \nbufnr\nvalue\1\0\1\nvalid\2\bcwd\tloop\bvim\14normalize\bnew\17plenary.path\frequire\14[No Name]\5\tname\tinfo�\1\1\0\5\0\a\0\r6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\3\0004\3\3\0005\4\4\0>\4\1\3=\3\5\2B\0\2\0023\1\6\0002\0\0�L\1\2\0\0\nitems\1\0\1\14remaining\2\1\0\1\14separator\6 \vcreate$telescope.pickers.entry_display\frequire�\r\1\0\b\0003\0J3\0\0\0006\1\1\0'\3\2\0B\1\2\0029\1\3\0015\3\f\0005\4\4\0005\5\5\0=\5\6\0045\5\t\0006\6\a\0009\6\b\6B\6\1\2=\6\n\5=\5\v\4=\4\r\0035\4\27\0005\5\14\0005\6\17\0005\a\15\0>\a\1\0065\a\16\0=\a\18\0065\a\19\0=\a\20\0065\a\21\0=\a\22\6=\6\23\5\18\6\0\0B\6\1\2=\6\24\0055\6\26\0006\a\a\0009\a\25\a=\a\n\6=\6\a\5=\5\28\0045\5\30\0005\6\29\0=\6\23\0055\6 \0005\a\31\0=\a!\6=\6\"\0055\6#\0=\6$\5=\5%\0045\5'\0005\6&\0=\6\23\0055\6(\0=\6)\5=\5*\0045\5,\0005\6+\0=\6\23\0055\6-\0=\6)\5=\5.\4=\4/\3B\1\2\0016\1\1\0'\3\2\0B\1\2\0029\0010\1'\0031\0B\1\2\0016\1\1\0'\3\2\0B\1\2\0029\0010\1'\0032\0B\1\2\1K\0\1\0\rfrecency\bfzf\19load_extension\fpickers\16grep_string\1\t\0\0\arg\18--color=never\17--no-heading\r--hidden\r--column\17--smart-case\20--with-filename\18--line-number\1\0\5\18prompt_prefix\16Filter ❯ \18results_title\5\18preview_title\5\24disable_coordinates\2\21disable_devicons\2\1\t\0\0\b─\b│\b─\b│\b┌\b┐\b┘\b└\14live_grep\22vimgrep_arguments\1\t\0\0\arg\18--color=never\17--no-heading\r--hidden\r--column\17--smart-case\20--with-filename\18--line-number\1\0\6\18prompt_prefix\16Regexp ❯ \18results_title\5\21disable_devicons\2\18preview_title\5\24disable_coordinates\2\17prompt_title\5\1\t\0\0\b─\b│\b─\b│\b┌\b┐\b┘\b└\15find_files\17find_command\1\6\0\0\arg\f--files\r--hidden\18--color=never\17--no-heading\18layout_config\vheight\1\0\0\1\0\1\fpadding\3\1\1\0\a\18prompt_prefix\15Files ❯ \18results_title\5\17prompt_title\5\21sorting_strategy\15descending\20layout_strategy\rvertical\18preview_title\5\21disable_devicons\2\1\t\0\0\b─\b│\b─\b│\b┌\b┐\b┘\b└\fbuffers\1\0\0\1\0\0\22telescope_buffers\16entry_maker\16borderchars\fpreview\1\t\0\0\b─\b│\b─\b│\b┌\b┐\b┘\b└\fresults\1\t\0\0\b─\b│\b─\b│\b├\b┤\b┘\b└\vprompt\1\0\0\1\t\0\0\b─\b│\6 \b│\b┌\b┐\b│\b│\1\t\0\0\b─\b│\b─\b│\b┌\b┐\b┘\b└\1\0\b\18prompt_prefix\17Buffers ❯ \21disable_devicons\2\rsort_mru\2\18sort_lastused\2\26ignore_current_buffer\2\14previewer\1\17prompt_title\5\ntheme\rdropdown\rdefaults\1\0\0\21default_mappings\6i\1\0\0\23telescope_defaults\rmappings\17path_display\1\2\0\0\rtruncate\1\0\3\18prompt_prefix\t❯ \20scroll_strategy\ncycle\20selection_caret\a> \nsetup\14telescope\frequire\0\0" },
    load_after = {},
    loaded = true,
    needs_bufread = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/opt/telescope.nvim",
    url = "https://github.com/nvim-telescope/telescope.nvim"
  },
  ["vim-dirvish"] = {
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/vim-dirvish",
    url = "https://github.com/justinmk/vim-dirvish"
  },
  ["vim-eunuch"] = {
    commands = { "Delete", "Delete!", "Unlink", "Unlink!", "Move", "Move!", "Rename", "Rename!", "Chmod", "Mkdir", "Mkdir!", "Cfind", "Cfind!", "Lfind", "Lfind!", "Clocate", "Clocate!", "Llocate", "Llocate!", "SudoEdit", "SudoWrite", "Wall", "W" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/opt/vim-eunuch",
    url = "https://github.com/tpope/vim-eunuch"
  },
  ["vim-matchup"] = {
    config = { "\27LJ\2\n�\2\0\0\4\0\14\0\0296\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0005\3\3\0=\3\5\2B\0\2\0016\0\6\0009\0\a\0)\1\1\0=\1\b\0006\0\6\0009\0\a\0)\1\1\0=\1\t\0006\0\6\0009\0\a\0)\1d\0=\1\n\0006\0\6\0009\0\a\0)\1\1\0=\1\v\0006\0\6\0009\0\a\0005\1\r\0=\1\f\0K\0\1\0\1\0\1\vmethod\18status_manual!matchup_matchparen_offscreen\28matchup_override_vimtex+matchup_matchparen_deferred_show_delay matchup_matchparen_deferred\29matchup_surround_enabled\6g\bvim\fmatchup\1\0\0\1\0\2\venable\2\25disable_virtual_text\2\nsetup\28nvim-treesitter.configs\frequire\0" },
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/vim-matchup",
    url = "https://github.com/andymass/vim-matchup"
  },
  ["vim-obsession"] = {
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/vim-obsession",
    url = "https://github.com/tpope/vim-obsession"
  },
  ["vim-repeat"] = {
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/vim-repeat",
    url = "https://github.com/tpope/vim-repeat"
  },
  ["vim-sayonara"] = {
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/vim-sayonara",
    url = "https://github.com/mhinz/vim-sayonara"
  },
  ["vim-surround"] = {
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/vim-surround",
    url = "https://github.com/tpope/vim-surround"
  },
  winresizer = {
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/winresizer",
    url = "https://github.com/simeji/winresizer"
  },
  ["zen-mode.nvim"] = {
    config = { "\27LJ\2\nI\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\fdisable\30indent_blankline.commands\frequire-\1\0\4\0\3\0\0066\0\0\0009\0\1\0003\2\2\0)\3d\0B\0\3\1K\0\1\0\0\rdefer_fn\bvimH\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\venable\30indent_blankline.commands\frequire-\1\0\4\0\3\0\0066\0\0\0009\0\1\0003\2\2\0)\3d\0B\0\3\1K\0\1\0\0\rdefer_fn\bvim�\1\1\0\5\0\17\0\0216\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\6\0005\3\4\0005\4\3\0=\4\5\3=\3\a\0025\3\t\0005\4\b\0=\4\5\0035\4\n\0=\4\v\3=\3\f\0023\3\r\0=\3\14\0023\3\15\0=\3\16\2B\0\2\1K\0\1\0\ron_close\0\fon_open\0\fplugins\rgitsigns\1\0\1\fenabled\2\1\0\0\1\0\3\nruler\1\fshowcmd\1\fenabled\2\vwindow\1\0\0\foptions\1\0\0\1\0\2\vnumber\1\tlist\1\nsetup\rzen-mode\frequire\0" },
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/zen-mode.nvim",
    url = "https://github.com/folke/zen-mode.nvim"
  },
  ["zoomwintab.vim"] = {
    loaded = true,
    path = "/home/dpereira/.local/share/nvim/site/pack/packer/start/zoomwintab.vim",
    url = "https://github.com/troydm/zoomwintab.vim"
  }
}

time([[Defining packer_plugins]], false)
-- Config for: nvim-treesitter-textobjects
time([[Config for nvim-treesitter-textobjects]], true)
try_loadstring("\27LJ\2\n�\4\0\0\6\0\20\0\0236\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\18\0005\3\f\0005\4\3\0005\5\4\0=\5\5\0045\5\6\0=\5\a\0045\5\b\0=\5\t\0045\5\n\0=\5\v\4=\4\r\0035\4\14\0005\5\15\0=\5\16\4=\4\17\3=\3\19\2B\0\2\1K\0\1\0\16textobjects\1\0\0\vselect\fkeymaps\1\0\6\aip\21@parameter.inner\aim\20@function.inner\aap\21@parameter.outer\aac\17@class.outer\aic\17@class.inner\aam\20@function.outer\1\0\2\venable\2\14lookahead\2\tmove\1\0\0\22goto_previous_end\1\0\2\a[]\17@class.outer\a[M\20@function.outer\24goto_previous_start\1\0\2\a[m\20@function.outer\a[[\17@class.outer\18goto_next_end\1\0\2\a][\17@class.outer\a]M\20@function.outer\20goto_next_start\1\0\2\a]m\20@function.outer\a]]\17@class.outer\1\0\2\venable\2\14set_jumps\2\nsetup\28nvim-treesitter.configs\frequire\0", "config", "nvim-treesitter-textobjects")
time([[Config for nvim-treesitter-textobjects]], false)
-- Config for: nvim-treesitter-endwise
time([[Config for nvim-treesitter-endwise]], true)
try_loadstring("\27LJ\2\ng\0\0\4\0\6\0\t6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0005\3\3\0=\3\5\2B\0\2\1K\0\1\0\fendwise\1\0\0\1\0\1\venable\2\nsetup\28nvim-treesitter.configs\frequire\0", "config", "nvim-treesitter-endwise")
time([[Config for nvim-treesitter-endwise]], false)
-- Config for: zen-mode.nvim
time([[Config for zen-mode.nvim]], true)
try_loadstring("\27LJ\2\nI\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\fdisable\30indent_blankline.commands\frequire-\1\0\4\0\3\0\0066\0\0\0009\0\1\0003\2\2\0)\3d\0B\0\3\1K\0\1\0\0\rdefer_fn\bvimH\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\venable\30indent_blankline.commands\frequire-\1\0\4\0\3\0\0066\0\0\0009\0\1\0003\2\2\0)\3d\0B\0\3\1K\0\1\0\0\rdefer_fn\bvim�\1\1\0\5\0\17\0\0216\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\6\0005\3\4\0005\4\3\0=\4\5\3=\3\a\0025\3\t\0005\4\b\0=\4\5\0035\4\n\0=\4\v\3=\3\f\0023\3\r\0=\3\14\0023\3\15\0=\3\16\2B\0\2\1K\0\1\0\ron_close\0\fon_open\0\fplugins\rgitsigns\1\0\1\fenabled\2\1\0\0\1\0\3\nruler\1\fshowcmd\1\fenabled\2\vwindow\1\0\0\foptions\1\0\0\1\0\2\vnumber\1\tlist\1\nsetup\rzen-mode\frequire\0", "config", "zen-mode.nvim")
time([[Config for zen-mode.nvim]], false)
-- Config for: nvim-treesitter
time([[Config for nvim-treesitter]], true)
try_loadstring("\27LJ\2\n�\1\0\0\4\0\f\0\0156\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0005\3\3\0=\3\5\0025\3\6\0=\3\a\0025\3\b\0=\3\t\0025\3\n\0=\3\v\2B\0\2\1K\0\1\0\16textobjects\1\0\1\venable\2\26incremental_selection\1\0\1\venable\2\vindent\1\0\1\venable\2\14highlight\1\0\0\1\0\1\venable\2\nsetup\28nvim-treesitter.configs\frequire\0", "config", "nvim-treesitter")
time([[Config for nvim-treesitter]], false)
-- Config for: filetype.nvim
time([[Config for filetype.nvim]], true)
try_loadstring("\27LJ\2\n�\1\0\0\5\0\n\0\r6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\b\0005\3\4\0005\4\3\0=\4\5\0035\4\6\0=\4\a\3=\3\t\2B\0\2\1K\0\1\0\14overrides\1\0\0\fliteral\1\0\2\v.nycrc\15javascript\15Dangerfile\truby\fcomplex\1\0\0\1\0\1\19^Dockerfile-.*\15dockerfile\nsetup\rfiletype\frequire\0", "config", "filetype.nvim")
time([[Config for filetype.nvim]], false)
-- Config for: Comment.nvim
time([[Config for Comment.nvim]], true)
try_loadstring("\27LJ\2\n5\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\fComment\frequire\0", "config", "Comment.nvim")
time([[Config for Comment.nvim]], false)
-- Config for: indent-blankline.nvim
time([[Config for indent-blankline.nvim]], true)
try_loadstring("\27LJ\2\n�\1\0\0\4\0\6\0\t6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\3\0005\3\4\0=\3\5\2B\0\2\1K\0\1\0\30space_char_highlight_list\1\2\0\0\15Whitespace\1\0\2\19use_treesitter\2\21show_end_of_line\2\nsetup\21indent_blankline\frequire\0", "config", "indent-blankline.nvim")
time([[Config for indent-blankline.nvim]], false)
-- Config for: nvim-autopairs
time([[Config for nvim-autopairs]], true)
try_loadstring("\27LJ\2\nh\0\0\3\0\4\0\a6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\3\0B\0\2\1K\0\1\0\1\0\2\rcheck_ts\2\30enable_check_bracket_line\1\nsetup\19nvim-autopairs\frequire\0", "config", "nvim-autopairs")
time([[Config for nvim-autopairs]], false)
-- Config for: gitsigns.nvim
time([[Config for gitsigns.nvim]], true)
try_loadstring("\27LJ\2\n�\3\0\0\5\0\18\0\0226\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\14\0005\3\4\0005\4\3\0=\4\5\0035\4\6\0=\4\a\0035\4\b\0=\4\t\0035\4\n\0=\4\v\0035\4\f\0=\4\r\3=\3\15\0026\3\16\0009\3\1\3=\3\17\2B\0\2\1K\0\1\0\14on_attach\rmappings\nsigns\1\0\0\17changedelete\1\0\3\ahl\fRedSign\nnumhl\21GitSignsChangeNr\ttext\6~\14topdelete\1\0\3\ahl\fRedSign\nnumhl\21GitSignsDeleteNr\ttext\b‾\vdelete\1\0\3\ahl\fRedSign\nnumhl\21GitSignsDeleteNr\ttext\6_\vchange\1\0\3\ahl\rBlueSign\nnumhl\21GitSignsChangeNr\ttext\6~\badd\1\0\0\1\0\3\ahl\14GreenSign\nnumhl\18GitSignsAddNr\ttext\6+\nsetup\rgitsigns\frequire\0", "config", "gitsigns.nvim")
time([[Config for gitsigns.nvim]], false)
-- Config for: nvim-pqf
time([[Config for nvim-pqf]], true)
try_loadstring("\27LJ\2\ni\0\0\4\0\6\0\t6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0005\3\3\0=\3\5\2B\0\2\1K\0\1\0\nsigns\1\0\0\1\0\4\fwarning\6W\tinfo\6I\thint\6H\nerror\6E\nsetup\bpqf\frequire\0", "config", "nvim-pqf")
time([[Config for nvim-pqf]], false)
-- Config for: edge
time([[Config for edge]], true)
try_loadstring("\27LJ\2\n�\f\0\0\3\0\3\0\0056\0\0\0009\0\1\0'\2\2\0B\0\2\1K\0\1\0�\f        function! s:edge_custom() abort\n          \" Link a highlight group to a predefined highlight group.\n          \" See `colors/edge.vim` for all predefined highlight groups.\n          \" Customize nvim-pqf (https://gitlab.com/yorickpeterse/nvim-pqf)\n          highlight! link qfPath BlueSign\n          highlight! link qfPosition Identifier\n          highlight! link qfError ErrorMsg\n          highlight! link qfWarning WarningMsg\n          highlight! link qfInfo MoreMsg\n          highlight! link qfHint ModeMsg\n\n          \" Initialize the color palette.\n          \" The parameter is a valid value for `g:edge_style`,\n          let l:palette = edge#get_palette('aura')\n          \" Define a highlight group.\n          \" The first parameter is the name of a highlight group,\n          \" the second parameter is the foreground color,\n          \" the third parameter is the background color,\n          \" the fourth parameter is for UI highlighting which is optional,\n          \" and the last parameter is for `guisp` which is also optional.\n          \" See `autoload/edge.vim` for the format of `l:palette`.\n          call edge#highlight('QuickFixLine', l:palette.none, l:palette.none, 'bold')\n\n          \" Customize NvimTree\n          call edge#highlight('NvimTreeExecFile', l:palette.none, l:palette.none, 'bold')\n          call edge#highlight('NvimTreeSymlink', l:palette.purple, l:palette.none)\n        endfunction\n\n        augroup EdgeCustom\n          autocmd!\n          autocmd ColorScheme edge call s:edge_custom()\n        augroup END\n\n        \" Set theme\n        set background=light\n        color edge\n      \bcmd\bvim\0", "config", "edge")
time([[Config for edge]], false)
-- Config for: nvim-ts-autotag
time([[Config for nvim-ts-autotag]], true)
try_loadstring("\27LJ\2\ng\0\0\4\0\6\0\t6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0005\3\3\0=\3\5\2B\0\2\1K\0\1\0\fautotag\1\0\0\1\0\1\venable\2\nsetup\28nvim-treesitter.configs\frequire\0", "config", "nvim-ts-autotag")
time([[Config for nvim-ts-autotag]], false)
-- Config for: vim-matchup
time([[Config for vim-matchup]], true)
try_loadstring("\27LJ\2\n�\2\0\0\4\0\14\0\0296\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0005\3\3\0=\3\5\2B\0\2\0016\0\6\0009\0\a\0)\1\1\0=\1\b\0006\0\6\0009\0\a\0)\1\1\0=\1\t\0006\0\6\0009\0\a\0)\1d\0=\1\n\0006\0\6\0009\0\a\0)\1\1\0=\1\v\0006\0\6\0009\0\a\0005\1\r\0=\1\f\0K\0\1\0\1\0\1\vmethod\18status_manual!matchup_matchparen_offscreen\28matchup_override_vimtex+matchup_matchparen_deferred_show_delay matchup_matchparen_deferred\29matchup_surround_enabled\6g\bvim\fmatchup\1\0\0\1\0\2\venable\2\25disable_virtual_text\2\nsetup\28nvim-treesitter.configs\frequire\0", "config", "vim-matchup")
time([[Config for vim-matchup]], false)
-- Load plugins in order defined by `after`
time([[Sequenced loading]], true)
vim.cmd [[ packadd lualine.nvim ]]

-- Config for: lualine.nvim
try_loadstring("\27LJ\2\n�\1\0\1\3\0\1\0\0065\1\0\0008\2\0\1\14\0\2\0X\3\1�\18\2\0\0L\2\2\0\1\0\17\fCOMMAND\6C\14O-PENDING\bO-P\nSHELL\aSH\vS-LINE\bS-L\fCONFIRM\fCONFIRM\tMORE\tMORE\vVISUAL\6V\14V-REPLACE\bV-R\vV-LINE\bV-L\vINSERT\6I\vNORMAL\6N\rTERMINAL\tTERM\aEX\aEX\fREPLACE\6R\fV-BLOCK\bV-B\fS-BLOCK\bS-B\vSELECT\6S�\1\0\0\6\0\n\0\0176\0\0\0'\2\1\0B\0\2\2\18\3\0\0009\1\2\0006\4\3\0009\4\4\0049\4\5\0049\4\6\4B\1\3\2\18\3\1\0009\1\a\0016\4\3\0009\4\b\0049\4\t\4B\4\1\0C\1\1\0\vgetcwd\afn\18make_relative\t_dir\fdirvish\6b\bvim\bnew\17plenary.path\frequire�\1\0\0\2\0\b\0 5\0\4\0006\1\0\0009\1\1\0019\1\2\1\15\0\1\0X\2\4�6\1\0\0009\1\1\0019\1\2\0019\1\3\1=\1\3\0006\1\0\0009\1\1\0019\1\2\1\15\0\1\0X\2\4�6\1\0\0009\1\1\0019\1\2\0019\1\5\1=\1\6\0006\1\0\0009\1\1\0019\1\2\1\15\0\1\0X\2\4�6\1\0\0009\1\1\0019\1\2\0019\1\a\1=\1\a\0L\0\2\0\fremoved\rmodified\fchanged\1\0\0\nadded\25gitsigns_status_dict\6b\bvimC\0\0\1\0\5\1\t6\0\0\0009\0\1\0009\0\2\0\t\0\0\0X\0\2�'\0\3\0L\0\2\0'\0\4\0L\0\2\0\5\t⮻ \15zoomwintab\6t\bvim\2�\6\1\0\v\0,\0E3\0\0\0003\1\1\0005\2\6\0005\3\2\0004\4\3\0>\1\1\4=\4\3\0035\4\4\0=\4\5\3=\3\a\0025\3\b\0=\3\t\0023\3\n\0003\4\v\0006\5\f\0'\a\r\0B\5\2\0029\5\14\0055\a\21\0005\b\15\0005\t\16\0=\t\17\b5\t\18\0=\t\19\b4\t\0\0=\t\20\b=\b\22\a5\b\25\0004\t\3\0005\n\23\0=\0\24\n>\n\1\t>\4\2\t=\t\3\b5\t\26\0005\n\27\0=\3\28\n>\n\2\t=\t\29\b5\t\30\0=\t\31\b5\t \0=\t!\b5\t\"\0=\t\5\b5\t#\0=\t$\b=\b\a\a5\b%\0004\t\0\0=\t\3\b4\t\0\0=\t\29\b5\t&\0=\t\31\b5\t'\0=\t!\b4\t\0\0=\t\5\b4\t\0\0=\t$\b=\b(\a4\b\0\0=\b)\a5\b*\0>\2\4\b=\b+\aB\5\2\1K\0\1\0\15extensions\1\4\0\0\14nvim-tree\rquickfix\rfugitive\ftabline\22inactive_sections\1\2\0\0\16bo:filetype\1\2\0\0\rfilename\1\0\0\14lualine_z\1\2\0\0\16diagnostics\1\2\0\0\16bo:filetype\14lualine_x\1\2\0\0\rencoding\14lualine_c\1\2\0\0\rfilename\14lualine_b\vsource\1\2\0\0\tdiff\1\2\0\0\vbranch\1\0\0\bfmt\1\2\0\0\tmode\foptions\1\0\0\23disabled_filetypes\23section_separators\1\0\2\nright\b\tleft\b\25component_separators\1\0\2\nright\b\tleft\b\1\0\4\25always_divide_middle\2\tpath\3\1\18icons_enabled\2\ntheme\tauto\nsetup\flualine\frequire\0\0\14filetypes\1\2\0\0\fdirvish\rsections\1\0\0\14lualine_y\1\2\0\0\16bo:filetype\14lualine_a\1\0\0\0\0\0", "config", "lualine.nvim")

vim.cmd [[ packadd telescope-fzf-native.nvim ]]
vim.cmd [[ packadd telescope-frecency.nvim ]]
vim.cmd [[ packadd telescope.nvim ]]

-- Config for: telescope.nvim
try_loadstring("\27LJ\2\n�\1\0\1\6\1\b\0\0166\1\0\0'\3\1\0B\1\2\0029\1\2\0015\3\6\0006\4\3\0009\4\4\0049\4\5\4B\4\1\2=\4\5\0039\4\a\0B\1\3\2-\2\0\0004\4\3\0>\1\1\4D\2\2\0\0\0\rfilename\1\0\0\bcwd\tloop\bvim\19transform_path\20telescope.utils\frequire�\2\1\1\a\1\19\0&9\1\0\0009\1\1\1\6\1\2\0X\1\4�9\1\0\0009\1\1\1\14\0\1\0X\2\1�'\1\3\0006\2\4\0'\4\5\0B\2\2\2\18\4\2\0009\2\6\2\18\5\1\0B\2\3\2\18\4\2\0009\2\a\0026\5\b\0009\5\t\0059\5\n\5B\5\1\0A\2\1\2\18\1\2\0005\2\v\0=\1\f\0029\3\r\0'\4\14\0\18\5\1\0&\3\5\3=\3\15\0029\3\r\0=\3\r\0023\3\16\0=\3\17\2=\1\18\0022\0\0�L\2\2\0\0�\rfilename\fdisplay\0\fordinal\b : \nbufnr\nvalue\1\0\1\nvalid\2\bcwd\tloop\bvim\14normalize\bnew\17plenary.path\frequire\14[No Name]\5\tname\tinfo�\1\1\0\5\0\a\0\r6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\3\0004\3\3\0005\4\4\0>\4\1\3=\3\5\2B\0\2\0023\1\6\0002\0\0�L\1\2\0\0\nitems\1\0\1\14remaining\2\1\0\1\14separator\6 \vcreate$telescope.pickers.entry_display\frequire�\r\1\0\b\0003\0J3\0\0\0006\1\1\0'\3\2\0B\1\2\0029\1\3\0015\3\f\0005\4\4\0005\5\5\0=\5\6\0045\5\t\0006\6\a\0009\6\b\6B\6\1\2=\6\n\5=\5\v\4=\4\r\0035\4\27\0005\5\14\0005\6\17\0005\a\15\0>\a\1\0065\a\16\0=\a\18\0065\a\19\0=\a\20\0065\a\21\0=\a\22\6=\6\23\5\18\6\0\0B\6\1\2=\6\24\0055\6\26\0006\a\a\0009\a\25\a=\a\n\6=\6\a\5=\5\28\0045\5\30\0005\6\29\0=\6\23\0055\6 \0005\a\31\0=\a!\6=\6\"\0055\6#\0=\6$\5=\5%\0045\5'\0005\6&\0=\6\23\0055\6(\0=\6)\5=\5*\0045\5,\0005\6+\0=\6\23\0055\6-\0=\6)\5=\5.\4=\4/\3B\1\2\0016\1\1\0'\3\2\0B\1\2\0029\0010\1'\0031\0B\1\2\0016\1\1\0'\3\2\0B\1\2\0029\0010\1'\0032\0B\1\2\1K\0\1\0\rfrecency\bfzf\19load_extension\fpickers\16grep_string\1\t\0\0\arg\18--color=never\17--no-heading\r--hidden\r--column\17--smart-case\20--with-filename\18--line-number\1\0\5\18prompt_prefix\16Filter ❯ \18results_title\5\18preview_title\5\24disable_coordinates\2\21disable_devicons\2\1\t\0\0\b─\b│\b─\b│\b┌\b┐\b┘\b└\14live_grep\22vimgrep_arguments\1\t\0\0\arg\18--color=never\17--no-heading\r--hidden\r--column\17--smart-case\20--with-filename\18--line-number\1\0\6\18prompt_prefix\16Regexp ❯ \18results_title\5\21disable_devicons\2\18preview_title\5\24disable_coordinates\2\17prompt_title\5\1\t\0\0\b─\b│\b─\b│\b┌\b┐\b┘\b└\15find_files\17find_command\1\6\0\0\arg\f--files\r--hidden\18--color=never\17--no-heading\18layout_config\vheight\1\0\0\1\0\1\fpadding\3\1\1\0\a\18prompt_prefix\15Files ❯ \18results_title\5\17prompt_title\5\21sorting_strategy\15descending\20layout_strategy\rvertical\18preview_title\5\21disable_devicons\2\1\t\0\0\b─\b│\b─\b│\b┌\b┐\b┘\b└\fbuffers\1\0\0\1\0\0\22telescope_buffers\16entry_maker\16borderchars\fpreview\1\t\0\0\b─\b│\b─\b│\b┌\b┐\b┘\b└\fresults\1\t\0\0\b─\b│\b─\b│\b├\b┤\b┘\b└\vprompt\1\0\0\1\t\0\0\b─\b│\6 \b│\b┌\b┐\b│\b│\1\t\0\0\b─\b│\b─\b│\b┌\b┐\b┘\b└\1\0\b\18prompt_prefix\17Buffers ❯ \21disable_devicons\2\rsort_mru\2\18sort_lastused\2\26ignore_current_buffer\2\14previewer\1\17prompt_title\5\ntheme\rdropdown\rdefaults\1\0\0\21default_mappings\6i\1\0\0\23telescope_defaults\rmappings\17path_display\1\2\0\0\rtruncate\1\0\3\18prompt_prefix\t❯ \20scroll_strategy\ncycle\20selection_caret\a> \nsetup\14telescope\frequire\0\0", "config", "telescope.nvim")

time([[Sequenced loading]], false)

-- Command lazy-loads
time([[Defining lazy-load commands]], true)
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Neogit lua require("packer.load")({'neogit'}, { cmd = "Neogit", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Delete lua require("packer.load")({'vim-eunuch'}, { cmd = "Delete", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[au CmdUndefined Delete! ++once lua require"packer.load"({'vim-eunuch'}, {}, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Unlink lua require("packer.load")({'vim-eunuch'}, { cmd = "Unlink", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[au CmdUndefined Unlink! ++once lua require"packer.load"({'vim-eunuch'}, {}, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Move lua require("packer.load")({'vim-eunuch'}, { cmd = "Move", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[au CmdUndefined Move! ++once lua require"packer.load"({'vim-eunuch'}, {}, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Rename lua require("packer.load")({'vim-eunuch'}, { cmd = "Rename", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[au CmdUndefined Rename! ++once lua require"packer.load"({'vim-eunuch'}, {}, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Chmod lua require("packer.load")({'vim-eunuch'}, { cmd = "Chmod", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Mkdir lua require("packer.load")({'vim-eunuch'}, { cmd = "Mkdir", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[au CmdUndefined Mkdir! ++once lua require"packer.load"({'vim-eunuch'}, {}, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Cfind lua require("packer.load")({'vim-eunuch'}, { cmd = "Cfind", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[au CmdUndefined Cfind! ++once lua require"packer.load"({'vim-eunuch'}, {}, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Lfind lua require("packer.load")({'vim-eunuch'}, { cmd = "Lfind", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[au CmdUndefined Lfind! ++once lua require"packer.load"({'vim-eunuch'}, {}, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Clocate lua require("packer.load")({'vim-eunuch'}, { cmd = "Clocate", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[au CmdUndefined Clocate! ++once lua require"packer.load"({'vim-eunuch'}, {}, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Llocate lua require("packer.load")({'vim-eunuch'}, { cmd = "Llocate", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[au CmdUndefined Llocate! ++once lua require"packer.load"({'vim-eunuch'}, {}, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file SudoEdit lua require("packer.load")({'vim-eunuch'}, { cmd = "SudoEdit", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file SudoWrite lua require("packer.load")({'vim-eunuch'}, { cmd = "SudoWrite", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Wall lua require("packer.load")({'vim-eunuch'}, { cmd = "Wall", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file W lua require("packer.load")({'vim-eunuch'}, { cmd = "W", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file NvimTreeFindFileToggle lua require("packer.load")({'nvim-tree.lua'}, { cmd = "NvimTreeFindFileToggle", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file NvimTreeFocus lua require("packer.load")({'nvim-tree.lua'}, { cmd = "NvimTreeFocus", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file NvimTreeToggle lua require("packer.load")({'nvim-tree.lua'}, { cmd = "NvimTreeToggle", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
time([[Defining lazy-load commands]], false)

vim.cmd [[augroup packer_load_aucmds]]
vim.cmd [[au!]]
  -- Filetype lazy-loads
time([[Defining lazy-load filetype autocommands]], true)
vim.cmd [[au FileType jsx ++once lua require("packer.load")({'nvim-colorizer.lua'}, { ft = "jsx" }, _G.packer_plugins)]]
vim.cmd [[au FileType css ++once lua require("packer.load")({'nvim-colorizer.lua'}, { ft = "css" }, _G.packer_plugins)]]
vim.cmd [[au FileType vue ++once lua require("packer.load")({'nvim-colorizer.lua'}, { ft = "vue" }, _G.packer_plugins)]]
vim.cmd [[au FileType markdown ++once lua require("packer.load")({'markdown-preview.nvim'}, { ft = "markdown" }, _G.packer_plugins)]]
vim.cmd [[au FileType vim ++once lua require("packer.load")({'nvim-colorizer.lua'}, { ft = "vim" }, _G.packer_plugins)]]
vim.cmd [[au FileType tsx ++once lua require("packer.load")({'nvim-colorizer.lua'}, { ft = "tsx" }, _G.packer_plugins)]]
vim.cmd [[au FileType javascript ++once lua require("packer.load")({'nvim-colorizer.lua'}, { ft = "javascript" }, _G.packer_plugins)]]
vim.cmd [[au FileType html ++once lua require("packer.load")({'nvim-colorizer.lua'}, { ft = "html" }, _G.packer_plugins)]]
time([[Defining lazy-load filetype autocommands]], false)
vim.cmd("augroup END")
if should_profile then save_profiles() end

end)

if not no_errors then
  error_msg = error_msg:gsub('"', '\\"')
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
