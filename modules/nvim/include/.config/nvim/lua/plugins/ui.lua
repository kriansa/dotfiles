return function(use)
  -- Theme
  --  use 'EdenEast/nightfox.nvim'
  use {
    'sainnhe/edge',
    config = function()
      vim.cmd([[
        function! s:edge_custom() abort
          " Link a highlight group to a predefined highlight group.
          " See `colors/edge.vim` for all predefined highlight groups.
          " Customize nvim-pqf (https://gitlab.com/yorickpeterse/nvim-pqf)
          highlight! link qfPath BlueSign
          highlight! link qfPosition Identifier
          highlight! link qfError ErrorMsg
          highlight! link qfWarning WarningMsg
          highlight! link qfInfo MoreMsg
          highlight! link qfHint ModeMsg

          " Initialize the color palette.
          " The parameter is a valid value for `g:edge_style`,
          let l:palette = edge#get_palette('aura')
          " Define a highlight group.
          " The first parameter is the name of a highlight group,
          " the second parameter is the foreground color,
          " the third parameter is the background color,
          " the fourth parameter is for UI highlighting which is optional,
          " and the last parameter is for `guisp` which is also optional.
          " See `autoload/edge.vim` for the format of `l:palette`.
          call edge#highlight('QuickFixLine', l:palette.none, l:palette.none, 'bold')

          " Customize NvimTree
          call edge#highlight('NvimTreeExecFile', l:palette.none, l:palette.none, 'bold')
          call edge#highlight('NvimTreeSymlink', l:palette.purple, l:palette.none)
        endfunction

        augroup EdgeCustom
          autocmd!
          autocmd ColorScheme edge call s:edge_custom()
        augroup END

        " Set theme
        set background=light
        color edge
      ]])
    end
  }

  -- Status bar
  use {
    'nvim-lualine/lualine.nvim',
    -- We must ensure lualine gets loaded after the theme otherwise it fails to pick up some colors
    after = { 'edge' },
    requires = { 'kyazdani42/nvim-web-devicons' },
    config = function()
      local mode_map = function(str)
        local replacements = {
          ['NORMAL'] = "N",
          ['O-PENDING'] = "O-P",
          ['VISUAL'] = "V",
          ['V-LINE'] = "V-L",
          ['V-BLOCK'] = "V-B",
          ['SELECT'] = "S",
          ['S-LINE'] = "S-L",
          ['S-BLOCK'] = "S-B",
          ['INSERT'] = "I",
          ['REPLACE'] = "R",
          ['V-REPLACE'] = "V-R",
          ['COMMAND'] = "C",
          ['EX'] = "EX",
          ['MORE'] = "MORE",
          ['CONFIRM'] = "CONFIRM",
          ['SHELL'] = "SH",
          ['TERMINAL'] = "TERM",
        }

        return replacements[str] or str
      end

      -- A small extension to support Dirvish
      local get_dirvish_path = function()
        local Path = require('plenary.path')
        return Path:new(vim.b.dirvish["_dir"]):make_relative(vim.fn.getcwd())
      end
      local dirvish_ext = {
        sections = { lualine_a = {get_dirvish_path}, lualine_y = {'bo:filetype'} },
        filetypes = {'dirvish'},
      }

      -- A small function to use gitsigns as source for git diff
      local gitsigns_src = function()
        return {
          added = vim.b.gitsigns_status_dict and vim.b.gitsigns_status_dict.added,
          modified = vim.b.gitsigns_status_dict and vim.b.gitsigns_status_dict.changed,
          removed = vim.b.gitsigns_status_dict and vim.b.gitsigns_status_dict.removed,
        }
      end

      -- Return the status of zoomwintoggle
      local zoomwin_icon = function()
        if vim.t.zoomwintab == 1 then
          return '⮻ '
        end

        return ''
      end

      require('lualine').setup({
        options = {
          icons_enabled = true,
          theme = 'auto',
          component_separators = { left = '', right = ''},
          section_separators = { left = '', right = ''},
          disabled_filetypes = {},
          always_divide_middle = true,
          path = 1,
        },
        sections = {
          lualine_a = { { 'mode', fmt = mode_map }, zoomwin_icon },
          lualine_b = { 'branch', { 'diff', source = gitsigns_src } },
          lualine_c = {'filename'},
          lualine_x = {'encoding'},
          lualine_y = {'bo:filetype'},
          lualine_z = {'diagnostics'},
        },
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = {'filename'},
          lualine_x = {'bo:filetype'},
          lualine_y = {},
          lualine_z = {},
        },
        tabline = {},
        extensions = {'nvim-tree', 'quickfix', 'fugitive', dirvish_ext},
      })
    end
  }

  -- Enhance and colorize the experience of qf window
  use {
    "https://gitlab.com/yorickpeterse/nvim-pqf",
    config = function()
      require('pqf').setup({
        signs = {
          error = 'E',
          warning = 'W',
          info = 'I',
          hint = 'H'
        }
      })
    end
  }
end
