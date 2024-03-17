return {
  -- Theme
  --  { 'EdenEast/nightfox.nvim' }
  {
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
          let l:palette = edge#get_palette('aura', 0, {})
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
  },

  -- Status bar
  {
    'nvim-lualine/lualine.nvim',
    -- We must ensure lualine gets loaded after the theme otherwise it fails to pick up some colors
    dependencies = { 'sainnhe/edge', 'nvim-tree/nvim-web-devicons' },
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

      -- A small function to use gitsigns or gitgutter as source for git diff
      local git_status = function()
        if vim.b.gitsigns_status_dict then
          return {
            added = vim.b.gitsigns_status_dict.added,
            modified = vim.b.gitsigns_status_dict.changed,
            removed = vim.b.gitsigns_status_dict.removed,
          }

        elseif vim.b.gitgutter and vim.b.gitgutter.summary then
          return {
            added = vim.b.gitgutter.summary[1],
            modified = vim.b.gitgutter.summary[2],
            removed = vim.b.gitgutter.summary[3],
          }

        else
          return {}
        end
      end

      -- Return the status of zoomwintoggle
      local zoomwin_icon = function()
        if vim.t.zoomwintab == 1 then
          return '◱ '
        end

        return ''
      end

      -- A small extension to support dirvish
      local dirvish_ext = {
        sections = {
          lualine_a = { { 'mode', fmt = mode_map }, zoomwin_icon },
          lualine_c = {'filename'},
          lualine_y = {'bo:filetype'},
        },
        filetypes = {'dirvish'},
      }

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
          lualine_b = { 'branch', { 'diff', source = git_status } },
          lualine_c = {'filename'},
          lualine_x = {'location'},
          lualine_y = {'bo:filetype', 'diagnostics'},
          lualine_z = {},
        },
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = {'filename'},
          lualine_x = {'bo:filetype'},
          lualine_y = {},
          lualine_z = {},
        },
        tabline = {
          lualine_a = {{
            'tabs',
            mode = 1,
            path = 1,
            -- This sets the maximum filename length so it can be shortened when overflowing,
            -- becoming for instance a/b/c/file.txt
            tab_max_length = 40,
            -- This sets the maximum name length for all tabs, so the ones that are not active will
            -- be shortened to ellipsis when overflowing this threshold, thus becoming `...`
            max_length = function() return vim.o.columns end,
            component_separators = { left = '', right = ''},
            section_separators = { left = '', right = ''},
          }},
        },
        extensions = {'nvim-tree', 'quickfix', dirvish_ext},
      })

      -- When using `tabline` option, lualine automatically sets `showtabline` to 2, but it isn't
      -- what we want, so we override it after the setup
      -- See: https://github.com/nvim-lualine/lualine.nvim/issues/395#issuecomment-1312371694
      vim.o.showtabline = 1
    end
  },

  -- Enhance and colorize the experience of qf window
  {
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
  },
}
