return {
  -- Theme
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
          " highlight! link NvimTreeFolderName BlueSign
          " highlight! link NvimTreeEmptyFolderName NvimTreeFolderName
          " highlight! link NvimTreeOpenedFolderName NvimTreeFolderName
          " highlight! link NvimTreeSymlinkFolderName NvimTreeFolderName

          " Customize copilot-cmp
          call edge#highlight('CmpItemKindCopilot', l:palette.blue, l:palette.none, 'bold')
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
    event = "VeryLazy",
    -- We must ensure lualine gets loaded after the theme otherwise it fails to pick up some colors
    dependencies = { 'sainnhe/edge', 'nvim-tree/nvim-web-devicons', 'AndreM222/copilot-lualine' },
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

      -- A small function to use minidiff, gitsigns or gitgutter as source for git diff
      local git_status = function()
        if vim.b.minidiff_summary then
          return {
            added = vim.b.minidiff_summary.add,
            modified = vim.b.minidiff_summary.change,
            removed = vim.b.minidiff_summary.delete,
          }
        elseif vim.b.gitsigns_status_dict then
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
          return '󰁌 '
        end

        return ''
      end

      -- This is sort of a workaround for the lack of extensions driven by "buftypes".
      -- Traditionally, lualine allows creating an extension for a given filetype, so one can show a
      -- different bar setup for specific filetypes. However, when on Terminal buffers, the filetype
      -- is set to a blank string, and the buftype is the one that gets set to "terminal".
      --
      -- There's an open PR to add that feature.
      -- See: https://github.com/nvim-lualine/lualine.nvim/pull/1125
      --
      -- The workaround is to create a section that checks for the buftype and then sets the the
      -- output filename to a blank string.
      local filename = function()
        if vim.bo.buftype == 'terminal' then
          return ""
        end

        local lualine_filename = require('lualine.components.filename')
        return lualine_filename:new({ path = 1 }):update_status()
      end

      function removeprefix(s, p)
        return (s:sub(0, #p) == p) and s:sub(#p+1) or s
      end

      function removesuffix(s, p)
        return (s:sub(-#p) == p) and s:sub(1, -#p-1) or s
      end

      local unprefixed_filename = function()
        local name = removeprefix(vim.fn.expand('%'), "oil://")
        name = removeprefix(name, vim.fn.getcwd() .. "/")
        name = removesuffix(name, "/")

        local flags = ""
        if vim.bo.modified then
          flags = " [+]"
        end
        return name .. flags
      end

      -- A small extension to support oil.nvim
      local oil_ext = {
        sections = {
          lualine_a = { { 'mode', fmt = mode_map }, zoomwin_icon },
          lualine_c = {unprefixed_filename},
          lualine_z = {function() return "Oil" end},
        },
        filetypes = {'oil'},
      }

      -- A small extension to support avante.nvim
      local avante_ext = {
        sections = {
          lualine_a = { { 'mode', fmt = mode_map }, zoomwin_icon },
          lualine_c = {},
          lualine_z = {'bo:filetype'},
        },
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = {},
          lualine_x = {},
          lualine_y = {},
          lualine_z = {'bo:filetype'},
        },
        filetypes = {'Avante', 'AvanteSelectedFiles', 'AvanteInput'},
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
          lualine_c = {filename},
          lualine_x = {'location'}, -- or %c for only column
          lualine_y = {'copilot', 'bo:filetype', 'diagnostics'},
          lualine_z = {},
        },
        inactive_sections = {
          lualine_a = {},
          lualine_b = {},
          lualine_c = {filename},
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
        extensions = {'nvim-tree', 'quickfix', oil_ext, avante_ext},
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
