return {
  {
    "folke/snacks.nvim",
    opts = {
      picker = {
        preview = { enabled = false },
        layout = "dropdown",
        prompt = "❯ ",

        sources = {
          files = {
            ignored = false,
            hidden = true,
            toggles = {
              regex = { enabled = false },
              follow = { enabled = false },
            },
            supports_live = false,
            matcher = {
              filename_bonus = false, -- give bonus for matching file names (last part of the path)
            },
          },

          grep = {
            live = true,
            hidden = true,
            ignored = false,
            title = "Search",
          },

          lines = {
            layout = {
              preview = false,
              preset = "ivy",
            },
          },

          buffers = {
            format = "file",
            current = false,
            sort_lastused = true,
            toggles = {
              regex = { enabled = false },
              ignored = { enabled = false },
              follow = { enabled = false },
            },
            actions = {
              delete_buffer = function(picker)
                local selected = picker:selected({ fallback = true })

                -- Save the current cursor line in the list window
                local cursor_line = vim.api.nvim_win_get_cursor(picker.list.win.win)[1]

                for _, item in ipairs(selected) do
                  Snacks.bufdelete(item.buf)
                end

                picker:find()

                vim.schedule(function()
                  local count = picker.list:count()
                  if count > 0 then
                    local new_line = math.min(cursor_line, count)
                    vim.api.nvim_win_set_cursor(picker.list.win.win, { new_line, 0 })
                    picker.list.cursor = new_line  -- Sync internal state
                  end
                end)
              end,
            },
            win = {
              input = {
                keys = mappings.snacks_buffers,
              },
              list = {
                keys = mappings.snacks_buffers,
              },
            },
          },
        },

        layouts = {
          dropdown = {
            hidden = { "preview" },
            layout = {
              backdrop = false,
              row = 1,
              width = 0.4,
              min_width = 90,
              height = 0.8,
              border = "none",
              box = "vertical",
              { win = "preview", title = "{preview}", height = 0.4, border = true },
              {
                box = "vertical",
                border = true,
                title = "{title} {live} {flags}",
                title_pos = "left",
                { win = "input", height = 1, border = "bottom" },
                { win = "list", border = "none" },
              },
            },
          },
        },

        matcher = {
          fuzzy = true, -- use fuzzy matching
          smartcase = true, -- use smartcase
          ignorecase = true, -- use ignorecase
          sort_empty = false, -- sort results when the search string is empty
          filename_bonus = true, -- give bonus for matching file names (last part of the path)
          file_pos = true, -- support patterns like `file:line:col` and `file:line`
          -- the bonusses below, possibly require string concatenation and path normalization,
          -- so this can have a performance impact for large lists and increase memory usage
          cwd_bonus = false, -- give bonus for matching files in the cwd
          frecency = true, -- frecency bonus
          history_bonus = true, -- give more weight to chronological order
        },
        win = {
          input = {
            keys = mappings.snacks_general,
          },
        },
        toggles = {
          follow = { icon = "+follow", value = true },
          hidden = { icon = "+hidden", value = true, enabled = false },
          ignored = { icon = "+ignored", value = true },
          regex = { icon = "+regexp", value = true },
        },
        icons = {
          files = { enabled = false },
          git = { enabled = false },
        },
      },
    },
  },
}
