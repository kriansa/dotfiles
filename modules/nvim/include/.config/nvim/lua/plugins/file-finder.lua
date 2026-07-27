-- snacks hardcodes exclusions for these directories onto the fd/ripgrep command line. In both
-- tools a command-line glob outranks every ignore file, so while those flags are present nothing
-- in .fdignore/.rgignore can bring the directories back. Stripping the flags hands the decision
-- back to the ignore files.
local unexcluded_dirs = { [".git"] = true, [".bare"] = true }

-- Flags whose value is an exclusion pattern. Both tools accept `--flag value` and `--flag=value`.
local exclude_flags = {
  ["-E"] = true,
  ["--exclude"] = true,
  ["-g"] = true,
  ["--glob"] = true,
  ["--iglob"] = true,
}

local function without_hardcoded_excludes(args)
  local kept = {}
  local i = 1

  while i <= #args do
    local arg = args[i]

    -- A flag joined to its value by `=`, otherwise a bare flag taking the next argument.
    local joined_flag, value = arg:match("^(%-%-?[%w-]+)=(.*)$")
    local flag = joined_flag or (exclude_flags[arg] and arg or nil)
    if not joined_flag then
      value = args[i + 1]
    end

    -- ripgrep spells an exclusion as a negated glob, fd does not.
    local dir = value and value:gsub("^!", "")

    if flag and exclude_flags[flag] and dir and unexcluded_dirs[dir] then
      i = i + (joined_flag and 1 or 2)
    else
      kept[#kept + 1] = arg
      i = i + 1
    end
  end

  return kept
end

local function let_ignore_files_decide()
  local proc = require("snacks.picker.source.proc")
  if proc.hardcoded_excludes_stripped then
    return
  end
  proc.hardcoded_excludes_stripped = true

  -- Every picker that shells out builds its argument list privately and hands it straight to
  -- this function, so it is the one place that sees the final command for all of them.
  local spawn = proc.proc
  proc.proc = function(opts, ctx)
    local cmd = opts.cmd
    if opts.args and (cmd == "fd" or cmd == "fdfind" or cmd == "rg") then
      opts.args = without_hardcoded_excludes(opts.args)
    end
    return spawn(opts, ctx)
  end
end

return {
  {
    "folke/snacks.nvim",
    config = function(_, opts)
      let_ignore_files_decide()
      require("snacks").setup(opts)
    end,
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
