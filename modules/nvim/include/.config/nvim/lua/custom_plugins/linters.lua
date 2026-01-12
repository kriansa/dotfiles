-- Custom linters configuration module
local M = {}

-- Prioritize standardrb when both rubocop and standardrb are installed
function M.ruby_linter_command()
  local dirname = vim.fn.expand("%:p:h")

  local found_standard = vim.fs.find({".standard.yml", ".standard.yaml"}, { upward = true, path = dirname })[1]
  local found_rubocop = vim.fs.find({".rubocop.yml", ".rubocop.yaml"}, { upward = true, path = dirname })[1]

  if found_rubocop and not found_standard then
    return "rubocop"
  end

  -- By default we use standardrb
  return "standardrb"
end

function M.setup_linters()
  -- Define a linter wrapper that will use standardrb or rubocop depending on the presence of a
  -- configuration file
  require('lint').linters.standardrb_or_rubocop = {
    cmd = M.ruby_linter_command,
    args = {
      "--force-exclusion",
      "--format", "json",
      "--stdin",
      "--server",
      function() return vim.fn.expand("%:p") end,
    },
    stdin = true,
    ignore_exitcode = true,
    parser = function(output)
      local diagnostics = {}
      local decoded = vim.json.decode(output)
      local offences = #decoded.files > 0 and decoded.files[1].offenses or {}
      local severity_map = {
        ['fatal'] = vim.diagnostic.severity.ERROR,
        ['error'] = vim.diagnostic.severity.ERROR,
        ['warning'] = vim.diagnostic.severity.WARN,
        ['convention'] = vim.diagnostic.severity.HINT,
        ['refactor'] = vim.diagnostic.severity.INFO,
        ['info'] = vim.diagnostic.severity.INFO,
      }

      for _, off in pairs(offences or {}) do
        table.insert(diagnostics, {
          lnum = off.location.start_line - 1,
          col = off.location.start_column - 1,
          end_lnum = off.location.last_line - 1,
          end_col = off.location.last_column,
          severity = severity_map[off.severity],
          message = off.message,
          code = off.cop_name,
          user_data = {
            lsp = {
              code = off.cop_name,
            }
          }
        })
      end

      return diagnostics
    end,
  }

  require('lint').linters.reek = {
    cmd = 'reek',
    args = { '-f', 'json', '--stdin-filename', function() return vim.fn.expand("%:p") end },
    stdin = true,
    ignore_exitcode = true,
    parser = function(output)
      local diagnostics = {}
      local offenses = vim.json.decode(output)

      for _, entry in pairs(offenses or {}) do
        table.insert(diagnostics, {
          lnum = entry.lines[1] - 1,
          end_lnum = (entry.lines[2] or entry.lines[1]) - 1,
          col = 0,
          severity = vim.diagnostic.severity.INFO,
          message = entry.context .. " " .. entry.message,
          code = entry.smell_type,
        })
      end

      return diagnostics
    end,
  }

  -- Defines a linter for ansible_lint that uses the sarif output so we get more details on each
  -- linter run, as opposed to the default where all rules have the same severity
  require('lint').linters.ansible_lint = {
    cmd = 'ansible-lint',
    args = { '--offline', '-f', 'sarif', function() return vim.fn.expand("%:p") end },
    ignore_exitcode = true,
    parser = function(output)
      local diagnostics = {}
      local decoded = vim.json.decode(output)
      local offenses = decoded and decoded.runs and decoded.runs[1] and decoded.runs[1].results or {}
      local severity_map = {
        ['none'] = vim.diagnostic.severity.HINT,
        ['note'] = vim.diagnostic.severity.INFO,
        ['warning'] = vim.diagnostic.severity.WARN,
        ['error'] = vim.diagnostic.severity.ERROR,
      }
      local rules = {}
      local linter_rules = decoded and decoded.runs and decoded.runs[1] and decoded.runs[1].tool and decoded.runs[1].tool.driver and decoded.runs[1].tool.driver.rules or {}
      for _, rule in ipairs(linter_rules) do
        rules[rule.id] = rule.shortDescription.text
      end

      -- Loop through the offenses list
      for _, entry in pairs(offenses) do
        table.insert(diagnostics, {
          lnum = entry.locations[1].physicalLocation.region.startLine - 1,
          col = (entry.locations[1].physicalLocation.region.startColumn or 1) - 1,
          severity = severity_map[entry.level],
          message = rules[entry.ruleId],
          code = entry.ruleId,
        })
      end

      return diagnostics
    end,
  }

  require('lint').linters.vale.ignore_exitcode = true

  require('lint').linters_by_ft = {
    ruby = {"standardrb_or_rubocop", "reek"},
    markdown = {"vale"},
    python = {"ruff", "mypy"},

    ["yaml.ansible"] = {"ansible_lint"},

    javascript = {"eslint"},
    javascriptreact = {"eslint"},
    typescript = {"eslint"},
    typescriptreact = {"eslint"},
    vue = {"eslint"},

    go = {"golangcilint"},

    fish = {"fish"},
    sh = {"shellcheck"},
  }

  -- Set the linter to enabled by default
  vim.g._linter_enabled = true

  vim.api.nvim_create_user_command('LintToggle', function()
    vim.g._linter_enabled = not vim.g._linter_enabled

    local status
    if vim.g._linter_enabled then
      status = "enabled"
      require("lint").try_lint(nil, { ignore_errors = true })
    else
      status = "disabled"
      vim.diagnostic.reset()
    end

    print("Linter: " .. status)
  end, { desc = "Toggle the automatic linter execution on buffers" })

  vim.api.nvim_create_autocmd({ "BufEnter", "BufRead", "BufWritePost", "TextChanged" }, {
    callback = function()
      if vim.g._linter_enabled then
        require("lint").try_lint(nil, { ignore_errors = true })
      end
    end,
  })
end

return M
