return {
  {
    "stevearc/conform.nvim",
    opts = {
      stop_after_first = true,

      formatters_by_ft = {
        c = { "clang-format" },
        cpp = { "clang-format" },
        nim = { "nimpretty" },
        elixir = { "mix" },
        zig = { "zig fmt" },

        -- JS/TS/Vue
        javascript = { "prettierd", "prettier" },
        typescript = { "prettierd", "prettier" },
        javascriptreact = { "prettierd", "prettier" },
        typescriptreact = { "prettierd", "prettier" },
        vue = { "prettierd", "prettier" },
        tailwind = { "prettierd", "prettier" },

        -- Python (run both when available)
        python = function(bufnr)
          local conform = require("conform")
          local has_ruff = conform.get_formatter_info("ruff", bufnr).available
          local has_ruff_format = conform.get_formatter_info("ruff_format", bufnr).available

          if has_ruff and has_ruff_format then
            return { "ruff", "ruff_format" }
          end
          return { "isort", "black" }
        end,

        lua = { "stylua" },
        sh = { "shfmt" },
        rust = { "rustfmt" },
      },

      formatters = {
        ruff = {
          command = "ruff",
          args = {
            "check",
            "--fix",
            "--select=I",
            "--exit-zero",
            "--force-exclude",
            "--stdin-filename",
            "$FILENAME",
            "-",
          },
          stdin = true,
        },

        black = {
          command = "black",
          args = { "--line-length", "80", "-" },
          stdin = true,
        },
      },
    },
  },
}
