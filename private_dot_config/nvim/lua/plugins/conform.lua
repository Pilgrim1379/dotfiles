return {
    {
        "stevearc/conform.nvim",
        opts = {
            -- We want Python to run TWO formatters sequentially (ruff -> ruff_format),
            -- so do NOT stop after the first available formatter.
            stop_after_first = false,

            formatters_by_ft = {
                c = { "clang-format" },
                cpp = { "clang-format" },
                nim = { "nimpretty" },
                elixir = { "mix" },
                zig = { "zig fmt" },

                -- "First available" groups (choose prettierd if available, else prettier)
                tailwind = { { "prettierd", "prettier" } },
                javascript = { { "prettierd", "prettier" } },

                -- Python:
                --   1) ruff: import sorting (and only that, per config below)
                --   2) ruff_format: formatting
                --
                -- If Ruff isn't available, fall back to isort + black.
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
                -- Ruff "fix" step.
                -- Currently restricted to import sorting only (I rules).
                -- If later you want more autofixes, remove --select=I or tune it.
                ruff = {
                    command = "ruff",
                    args = {
                        "check",
                        "--fix",
                        "--select=I", -- import sorting only
                        "--exit-zero", -- never fail formatting step
                        "--force-exclude",
                        "--stdin-filename",
                        "$FILENAME",
                        "-",
                    },
                    stdin = true,
                },

                -- You had this already; keeping it as fallback only.
                black = {
                    command = "black",
                    args = { "--line-length", "80", "-" },
                    stdin = true,
                },
            },
        },
    },
}
