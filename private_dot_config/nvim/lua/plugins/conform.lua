return {
    {
        "stevearc/conform.nvim",
        opts = {
            -- NOTE: stop_after_first is NOT set globally here.
            --
            -- When set globally it applies to every formatter list, including
            -- Python's { "ruff", "ruff_format" } — conform would run ruff and
            -- stop, never running ruff_format. The result: no code formatting,
            -- only import sorting.
            --
            -- Instead, stop_after_first is set inline on the lists that are
            -- true fallback lists (e.g. prettierd → prettier). Python uses a
            -- function that already does its own availability check and returns
            -- a definitive list, so it runs all entries without stop_after_first.

            formatters_by_ft = {
                c               = { "clang-format" },
                cpp             = { "clang-format" },
                nim             = { "nimpretty" },
                elixir          = { "mix" },
                zig             = { "zig fmt" },

                -- JS/TS/JSX/TSX: Biome handles formatting and linting in one pass.
                -- The Biome LSP (lspconfig.lua) provides inline diagnostics.
                -- Requires a biome.json at the project root to activate.
                javascript      = { "biome" },
                typescript      = { "biome" },
                javascriptreact = { "biome" },
                typescriptreact = { "biome" },

                -- CSS, JSON, JSONC, GraphQL: all natively supported by Biome.
                css             = { "biome" },
                json            = { "biome" },
                jsonc           = { "biome" },
                graphql         = { "biome" },

                -- Vue: Biome's Vue support is experimental as of 2025.
                -- Uncomment once it stabilises, and add biome to vue filetypes
                -- in lspconfig.lua at the same time.
                -- vue          = { "biome" },

                -- Python: run BOTH ruff (fixer) and ruff_format (formatter) in sequence.
                -- The function checks availability and returns all formatters to run.
                -- No stop_after_first here — both must execute.
                --
                -- What each does:
                --   ruff          → check --fix --select=I  (import sorting via isort rules)
                --   ruff_format   → format                  (code style, replaces black)
                --
                -- Fallback: isort + black for environments without ruff.
                python          = function(bufnr)
                    local conform         = require("conform")
                    local has_ruff        = conform.get_formatter_info("ruff", bufnr).available
                    local has_ruff_format = conform.get_formatter_info("ruff_format", bufnr).available

                    if has_ruff and has_ruff_format then
                        return { "ruff", "ruff_format" }
                    end
                    return { "isort", "black" }
                end,

                lua             = { "stylua" },
                sh              = { "shfmt" },
                rust            = { "rustfmt" },
            },

            formatters = {
                -- ruff used as a fixer (import sorter) via stdin.
                -- --select=I restricts it to isort rules only, keeping this formatter
                -- focused. ruff_format (the separate formatter entry) handles style.
                ruff = {
                    command = "ruff",
                    args = {
                        "check",
                        "--fix",
                        "--select=I",  -- imports only
                        "--exit-zero", -- don't fail if there are unfixable diagnostics
                        "--force-exclude",
                        "--stdin-filename",
                        "$FILENAME",
                        "-",
                    },
                    stdin = true,
                },

                -- black used as a fallback when ruff_format is not available.
                -- Kept at 80 chars to stay compatible with older projects.
                black = {
                    command = "black",
                    args = { "--line-length", "80", "-" },
                    stdin = true,
                },
            },
        },
    },
}
