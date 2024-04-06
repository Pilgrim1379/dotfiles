return {
    {
        "stevearc/conform.nvim",
        -- https://github.com/stevearc/conform.nvim
        opts = {
            formatters_by_ft = {
                cs = {"csharpier"},
                elm = {"elm-format"},
                c = {"clang-format"},
                cpp = {"clang-format"},
                zig = {"zig fmt"},
                nim = {"nimpretty"},
                elixir = {"mix"},
                tailwind = {"prettier"},
                -- Conform will run multiple formatters sequentially
                -- You can use a function here to determine the formatters dynamically
                python = function(bufnr)
                    if require("conform").get_formatter_info("ruff_format",
                                                             bufnr).available then
                        return {"ruff_format"}
                    else
                        return {"isort", "black"}
                    end
                end,
                lua = {"stylua"},
                sh = {"shfmt"},
                rust = {"rustfmt"},
                -- Use a sub-list to run only the first available formatter
                javascript = {{"prettierd", "prettier"}}
            },
            formatters = {
                black = {
                    command = "dotnet-csharpier",
                    args = {"--write-stdout"}
                }
            }
        }
    }
}
