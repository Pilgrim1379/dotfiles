return {
    {
        "stevearc/conform.nvim",
        -- https://github.com/stevearc/conform.nvim
        opts = {
            formatters_by_ft = {
                cs = {"csharpier"},
                elixir = {"mix"},
                -- Conform will run multiple formatters sequentially
                python = {"isort", "black"},
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
