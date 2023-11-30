return {
    {
        "mfussenegger/nvim-lint",
        -- https://github.com/mfussenegger/nvim-lint
        opts = function(_, opts)
            local linters = require("lint").linters
            -- linters.mypy.cmd = prefer_bin_from_venv("mypy")
            linters.sqlfluff.args = vim.list_extend({"--dialect", "postgres"},
                                                    linters.sqlfluff.args)

            local linters_by_ft = {
                -- this extends lazyvim's nvim-lint setup
                -- https://www.lazyvim.org/extras/linting/nvim-lint
                protobuf = {"buf", "protolint"},
                python = {"mypy", "ruff"},
                sh = {"shellcheck"},
                sql = {"sqlfluff"},
                -- yaml = { "yamllint" },
                elixir = {"credo"}
            }

            -- extend opts.linters_by_ft
            for ft, linters_ in pairs(linters_by_ft) do
                opts.linters_by_ft[ft] = opts.linters_by_ft[ft] or {}
                vim.list_extend(opts.linters_by_ft[ft], linters_)
            end
        end
    }
}
