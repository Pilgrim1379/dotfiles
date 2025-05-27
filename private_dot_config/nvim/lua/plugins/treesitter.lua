return {
    {
        "nvim-treesitter/nvim-treesitter",
        opts = {
            highlight = {enable = true},
            indent = {enable = true},
            ensure_installed = {
                "bash", "c", "html", "javascript", "json", "jsonc", "json5",
                "lua", "luadoc", "luap", "markdown", "markdown_inline",
                "python", "ninja", "rst", "toml", "query", "regex", "tsx",
                "typescript", "vim", "vimdoc", "yaml", "elixir", "heex", "eex",
                "elm", "rust", "julia", "erlang", "clojure", "zig", "ocaml",
                "haskell", "nim", "astro", "sql"
            },
            incremental_selection = {
                enable = true,
                keymaps = {
                    init_selection = "<C-space>",
                    node_incremental = "<C-space>",
                    scope_incremental = false,
                    node_decremental = "<bs>"
                }
            }
        }
    }
}
