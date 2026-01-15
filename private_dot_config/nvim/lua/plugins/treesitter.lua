return {
    {
        "nvim-treesitter/nvim-treesitter",
        opts = function(_, opts)
            -- Highlighting
            opts.highlight = opts.highlight or {}
            opts.highlight.enable = true

            -- Indentation
            opts.indent = opts.indent or {}
            opts.indent.enable = true
            opts.indent.disable = opts.indent.disable or {}

            -- Disable Tree-sitter indent for problematic languages
            for _, ft in ipairs({ "python", "yaml" }) do
                if not vim.tbl_contains(opts.indent.disable, ft) then
                    table.insert(opts.indent.disable, ft)
                end
            end

            -- Parsers (minimal but high coverage for your workflow)
            opts.ensure_installed = {
                -- Neovim/LazyVim essentials
                "lua",
                "vim",
                "vimdoc",
                "query",
                "regex",

                -- Frequent formats
                "python",
                "javascript",
                "bash", -- covers zsh scripts well
                "json",
                "jsonc",

                -- Common docs
                "markdown",
                "markdown_inline",

                -- Keep because you edit YAML sometimes and we disabled TS indent for it
                "yaml",
            }

            -- Incremental selection (Tree-sitter)
            --
            -- Keymap legend:
            --   gnn → start selection at cursor (node)
            --   grn → expand to next node
            --   grm → shrink selection (minus)
            --   grc → expand to scope / container
            --
            -- 🧠 Why these keys work well:
            --   g-prefix → text-object / motion namespace
            --   n → node
            --   r → range
            --   m → minus (shrink)
            --   c → container / scope
            --
            opts.incremental_selection = {
                enable = true,
                keymaps = {
                    init_selection = "gnn",
                    node_incremental = "grn",
                    scope_incremental = "grc",
                    node_decremental = "grm",
                },
            }

            return opts
        end,
    },
}
