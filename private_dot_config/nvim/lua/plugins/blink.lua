return {
    "saghen/blink.cmp",
    opts = function(_, opts)
        -- ---------------------------------------------------------------------
        -- Your existing configuration (unchanged)
        -- ---------------------------------------------------------------------
        opts.snippets = {
            expand = function(snippet, _)
                return LazyVim.cmp.expand(snippet)
            end,
        }

        opts.appearance = {
            use_nvim_cmp_as_default = false,
            nerd_font_variant = "mono",
        }

        opts.completion = opts.completion or {}
        opts.completion.accept = {
            auto_brackets = { enabled = true },
        }

        opts.completion.menu = {
            draw = {
                treesitter = { "lsp" },
            },
        }

        opts.completion.documentation = {
            auto_show = true,
            auto_show_delay_ms = 200,
        }

        opts.completion.ghost_text = {
            enabled = vim.g.ai_cmp,
        }

        opts.sources = {
            compat = {},
            default = { "lsp", "path", "snippets", "buffer" },
        }

        opts.cmdline = {
            enabled = true,
            keymap = {
                preset = "cmdline",
                ["<Right>"] = false,
                ["<Left>"] = false,
            },
            completion = {
                list = { selection = { preselect = false } },
                menu = {
                    auto_show = function()
                        return vim.fn.getcmdtype() == ":"
                    end,
                },
                ghost_text = { enabled = true },
            },
        }

        opts.keymap = {
            preset = "enter",
            ["<C-y>"] = { "select_and_accept" },
        }

        -- ---------------------------------------------------------------------
        -- Borders (the ONLY new addition)
        -- ---------------------------------------------------------------------
        opts.completion.menu.border = BORDER_STYLE

        opts.completion.documentation.window = {
            border = BORDER_STYLE,
        }

        opts.signature = opts.signature or {}
        opts.signature.window = {
            border = BORDER_STYLE,
        }

        return opts
    end,
}
