-- lua/plugins/colorscheme.lua

local function is_macos_dark()
    -- returns "true\n" or "false\n"
    if vim.fn.has("mac") ~= 1 then
        return false
    end
    local out = vim.fn.system(
        [[osascript -e 'tell application "System Events" to tell appearance preferences to get dark mode']]
    )
    return type(out) == "string" and out:match("true") ~= nil
end

local function catppuccin_scheme()
    return is_macos_dark() and "catppuccin-mocha" or "catppuccin-latte"
end

local function apply_scheme()
    local want = catppuccin_scheme()
    if vim.g.colors_name ~= want then
        vim.cmd.colorscheme(want)
    end
end

return {
    { -- Catppuccin
        "catppuccin/nvim",
        name = "catppuccin",
        priority = 1000,
        lazy = false,
        opts = {
            background = { light = "latte", dark = "mocha" },

            -- Align diagnostic colours to Catppuccin severity palette
            -- (Error=red, Warn=yellow/peach, Info=blue, Hint=teal)
            custom_highlights = function(colors)
                return {
                    DiagnosticError = { fg = colors.red },
                    DiagnosticWarn = { fg = colors.yellow },
                    DiagnosticInfo = { fg = colors.blue },
                    DiagnosticHint = { fg = colors.teal },

                    DiagnosticVirtualTextError = { fg = colors.red },
                    DiagnosticVirtualTextWarn = { fg = colors.yellow },
                    DiagnosticVirtualTextInfo = { fg = colors.blue },
                    DiagnosticVirtualTextHint = { fg = colors.teal },

                    -- Underlines/undercurls for diagnostics (works nicely with your border/float UX)
                    DiagnosticUnderlineError = { sp = colors.red, undercurl = true },
                    DiagnosticUnderlineWarn = { sp = colors.yellow, undercurl = true },
                    DiagnosticUnderlineInfo = { sp = colors.blue, undercurl = true },
                    DiagnosticUnderlineHint = { sp = colors.teal, undercurl = true },
                }
            end,

            integrations = {
                aerial = true,
                alpha = true,
                cmp = true,
                dashboard = true,
                flash = true,
                fzf = true,
                grug_far = true,
                gitsigns = true,
                headlines = true,
                illuminate = true,
                indent_blankline = { enabled = true },
                leap = true,
                lsp_trouble = true,
                mason = true,
                markdown = true,
                mini = true,
                native_lsp = {
                    enabled = true,
                    underlines = {
                        errors = { "undercurl" },
                        hints = { "undercurl" },
                        warnings = { "undercurl" },
                        information = { "undercurl" },
                    },
                },
                navic = { enabled = true, custom_bg = "lualine" },
                neotest = true,
                neotree = true,
                noice = true,
                notify = true,
                semantic_tokens = true,
                snacks = true,
                telescope = true,
                treesitter = true,
                treesitter_context = true,
                which_key = true,
                blink_cmp = true,
            },
        },
        config = function(_, opts)
            require("catppuccin").setup(opts)

            -- Apply initial scheme
            apply_scheme()

            -- Keep it synced when you come back to Neovim (like WezTerm does live)
            vim.api.nvim_create_autocmd({ "VimEnter", "FocusGained" }, {
                callback = apply_scheme,
            })
        end,
    },

    { -- Tell LazyVim to use whatever Catppuccin variant we chose
        "LazyVim/LazyVim",
        opts = function(_, o)
            o.colorscheme = catppuccin_scheme()
        end,
    },

    { -- bufferline: apply catppuccin theme when catppuccin is active
        "akinsho/bufferline.nvim",
        optional = true,
        opts = function(_, o)
            if (vim.g.colors_name or ""):find("catppuccin") then
                o.highlights = require("catppuccin.special.bufferline").get_theme()
            end
        end,
    },
}
