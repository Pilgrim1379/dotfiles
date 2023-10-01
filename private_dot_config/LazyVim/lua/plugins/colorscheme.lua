-- https://github.com/jascha030/macos-nvim-dark-mode
local os_is_dark = function()
    return (vim.call('system',
                     [[echo $(defaults read -globalDomain AppleInterfaceStyle &> /dev/null && echo 'dark' || echo 'light')]])):find(
               'dark') ~= nil
end

return {
    "catppuccin/nvim",
    lazy = true,
    name = "catppuccin",
    opts = {
        transparent_background = true,
        integrations = {
            alpha = true,
            cmp = true,
            flash = true,
            gitsigns = true,
            illuminate = true,
            indent_blankline = {enabled = true},
            lsp_trouble = true,
            mason = true,
            mini = true,
            native_lsp = {
                enabled = true,
                underlines = {
                    errors = {"undercurl"},
                    hints = {"undercurl"},
                    warnings = {"undercurl"},
                    information = {"undercurl"}
                }
            },
            navic = {enabled = true, custom_bg = "lualine"},
            neotest = true,
            noice = true,
            notify = true,
            neotree = true,
            semantic_tokens = true,
            telescope = true,
            treesitter = true,
            which_key = true
        }
    },
    {
        "LazyVim/LazyVim",
        opts = function(_, opts)
            if os_is_dark() then
                -- colorscheme catppuccin " catppuccin-latte, catppuccin-frappe, catppuccin-macchiato, catppuccin-mocha
                opts.colorscheme = 'catppuccin-mocha'
            else
                opts.colorscheme = 'catppuccin-latte'
            end
        end
    }
}

