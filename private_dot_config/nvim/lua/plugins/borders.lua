-- Set border of some LazyVim plugins to rounded
return {
    -- lazyvim.plugins.coding
    -- lazyvim.plugins.editor
    {"mason.nvim", opts = {ui = {border = BORDER_STYLE}}},
    {"which-key.nvim", opts = {win = {border = BORDER_STYLE}}},
    {"gitsigns.nvim", opts = {preview_config = {border = BORDER_STYLE}}},
    -- lazyvim.plugins.lsp
    {
        "nvim-lspconfig",
        opts = function(_, opts)
            -- Set LspInfo border
            require("lspconfig.ui.windows").default_options.border =
                BORDER_STYLE
            return opts
        end
    }, -- {"none-ls.nvim", opts = {border = BORDER_STYLE}},
    {"snacks.nvim", opts = {win = {border = BORDER_STYLE}}}, {
        "saghen/blink.cmp",
        opts = {
            completion = {
                menu = {border = 'single'},
                documentation = {window = {border = 'single'}}
            },
            signature = {window = {border = 'single'}}
        }
    }
}
