-- Set border of some LazyVim plugins to rounded
return {
    -- lazyvim.plugins.coding
    -- lazyvim.plugins.editor
    { "mason.nvim",     opts = { ui = { border = BORDER_STYLE } } },
    { "which-key.nvim", opts = { win = { border = BORDER_STYLE } } },
    { "gitsigns.nvim",  opts = { preview_config = { border = BORDER_STYLE } } },
    -- lazyvim.plugins.lsp
}
