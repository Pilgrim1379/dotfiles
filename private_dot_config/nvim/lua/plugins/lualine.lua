return {
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
    dependencies = {
        "nvim-tree/nvim-web-devicons",
    },
    opts = {
        options = {
            -- catppuccin-nvim theme follows mocha/latte automatically
            -- because colorscheme.lua sets the variant before lualine loads.
            theme                = "catppuccin-nvim",
            component_separators = { left = "", right = "" },
            section_separators   = { left = "", right = "" },
            -- NOTE: globalstatus removed — LazyVim sets it to true by default.
            -- Keeping it here was harmless but redundant.
        },
    },
}
