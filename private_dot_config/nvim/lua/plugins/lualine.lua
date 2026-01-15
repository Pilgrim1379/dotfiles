return {
    "nvim-lualine/lualine.nvim",
    event = "VeryLazy",
    dependencies = {
        "nvim-tree/nvim-web-devicons", -- optional, but recommended for icons
    },
    opts = {
        options = {
            theme = "catppuccin", -- follows catppuccin-mocha/latte automatically
            component_separators = { left = "", right = "" },
            section_separators = { left = "", right = "" },
            globalstatus = true, -- nice modern single statusline (optional)
        },
    },
}
