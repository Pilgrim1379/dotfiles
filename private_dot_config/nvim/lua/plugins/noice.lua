return {
    {
        "folke/noice.nvim",
        opts = {
            messages = {
                view = "bottom",
            },

            views = {
                -- Custom "bottom" view: popup above the command line with a 2-line
                -- safe zone so messages don't overlap what you're typing.
                bottom = {
                    backend = "popup",
                    position = { row = -2, col = 0 },
                    size = {
                        width = "96%",
                        height = "auto",
                    },
                    border = {
                        style = BORDER_STYLE,
                        padding = { 0, 1 },
                    },
                    win_options = {
                        wrap = true,
                        winhighlight = "Normal:Normal,FloatBorder:FloatBorder",
                    },
                    timeout = 3000,
                },

                -- NOTE: lsp_doc border removed here.
                -- The lsp_doc_border = true preset (below) already sets a border
                -- on the LSP documentation popup. Defining lsp_doc in views too
                -- was redundant.
            },

            presets = {
                -- Adds a border to the LSP hover/signature docs popup.
                -- This is the canonical way to set it; no need to override the view.
                lsp_doc_border = true,
            },
        },
    },
}
