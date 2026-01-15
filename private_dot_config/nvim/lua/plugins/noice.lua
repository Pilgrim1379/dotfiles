return {
    {
        "folke/noice.nvim",
        opts = {
            messages = {
                view = "bottom",
            },

            views = {
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

                    timeout = 5000,
                },

                -- ✅ THIS is the important addition
                lsp_doc = {
                    border = {
                        style = BORDER_STYLE,
                    },
                },
            },

            presets = {
                lsp_doc_border = true,
            },

            routes = {
                {
                    filter = {
                        event = "msg_show",
                        find = "completion dismissed",
                    },
                    opts = {
                        timeout = 300,
                    },
                },
            },
        },
    },
}
