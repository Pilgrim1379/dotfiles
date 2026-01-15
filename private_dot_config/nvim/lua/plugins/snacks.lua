return {
    {
        "folke/snacks.nvim",
        opts = function(_, opts)
            -- Keep your existing options
            opts.notifier = opts.notifier or {}
            opts.notifier.timeout = opts.notifier.timeout or 5000

            -- Add consistent border styling
            opts.win = opts.win or {}
            opts.win.border = BORDER_STYLE

            return opts
        end,
    },
}
