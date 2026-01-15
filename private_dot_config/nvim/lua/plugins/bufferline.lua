return {
  {
    "akinsho/bufferline.nvim",
    opts = function(_, opts)
      opts.options = opts.options or {}

      -- Always show the bufferline, even with a single buffer
      opts.options.always_show_bufferline = true

      -- Visual indicator for unsaved buffers
      -- Pick any icon you like: "●", "", "✱", etc.
      opts.options.modified_icon = "●"

      -- Optional: make it more obvious by keeping the indicator visible
      -- (Some people prefer an underline indicator; this keeps defaults but ensures it's on)
      opts.options.indicator = opts.options.indicator or {}
      opts.options.indicator.style = "icon"

      return opts
    end,
  },
}
