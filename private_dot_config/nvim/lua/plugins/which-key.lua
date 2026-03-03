return {
  {
    "folke/which-key.nvim",
    opts = function(_, opts)
      -- LazyVim uses opts.spec (not opts.defaults)
      opts.spec = opts.spec or {}

      -- Add a proper group for <leader>U (capital U)
      -- This makes it display like the standard LazyVim entries: icon + description.
      table.insert(opts.spec, {
        mode = { "n", "x" },
        {
          "<leader>U",
          group = "Visual Toggles",
          icon = { icon = "󰙵", color = "azure" },
        },
        {
          "<leader>Uh",
          desc = "Toggle inlay hints",
          icon = { icon = "󰞋", color = "green" },
        },
        {
          "<leader>Uo",
          desc = "Toggle comment continuation",
          icon = { icon = "󰅺", color = "yellow" },
        },
      })
    end,
  },
}
