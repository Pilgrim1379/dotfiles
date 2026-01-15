return {
  { "nvim-mini/mini.pairs", enabled = false },

  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    config = function()
      local npairs = require("nvim-autopairs")
      local Rule = require("nvim-autopairs.rule")
      local cond = require("nvim-autopairs.conds")

      npairs.setup({ check_ts = true })

      -- ---------------------------------------------------------------------
      -- Prevent generic quote autopairs from interfering with triple quotes in Python
      -- (This is what causes the "extra quotes" on auto-close.)
      -- ---------------------------------------------------------------------
      local dq = npairs.get_rule('"')
      if dq then
        dq.not_filetypes = dq.not_filetypes or {}
        if not vim.tbl_contains(dq.not_filetypes, "python") then
          table.insert(dq.not_filetypes, "python")
        end
      end

      local sq = npairs.get_rule("'")
      if sq then
        sq.not_filetypes = sq.not_filetypes or {}
        if not vim.tbl_contains(sq.not_filetypes, "python") then
          table.insert(sq.not_filetypes, "python")
        end
      end

      -- ---------------------------------------------------------------------
      -- Python triple quotes (both kinds)
      -- ---------------------------------------------------------------------
      npairs.add_rules({
        Rule('"""', '"""', "python")
            :with_pair(cond.not_after_text('"""'))
            :with_move(cond.after_text('"""')),

        Rule("'''", "'''", "python")
            :with_pair(cond.not_after_text("'''"))
            :with_move(cond.after_text("'''")),
      })
    end,
  },
}
