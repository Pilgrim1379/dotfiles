return {
  "saghen/blink.cmp",
  opts = function(_, opts)
    -- ---------------------------------------------------------------------
    -- Appearance
    -- ---------------------------------------------------------------------
    opts.appearance = opts.appearance or {}
    opts.appearance.use_nvim_cmp_as_default = false
    opts.appearance.nerd_font_variant = "mono"

    -- ---------------------------------------------------------------------
    -- Completion UI
    -- ---------------------------------------------------------------------
    opts.completion = opts.completion or {}

    -- Disable blink's experimental auto-brackets accept feature
    opts.completion.accept = opts.completion.accept or {}
    opts.completion.accept.auto_brackets = opts.completion.accept.auto_brackets or {}
    opts.completion.accept.auto_brackets.enabled = false

    -- Menu styling + border
    opts.completion.menu = opts.completion.menu or {}
    opts.completion.menu.draw = opts.completion.menu.draw or {}
    opts.completion.menu.draw.treesitter = { "lsp" }
    opts.completion.menu.border = BORDER_STYLE

    -- Docs popup + border
    opts.completion.documentation = opts.completion.documentation or {}
    opts.completion.documentation.auto_show = true
    opts.completion.documentation.auto_show_delay_ms = 200
    opts.completion.documentation.window = opts.completion.documentation.window or {}
    opts.completion.documentation.window.border = BORDER_STYLE

    -- Ghost text
    opts.completion.ghost_text = opts.completion.ghost_text or {}
    opts.completion.ghost_text.enabled = vim.g.ai_cmp

    -- Signature popup border
    opts.signature = opts.signature or {}
    opts.signature.window = opts.signature.window or {}
    opts.signature.window.border = BORDER_STYLE

    -- ---------------------------------------------------------------------
    -- Snippets: OFF by default, ON for markup-heavy filetypes
    --
    -- Why:
    -- - Some JS/TS LSPs send "template" completions as malformed snippets (e.g. {}~)
    -- - Neovim's snippet parser is strict -> errors
    -- - We keep snippets disabled for logic-heavy JS/TS, but enable them for markup
    --   where they're genuinely useful (HTML/Vue/JSX/TSX/CSS).
    -- ---------------------------------------------------------------------
    opts.sources = opts.sources or {}
    opts.sources.compat = opts.sources.compat or {}

    -- Reliable in LazyVim: disable the snippets provider globally
    opts.sources.providers = opts.sources.providers or {}
    opts.sources.providers.snippets = opts.sources.providers.snippets or {}
    opts.sources.providers.snippets.enabled = false

    -- Default sources (no snippets)
    opts.sources.default = { "lsp", "path", "buffer" }

    -- Re-enable snippets only for markup-heavy filetypes
    opts.sources.per_filetype = opts.sources.per_filetype or {}

    local with_snippets = {
      inherit_defaults = false,
      "lsp",
      "path",
      "snippets",
      "buffer",
    }

    for _, ft in ipairs({
      "html",
      "css",
      "scss",
      "javascriptreact",  -- JSX
      "typescriptreact",  -- TSX
      "vue",
      "svelte",
    }) do
      opts.sources.per_filetype[ft] = with_snippets
    end

    -- ---------------------------------------------------------------------
    -- Snippet expansion: enabled (needed for HTML/Vue/JSX/TSX productivity)
    -- Uses LazyVim's configured snippet engine.
    -- ---------------------------------------------------------------------
    opts.snippets = opts.snippets or {}
    opts.snippets.expand = function(snippet, _)
      return LazyVim.cmp.expand(snippet)
    end

    -- ---------------------------------------------------------------------
    -- Safety filter: drop snippet-like template items that caused errors
    --
    -- Notes:
    -- - Works across sources (including LSP and snippets)
    -- - Drops labels ending with "~" (e.g. {}~, #region~)
    -- - Drops LSP snippet-format items when we're NOT in markup-heavy filetypes
    --   (keeps real snippets in markup buffers)
    -- ---------------------------------------------------------------------
    local prev_transform = opts.sources.transform_items
    opts.sources.transform_items = function(ctx, items)
      if type(prev_transform) == "function" then
        items = prev_transform(ctx, items)
      end

      local ft = vim.bo.filetype
      local allow_snippets = opts.sources.per_filetype and opts.sources.per_filetype[ft] ~= nil

      local SNIPPET_KIND =
        (vim.lsp.protocol and vim.lsp.protocol.CompletionItemKind and vim.lsp.protocol.CompletionItemKind.Snippet)
        or 15

      local out = {}
      for _, item in ipairs(items or {}) do
        local label = ""
        if type(item) == "table" then
          label = item.label or item.text or item.word or ""
        end

        local insertTextFormat = item.insertTextFormat or item.insert_text_format
        local is_lsp_snippet = insertTextFormat == 2
        local is_kind_snippet = item.kind == SNIPPET_KIND
        local is_tilde_template = type(label) == "string" and label:sub(-1) == "~"

        -- Always drop the tilde templates (they were the troublemakers)
        if is_tilde_template then
          goto continue
        end

        -- If we're NOT in a markup filetype, drop snippet-format items too
        if not allow_snippets and (is_lsp_snippet or is_kind_snippet) then
          goto continue
        end

        table.insert(out, item)
        ::continue::
      end

      return out
    end

    return opts
  end,
}
