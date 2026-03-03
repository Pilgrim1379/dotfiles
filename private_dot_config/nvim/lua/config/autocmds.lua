-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds:
-- https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua

local api = vim.api

-- ---------------------------------------------------------------------------
-- UI toggle notifier helper
-- ---------------------------------------------------------------------------
local function notify_toggle(icon, label, enabled, scope)
    scope = scope or "this buffer"
    vim.notify(
        string.format("%s %s: %s (%s)", icon, label, enabled and "ON" or "OFF", scope),
        vim.log.levels.INFO
    )
end

-- ---------------------------------------------------------------------------
-- Diagnostics float on CursorHold / CursorHoldI
-- Uses global BORDER_STYLE from init.lua
-- ---------------------------------------------------------------------------
do
    local group = api.nvim_create_augroup("user_diagnostic_float", { clear = true })

    api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
        group = group,
        callback = function()
            vim.diagnostic.open_float(nil, {
                focus = false,
                border = BORDER_STYLE,
                scope = "cursor",
            })
        end,
    })
end

-- ---------------------------------------------------------------------------
-- Disable Copilot on startup
-- ---------------------------------------------------------------------------
vim.cmd("silent! Copilot disable")

-- ---------------------------------------------------------------------------
-- Restore terminal cursor shape on exit or suspend
-- ---------------------------------------------------------------------------
do
    local group = api.nvim_create_augroup("restore_cursor_shape_on_exit", { clear = true })

    api.nvim_create_autocmd({ "VimLeave", "VimSuspend" }, {
        group = group,
        callback = function()
            vim.opt.guicursor = "a:ver1"
        end,
    })
end

-- ---------------------------------------------------------------------------
-- Disable automatic comment continuation (default: OFF)
-- Fixes extra quotes / comment leaders in multiline comments
-- ---------------------------------------------------------------------------
do
    local group = api.nvim_create_augroup("disable_comment_continuation", { clear = true })

    api.nvim_create_autocmd("BufEnter", {
        group = group,
        callback = function(args)
            vim.opt_local.formatoptions:remove({ "c", "r", "o" })
            -- Track state so the toggle is consistent
            vim.b[args.buf].cc_disabled = true
        end,
    })
end

-- ---------------------------------------------------------------------------
-- Toggle comment continuation per-buffer
-- Default: OFF (we remove c/r/o on BufEnter)
-- Toggle: restores original formatoptions for this buffer
-- ---------------------------------------------------------------------------
do
    api.nvim_create_user_command("ToggleCommentContinuation", function()
        local bufnr = api.nvim_get_current_buf()

        -- Skip special buffers
        if vim.bo[bufnr].buftype ~= "" then
            return
        end

        -- Save original formatoptions once per buffer
        if vim.b[bufnr].fo_before_cc_disable == nil then
            vim.b[bufnr].fo_before_cc_disable = vim.bo[bufnr].formatoptions
        end

        -- Our state flag (default OFF because BufEnter removes c/r/o)
        if vim.b[bufnr].cc_disabled == nil then
            vim.b[bufnr].cc_disabled = true
        end

        if vim.b[bufnr].cc_disabled then
            -- Turn ON: restore original formatoptions
            local orig = vim.b[bufnr].fo_before_cc_disable
            if type(orig) == "string" and orig ~= "" then
                vim.bo[bufnr].formatoptions = orig
            else
                vim.opt_local.formatoptions:append({ "c", "r", "o" })
            end
            vim.b[bufnr].cc_disabled = false
            notify_toggle("󰅺", "Comment continuation", true)
        else
            -- Turn OFF: remove c/r/o
            vim.opt_local.formatoptions:remove({ "c", "r", "o" })
            vim.b[bufnr].cc_disabled = true
            notify_toggle("󰅺", "Comment continuation", false)
        end
    end, { desc = "Toggle automatic comment continuation for current buffer" })

    -- UI toggle mapping (LazyVim-style, no known conflicts)
    vim.keymap.set("n", "<leader>Uo", "<cmd>ToggleCommentContinuation<cr>", {
        desc = "Comment continuation",
    })
end

-- ---------------------------------------------------------------------------
-- LspAttach tweaks:
-- - Disable Ruff hover provider (so basedpyright owns hover + type info)
-- - Disable LSP formatting for ruff + basedpyright (Conform owns formatting)
-- - Disable LSP formatting for vtsls (Prettier via Conform owns JS/TS formatting)
-- ---------------------------------------------------------------------------
do
  local group = api.nvim_create_augroup("lsp_attach_tweaks", { clear = true })

  api.nvim_create_autocmd("LspAttach", {
    group = group,
    callback = function(args)
      local client = vim.lsp.get_client_by_id(args.data.client_id)
      if not client then
        return
      end

      -- Ruff should NOT provide hover (basedpyright has richer type hover)
      if client.name == "ruff" then
        client.server_capabilities.hoverProvider = false
      end

      -- Ensure Conform always owns formatting for Python
      if client.name == "basedpyright" or client.name == "ruff" then
        client.server_capabilities.documentFormattingProvider = false
        client.server_capabilities.documentRangeFormattingProvider = false
      end

      -- Ensure Conform (prettierd/prettier) owns formatting for JS/TS
      if client.name == "vtsls" then
        client.server_capabilities.documentFormattingProvider = false
        client.server_capabilities.documentRangeFormattingProvider = false
      end
    end,
  })
end

-- ---------------------------------------------------------------------------
-- Ensure basedpyright attaches to ALL python buffers
-- ---------------------------------------------------------------------------
do
    local group = api.nvim_create_augroup("auto_attach_basedpyright", { clear = true })

    api.nvim_create_autocmd("FileType", {
        group = group,
        pattern = "python",
        callback = function(args)
            local bufnr = args.buf

            if vim.bo[bufnr].buftype ~= "" then
                return
            end

            vim.schedule(function()
                for _, c in ipairs(vim.lsp.get_clients({ bufnr = bufnr })) do
                    if c.name == "basedpyright" then
                        return
                    end
                end

                for _, c in ipairs(vim.lsp.get_clients()) do
                    if c.name == "basedpyright" then
                        pcall(vim.lsp.buf_attach_client, bufnr, c.id)
                        return
                    end
                end

                vim.cmd("silent! LspStart basedpyright")
            end)
        end,
    })
end

-- ---------------------------------------------------------------------------
-- Highlight yanked text
-- ---------------------------------------------------------------------------
do
    local group = api.nvim_create_augroup("user_highlight_yank", { clear = true })

    api.nvim_create_autocmd("TextYankPost", {
        group = group,
        callback = function()
            vim.highlight.on_yank()
        end,
    })
end

-- ---------------------------------------------------------------------------
-- Automatically create missing directories on save
-- ---------------------------------------------------------------------------
do
    local group = api.nvim_create_augroup("user_auto_mkdir", { clear = true })

    api.nvim_create_autocmd("BufWritePre", {
        group = group,
        callback = function(args)
            local file = args.match
            if file == "" or file:match("^%a+://") then
                return
            end

            local dir = vim.fn.fnamemodify(file, ":p:h")
            if vim.fn.isdirectory(dir) == 0 then
                vim.fn.mkdir(dir, "p")
            end
        end,
    })
end

-- ---------------------------------------------------------------------------
-- Toggle LSP inlay hints (type hints) per-buffer
-- ---------------------------------------------------------------------------
do
    api.nvim_create_user_command("ToggleInlayHints", function()
        local bufnr = api.nvim_get_current_buf()

        -- Neovim 0.10+ API: get/set per-buffer enablement
        local enabled = vim.lsp.inlay_hint.is_enabled({ bufnr = bufnr })
        vim.lsp.inlay_hint.enable(not enabled, { bufnr = bufnr })

        notify_toggle("󰞋", "Inlay hints", not enabled)
    end, { desc = "Toggle LSP inlay hints (type hints) for current buffer" })

    -- LazyVim UI toggle group: <leader>U...
    vim.keymap.set("n", "<leader>Uh", "<cmd>ToggleInlayHints<cr>", {
        desc = "Inlay hints",
    })
end
