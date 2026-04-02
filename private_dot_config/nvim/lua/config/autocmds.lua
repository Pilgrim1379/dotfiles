-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds:
-- https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua

local api = vim.api

-- ---------------------------------------------------------------------------
-- UI toggle notifier helper
-- Used by ToggleCommentContinuation and ToggleInlayHints below.
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
-- Without this, the block cursor persists in the terminal after Neovim exits.
-- "a:ver1" = thin vertical bar across all modes.
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
--
-- Neovim's default formatoptions include c, r, o which auto-insert the
-- comment leader when you press Enter or o/O inside a comment. This is
-- often annoying (extra slashes, quote marks). We remove them on every
-- BufEnter and expose a toggle to restore them per-buffer if needed.
-- ---------------------------------------------------------------------------
do
    local group = api.nvim_create_augroup("disable_comment_continuation", { clear = true })

    api.nvim_create_autocmd("BufEnter", {
        group = group,
        callback = function(args)
            vim.opt_local.formatoptions:remove({ "c", "r", "o" })
            -- Track state so the toggle command below starts in the right state
            vim.b[args.buf].cc_disabled = true
        end,
    })
end

-- ---------------------------------------------------------------------------
-- Toggle comment continuation per-buffer
-- Default: OFF (we strip c/r/o on BufEnter above)
-- Toggle restores the buffer's original formatoptions.
-- Keymap: <leader>Uo (defined in keymaps.lua)
-- ---------------------------------------------------------------------------
do
    api.nvim_create_user_command("ToggleCommentContinuation", function()
        local bufnr = api.nvim_get_current_buf()

        -- Skip special buffers (terminals, quickfix, help, etc.)
        if vim.bo[bufnr].buftype ~= "" then
            return
        end

        -- Save the original formatoptions once per buffer (before we stripped c/r/o)
        if vim.b[bufnr].fo_before_cc_disable == nil then
            vim.b[bufnr].fo_before_cc_disable = vim.bo[bufnr].formatoptions
        end

        -- Initialise state flag (default OFF because BufEnter removes c/r/o)
        if vim.b[bufnr].cc_disabled == nil then
            vim.b[bufnr].cc_disabled = true
        end

        if vim.b[bufnr].cc_disabled then
            -- Turn ON: restore the original formatoptions snapshot
            local orig = vim.b[bufnr].fo_before_cc_disable
            if type(orig) == "string" and orig ~= "" then
                vim.bo[bufnr].formatoptions = orig
            else
                vim.opt_local.formatoptions:append({ "c", "r", "o" })
            end
            vim.b[bufnr].cc_disabled = false
            notify_toggle("󰅺", "Comment continuation", true)
        else
            -- Turn OFF: remove c/r/o again
            vim.opt_local.formatoptions:remove({ "c", "r", "o" })
            vim.b[bufnr].cc_disabled = true
            notify_toggle("󰅺", "Comment continuation", false)
        end
    end, { desc = "Toggle automatic comment continuation for current buffer" })
    -- NOTE: the <leader>Uo keymap lives in keymaps.lua — not duplicated here.
end

-- ---------------------------------------------------------------------------
-- LspAttach tweaks
--
-- Why disable formatting capabilities here?
-- Conform owns all formatting (see conform.lua). If we leave the LSP
-- formatting providers enabled, :Format and format-on-save could use the
-- LSP instead of Conform, giving inconsistent results.
--
-- Why disable Ruff hover?
-- Ty (the type checker) provides richer hover with type info. Ruff's
-- hover is redundant and creates duplicate popups.
--
-- NOTE: No basedpyright checks — it is explicitly disabled (= false) in
-- lspconfig.lua, so it will never attach. Removing dead guard clauses.
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

            -- Ruff should NOT provide hover — Ty has richer type-hover
            if client.name == "ruff" then
                client.server_capabilities.hoverProvider = false
            end

            -- Ruff: let Conform own formatting (ruff + ruff_format via conform.lua)
            if client.name == "ruff" then
                client.server_capabilities.documentFormattingProvider = false
                client.server_capabilities.documentRangeFormattingProvider = false
            end

            -- vtsls: let Conform (prettierd/prettier) own JS/TS formatting
            if client.name == "vtsls" then
                client.server_capabilities.documentFormattingProvider = false
                client.server_capabilities.documentRangeFormattingProvider = false
            end
        end,
    })
end

-- ---------------------------------------------------------------------------
-- Automatically create missing parent directories on save
-- Useful when editing a new file at a path that doesn't exist yet.
-- Skips protocol URLs (e.g. oil://, fugitive://).
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
-- Requires Neovim 0.10+ and server support (ty, lua_ls, etc.)
-- Keymap: <leader>Uh (defined in keymaps.lua)
-- ---------------------------------------------------------------------------
do
    api.nvim_create_user_command("ToggleInlayHints", function()
        local bufnr = api.nvim_get_current_buf()
        local enabled = vim.lsp.inlay_hint.is_enabled({ bufnr = bufnr })
        vim.lsp.inlay_hint.enable(not enabled, { bufnr = bufnr })
        notify_toggle("󰞋", "Inlay hints", not enabled)
    end, { desc = "Toggle LSP inlay hints (type hints) for current buffer" })
    -- NOTE: the <leader>Uh keymap lives in keymaps.lua — not duplicated here.
end
