-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set:
-- https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua

vim.keymap.set("n", "<leader>pw", function()
    vim.notify(vim.fn.getcwd())
end, { desc = "Show working directory" })

-- ---------------------------------------------------------------------------
-- jk / jj: close blink.cmp if open; otherwise exit insert mode
--
-- Design goals:
-- - No 'j' lag: uses expr + nowait
-- - Only acts in real, editable buffers (avoids clobbering popups/terminals)
-- - Gracefully degrades if blink.cmp isn't loaded
-- ---------------------------------------------------------------------------
local function t(keys)
    return vim.api.nvim_replace_termcodes(keys, true, false, true)
end

local function is_real_insert_buffer()
    return vim.bo.buftype == "" and vim.bo.modifiable
end

local function blink_menu_visible(blink)
    -- blink.cmp has changed API names over versions — check both defensively
    if type(blink.is_visible) == "function" then
        return blink.is_visible()
    end
    if type(blink.visible) == "function" then
        return blink.visible()
    end
    return false
end

local function blink_hide(blink)
    if type(blink.hide) == "function" then
        blink.hide()
        return
    end
    if type(blink.close) == "function" then
        blink.close()
        return
    end
end

local function jk_handler(lhs)
    -- Don't mess with special plugin buffers: just insert what you typed
    if not is_real_insert_buffer() then
        return lhs
    end

    local ok, blink_mod = pcall(require, "blink.cmp")
    if ok and blink_mod then
        local cmp = blink_mod.cmp or blink_mod

        if blink_menu_visible(cmp) then
            -- Close completion menu first (best-effort), then escape.
            -- Note: we do NOT return "" to keep insert mode — it traps the
            -- cursor inside message boxes when blink is open there.
            blink_hide(cmp)
            vim.api.nvim_feedkeys(t("<C-e>"), "n", false)
            return t("<Esc>")
        end
    end

    -- No completion menu open → just exit insert mode
    return t("<Esc>")
end

vim.keymap.set("i", "jk", function() return jk_handler("jk") end,
    { expr = true, nowait = true, desc = "jk: dismiss completion or escape" })
vim.keymap.set("i", "jj", function() return jk_handler("jj") end,
    { expr = true, nowait = true, desc = "jj: dismiss completion or escape" })

-- Select all text in buffer
vim.keymap.set("n", "<leader>va", "ggVG", { desc = "Select all" })
-- ---------------------------------------------------------------------------
-- UI toggles (LazyVim <leader>U group)
-- These commands are defined in autocmds.lua; keymaps live here.
-- ---------------------------------------------------------------------------

-- Toggle comment auto-continuation for the current buffer (default: OFF)
vim.keymap.set("n", "<leader>Uo", "<cmd>ToggleCommentContinuation<cr>",
    { desc = "Comment continuation" })

-- Toggle LSP inlay hints for the current buffer
vim.keymap.set("n", "<leader>Uh", "<cmd>ToggleInlayHints<cr>",
    { desc = "Inlay hints" })

-- ---------------------------------------------------------------------------
-- <C-l>: jump over the next closing delimiter in insert mode
--
-- Autopairs inserts the closing bracket/quote ahead of the cursor.
-- This lets you hop over it without reaching for the arrow keys.
-- Falls back to Neovim's default <C-l> (screen redraw) if no closer ahead.
-- ---------------------------------------------------------------------------
vim.keymap.set("i", "<C-l>", function()
    local col = vim.api.nvim_win_get_cursor(0)[2]
    local line = vim.api.nvim_get_current_line()
    local nextc = line:sub(col + 1, col + 1)

    if nextc:match('[%]%})%>"\'`]') then
        return "<Right>"
    end

    return "<C-l>"
end, {
    expr = true,
    replace_keycodes = true,
    desc = "Jump over closing delimiter",
})

-- ---------------------------------------------------------------------------
-- mini.snippets navigation (insert mode)
--
-- <C-j>  → next snippet placeholder
-- <C-k>  → previous snippet placeholder
--
-- IMPORTANT: <C-k> insert-mode is ALSO claimed by LazyVim's LSP keymaps
-- (signature help). We disable that binding in lspconfig.lua ["*"] keys
-- so that this one wins. Signature help remains available via gK in normal
-- mode (also defined in lspconfig.lua).
-- ---------------------------------------------------------------------------
vim.keymap.set("i", "<C-j>", function()
    local ms = require("mini.snippets")
    if ms.session.get() then
        ms.session.jump(1)
    end
end, { desc = "Next snippet placeholder" })

vim.keymap.set("i", "<C-k>", function()
    local ms = require("mini.snippets")
    if ms.session.get() then
        ms.session.jump(-1)
    end
end, { desc = "Prev snippet placeholder" })

-- ---------------------------------------------------------------------------
-- Optional: jump over ALL consecutive closing delimiters in one press.
-- Uncomment to replace the single-closer <C-l> above.
-- ---------------------------------------------------------------------------
-- vim.keymap.set("i", "<C-l>", function()
--   local col = vim.api.nvim_win_get_cursor(0)[2]
--   local line = vim.api.nvim_get_current_line()
--   local out = {}
--
--   while true do
--     local nextc = line:sub(col + 1, col + 1)
--     if nextc == "" or not nextc:match('[%]%})%>"\']') then
--       break
--     end
--     table.insert(out, "<Right>")
--     col = col + 1
--   end
--
--   return #out > 0 and table.concat(out) or "<C-l>"
-- end, {
--   expr = true,
--   replace_keycodes = true,
--   desc = "Jump over closing delimiters",
-- })
