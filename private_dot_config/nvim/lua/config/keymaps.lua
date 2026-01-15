-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

vim.keymap.set("n", "<leader>pw", function()
    vim.notify(vim.fn.getcwd())
end, { desc = "Show working directory" })


-- jk / jj: close blink completion if open; otherwise exit insert mode
-- - timeout-safe: expr + nowait (no 'j' lag)
-- - "restricted": only acts in normal modifiable buffers (otherwise inserts the keys)

local function t(keys)
    return vim.api.nvim_replace_termcodes(keys, true, false, true)
end

-- Return true if we're in a normal editable buffer
local function is_real_insert_buffer()
    return vim.bo.buftype == "" and vim.bo.modifiable
end

local function blink_menu_visible(blink)
    -- blink.cmp has changed API names over time, so check a few safely
    if type(blink.is_visible) == "function" then
        return blink.is_visible()
    end
    if type(blink.visible) == "function" then
        return blink.visible()
    end
    return false
end

local function blink_hide(blink)
    -- try common hide/close function names
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
        -- blink sometimes exposes methods on blink_mod.cmp, sometimes on blink_mod directly
        local cmp = blink_mod.cmp or blink_mod

        -- Robust "is menu visible?" check
        local visible = type(cmp.is_visible) == "function" and cmp.is_visible()

        if visible then
            -- 1) Ask blink to hide if it supports it
            if type(cmp.hide) == "function" then
                cmp.hide()
            elseif type(cmp.close) == "function" then
                cmp.close()
            end

            -- 2) Also send <C-e> which closes the completion menu reliably
            vim.api.nvim_feedkeys(t("<C-e>"), "n", false)

            -- Visual cue (goes through your Noice msg_show UI)
            -- vim.api.nvim_echo({ { "󰒲 completion dismissed", "Comment" } }, false, {})
            -- vim.api.nvim_echo({ { "󰒲 dismissed", "DiagnosticHint" } }, false, {})
            -- Subtle visual cue
            vim.api.nvim_echo({ { "󰒲", "DiagnosticHint" } }, false, {})

            -- -- Stay in insert mode
            -- return ""

            -- ✅ Close completion menu, then leave insert mode
            return t("<Esc>")
        end
    end

    -- No completion menu → exit insert mode
    return t("<Esc>")
end


vim.keymap.set("i", "jk", function() return jk_handler("jk") end,
    { expr = true, nowait = true, desc = "jk: dismiss completion / escape" })
vim.keymap.set("i", "jj", function() return jk_handler("jj") end,
    { expr = true, nowait = true, desc = "jj: dismiss completion / escape" })

-- UI toggle: comment continuation (buffer-local)
vim.keymap.set(
    "n",
    "<leader>uo",
    "<cmd>ToggleCommentContinuation<cr>",
    { desc = "Toggle comment continuation" }
)
