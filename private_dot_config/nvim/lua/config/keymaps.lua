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
        local cmp = blink_mod.cmp or blink_mod

        if blink_menu_visible(cmp) then
            -- Close completion menu first (best-effort)...
            blink_hide(cmp)
            vim.api.nvim_feedkeys(t("<C-e>"), "n", false)

            -- return "" -- keep insert mode | This traps cursor in message boxes sometimes.

            -- ...then ESC out of everything
            return t("<Esc>")
        end
    end

    -- No completion menu → exit insert mode
    return t("<Esc>")
end

vim.keymap.set("i", "jk", function() return jk_handler("jk") end,
    { expr = true, nowait = true, desc = "jk: dismiss completion or escape" })
vim.keymap.set("i", "jj", function() return jk_handler("jj") end,
    { expr = true, nowait = true, desc = "jk: dismiss completion or escape" })

-- UI toggle: comment continuation (buffer-local)
vim.keymap.set(
    "n",
    "<leader>Uo",
    "<cmd>ToggleCommentContinuation<cr>",
    { desc = "Comment continuation" }
)

-- Jump over closing delimiters in Insert mode (when autopairs already inserted them)
vim.keymap.set("i", "<C-l>", function()
  local col = vim.api.nvim_win_get_cursor(0)[2]
  local line = vim.api.nvim_get_current_line()
  local nextc = line:sub(col + 1, col + 1)

  -- If next char is a closing delimiter, move over it
  if nextc:match('[%]%})%>"\'`]') then
    return "<Right>"
  end

  -- Otherwise keep Ctrl-L's normal behavior (redraw)
  return "<C-l>"
end, {
  expr = true,
  replace_keycodes = true,
  desc = "Jump over closing delimiter",
})

-- -- Optional upgrade: jump over multiple closers in one press
-- vim.keymap.set("i", "<C-l>", function()
--   local col = vim.api.nvim_win_get_cursor(0)[2]
--   local line = vim.api.nvim_get_current_line()
--   local out = {}

--   while true do
--     local nextc = line:sub(col + 1, col + 1)
--     if nextc == "" or not nextc:match('[%]%})%>"\']') then
--       break
--     end
--     table.insert(out, "<Right>")
--     col = col + 1
--   end

--   return #out > 0 and table.concat(out) or "<C-l>"
-- end, {
--   expr = true,
--   replace_keycodes = true,
--   desc = "Jump over closing delimiters",
-- })
--
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
