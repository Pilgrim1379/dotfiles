-- Options are automatically loaded before lazy.nvim startup
-- Default options:
-- https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua

local g                     = vim.g
local opt                   = vim.opt

-- Key timing
opt.timeoutlen              = 300

-- Format on save (Conform handles the actual formatting via conform.lua)
g.autoformat                = true

-- Indentation — safe defaults; overridden per filetype by tree-sitter / editorconfig
opt.expandtab               = true
opt.shiftwidth              = 4
opt.tabstop                 = 4
opt.smartindent             = true

-- Python host (used by plugins that shell out to python3, e.g. pynvim)
g.python3_host_prog         = vim.fn.exepath("python3")

-- Markdown fenced code block syntax highlighting
g.markdown_fenced_languages = {
    "bash", "c", "json", "lua", "python", "rust", "sh",
    "elixir", "haskell", "ocaml", "elm",
}

-- NOTE: showtabline = 2 removed.
-- bufferline.lua sets always_show_bufferline = true, which is the plugin-level
-- equivalent. Duplicating the vim option is harmless but redundant.
