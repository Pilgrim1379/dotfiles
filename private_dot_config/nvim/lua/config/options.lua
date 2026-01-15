-- Options are automatically loaded before lazy.nvim startup
-- Default options:
-- https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua

local g = vim.g
local opt = vim.opt

-- Key timing
opt.timeoutlen = 300

-- Formatting
g.autoformat = true

-- UI
opt.showtabline = 2 -- always show bufferline

-- Indentation (safe defaults)
opt.expandtab = true
opt.shiftwidth = 4
opt.tabstop = 4
opt.smartindent = true

-- Python provider (modern)
g.python3_host_prog = vim.fn.exepath("python3")

-- Markdown fenced code highlighting
g.markdown_fenced_languages = {
    "bash", "c", "json", "lua", "python", "rust", "sh",
    "elixir", "haskell", "ocaml", "elm",
}

-- LazyVim Python tooling
g.lazyvim_python_lsp = "basedpyright"
g.lazyvim_python_ruff = "ruff"
