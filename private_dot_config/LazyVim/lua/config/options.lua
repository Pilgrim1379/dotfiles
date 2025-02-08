-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here
local g = vim.g
local opt = vim.opt
local env = vim.env

-- Set Python Provider/Skip some remote provider loading
g.loaded_python_provider = 0
g.python3_host_prog = "python"
g.python_host_prog = "/usr/bin/python2"

-- Markdown highlight
g.markdown_fenced_languages = {
    "bash", "c", "json", "lua", "python", "rust", "sh", "elixir", "haskell",
    "ocaml", "elm"
}

-- GUI font
opt.guifont = "MonaspiceNe Nerd Font:h16"
-- opt.guifont = "JetBrainsMono Nerd Font:h16"
-- vim.g.lazyvim_prettier_needs_config = false

-- LSP Server to use for Python.
-- Set to "basedpyright" to use basedpyright instead of pyright.
vim.g.lazyvim_python_lsp = "basedpyright"
-- Set to "ruff_lsp" to use the old LSP implementation version.
vim.g.lazyvim_python_ruff = "ruff"
