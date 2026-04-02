-- Ref: https://github.com/loichyan/dotfiles/tree/lazyvim/config/nvim
-- Ref: https://github.com/elijahmanor/dotfiles
BORDER_STYLE = "single"

-- LazyVim Python tooling
-- vim.g.lazyvim_python_lsp = "basedpyright"
vim.g.lazyvim_python_lsp = "ty"
vim.g.lazyvim_python_ruff = "ruff"

-- bootstrap lazy.nvim, LazyVim and your plugins
require("config.lazy")
