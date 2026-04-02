local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
    local lazyrepo = "https://github.com/folke/lazy.nvim.git"
    local out = vim.fn.system({
        "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo,
        lazypath
    })
    if vim.v.shell_error ~= 0 then
        vim.api.nvim_echo({
            { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
            { out,                            "WarningMsg" },
            { "\nPress any key to exit..." }
        }, true, {})
        vim.fn.getchar()
        os.exit(1)
    end
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
    spec = {
        -- LazyVim core + its bundled plugin specs
        { "LazyVim/LazyVim", import = "lazyvim.plugins" },
        -- Your personal plugin specs (lua/plugins/)
        { import = "plugins" },
    },
    defaults = {
        -- Custom plugins load eagerly by default (lazy = false).
        -- Set to true only if you are confident every plugin handles lazy-loading itself.
        lazy = false,
        -- Always use the latest git commit rather than pinned release tags.
        -- Pinned releases (version = "*") are often stale and break things.
        version = false,
    },
    -- NOTE: tokyonight removed from install colorschemes.
    -- It was never used — catppuccin is the active scheme.
    -- Keeping unused entries here means lazy.nvim tries to install them.
    install = { colorscheme = { "catppuccin" } },
    checker = {
        enabled = true,  -- check for plugin updates in the background
        notify  = false, -- don't pop a notification on every update check
    },
    performance = {
        rtp = {
            -- Disable unused built-in Neovim RTP plugins for faster startup
            disabled_plugins = {
                "gzip",
                -- "matchit",    -- uncomment if you don't use matchit
                -- "matchparen", -- uncomment if treesitter handles brackets
                -- "netrwPlugin",-- uncomment if you use oil.nvim or similar
                "tarPlugin",
                "tohtml",
                "tutor",
                "zipPlugin",
            },
        },
    },
    ui = { border = BORDER_STYLE },
})
