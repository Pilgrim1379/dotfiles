return {
    "neovim/nvim-lspconfig",
    opts = {
        -- options for vim.diagnostic.config()
        diagnostics = {
            underline = true,
            update_in_insert = false,
            float = { border = BORDER_STYLE },
            virtual_text = false,
            --   virtual_text = {
            --     spacing = 4,
            --     source = "if_many",
            --     prefix = "●",
            --     -- this will set set the prefix to a function that returns the diagnostics icon based on the severity
            --     -- this only works on a recent 0.10.0 build. Will be set to "●" when not supported
            --     -- prefix = "icons",
            --   },
            severity_sort = true,
        },
        -- Enable this to enable the builtin LSP inlay hints on Neovim >= 0.10.0
        -- Be aware that you also will need to properly configure your LSP server to
        -- provide the inlay hints.
        inlay_hints = { enabled = false },
        -- add any global capabilities here
        capabilities = {},
        -- options for vim.lsp.buf.format
        -- `bufnr` and `filter` is handled by the LazyVim formatter,
        -- but can be also overridden when specified
        format = { formatting_options = nil, timeout_ms = nil },
        -- LSP Server Settings
        ---@type lspconfig.options
        servers = {
            tsserver = { mason = false },
            rust_analyzer = { mason = false },
            elixirls = { mason = false, cmd = { "language_server.sh" } },
            bashls = { mason = false },
            clangd = { mason = false },
            gopls = { mason = false },
            ruff_lsp = { mason = false },
            pyright = { mason = false },
            elmls = { mason = false },
            yamlls = { mason = false },
            -- => This package requires additional configuration for use in editors. Install package
            -- 'user-setup', or manually:

            -- * for Emacs, add these lines to ~/.emacs:
            --   (add-to-list 'load-path "/Users/NQA/.opam/default/share/emacs/site-lisp")
            --   (require 'ocp-indent)

            -- * for Vim, add this line to ~/.vimrc:
            --   set rtp^="/Users/NQA/.opam/default/share/ocp-indent/vim"
            -- ocamllsp = {
            --   mason = false
            -- },
            hls = {
                mason = false,
                cmd = { "haskell-language-server-wrapper", "--lsp" },
            },
            emmet_ls = {
                mason = false,
                filetypes = {
                    "css",
                    "eruby",
                    "html",
                    "javascript",
                    "javascriptreact",
                    "less",
                    "sass",
                    "scss",
                    "svelte",
                    "pug",
                    "typescriptreact",
                    "vue",
                    "eex",
                    "heex",
                },
            },
            html = { mason = false },
            tailwindcss = { mason = false, filetypes_exclude = { "markdown" } },
            julials = { mason = false },
            jsonls = { mason = false },
            lua_ls = {
                mason = false, -- set to false if you don't want this server to be installed with mason
                settings = {
                    Lua = {
                        workspace = { checkThirdParty = false },
                        completion = { callSnippet = "Replace" },
                    },
                },
            },
        },
        -- you can do any additional lsp server setup here
        -- return true if you don't want this server to be setup with lspconfig
        ---@type table<string, fun(server:string, opts:_.lspconfig.options):boolean?>
        setup = {
            -- example to setup with typescript.nvim
            -- tsserver = function(_, opts)
            --   require("typescript").setup({ server = opts })
            --   return true
            -- end,
            -- Specify * to use this function as a fallback for any server
            -- ["*"] = function(server, opts) end,
            tailwindcss = function(_, opts)
                local tw = require("lspconfig.server_configurations.tailwindcss")
                --- @param ft string
                opts.filetypes = vim.tbl_filter(function(ft)
                    return not vim.tbl_contains(opts.filetypes_exclude or {}, ft)
                end, tw.default_config.filetypes)
            end,
        },
    },
}
