return {
    "neovim/nvim-lspconfig",
    opts = function()
        return {
            -- options for vim.diagnostic.config()
            ---@type vim.diagnostic.Opts
            diagnostics = {
                underline = true,
                update_in_insert = false,
                float = {border = BORDER_STYLE},
                virtual_text = false,
                -- virtual_text = {
                --     spacing = 4,
                --     source = "if_many",
                --     prefix = "●"
                --     -- this will set set the prefix to a function that returns the diagnostics icon based on the severity
                --     -- this only works on a recent 0.10.0 build. Will be set to "●" when not supported
                --     -- prefix = "icons",
                -- },
                severity_sort = true,
                signs = {
                    text = {
                        [vim.diagnostic.severity.ERROR] = LazyVim.config.icons
                            .diagnostics.Error,
                        [vim.diagnostic.severity.WARN] = LazyVim.config.icons
                            .diagnostics.Warn,
                        [vim.diagnostic.severity.HINT] = LazyVim.config.icons
                            .diagnostics.Hint,
                        [vim.diagnostic.severity.INFO] = LazyVim.config.icons
                            .diagnostics.Info
                    }
                }
            },
            -- Enable this to enable the builtin LSP inlay hints on Neovim >= 0.10.0
            -- Be aware that you also will need to properly configure your LSP server to
            -- provide the inlay hints.
            inlay_hints = {
                enabled = true,
                exclude = {"vue"} -- filetypes for which you don't want to enable inlay hints
            },
            -- Enable this to enable the builtin LSP code lenses on Neovim >= 0.10.0
            -- Be aware that you also will need to properly configure your LSP server to
            -- provide the code lenses.
            codelens = {enabled = false},
            -- Enable lsp cursor word highlighting
            document_highlight = {enabled = true},
            -- add any global capabilities here
            capabilities = {
                workspace = {
                    fileOperations = {didRename = true, willRename = true}
                }
            },
            -- options for vim.lsp.buf.format
            -- `bufnr` and `filter` is handled by the LazyVim formatter,
            -- but can be also overridden when specified
            format = {formatting_options = nil, timeout_ms = nil},
            -- LSP Server Settings
            ---@type lspconfig.options
            servers = {
                lua_ls = {
                    mason = false, -- set to false if you don't want this server to be installed with mason
                    -- Use this to add any additional keymaps
                    -- for specific lsp servers
                    -- ---@type LazyKeysSpec[]
                    -- keys = {},
                    settings = {
                        Lua = {
                            workspace = {checkThirdParty = false},
                            codeLens = {enable = true},
                            completion = {callSnippet = "Replace"},
                            doc = {privateName = {"^_"}},
                            hint = {
                                enable = true,
                                setType = false,
                                paramType = true,
                                paramName = "Disable",
                                semicolon = "Disable",
                                arrayIndex = "Disable"
                            }
                        }
                    }
                },
                -- rust_analyzer = {mason = false},
                vtsls = {mason = false},
                elixirls = {
                    mason = false,
                    cmd = {"language_server.sh"},
                    settings = {
                        elixirLS = {
                            -- I choose to disable dialyzer for personal reasons, but
                            -- I would suggest you also disable it unless you are well
                            -- acquainted with dialzyer and know how to use it.
                            dialyzerEnabled = false,
                            -- I also choose to turn off the auto dep fetching feature.
                            -- It often get's into a weird state that requires deleting
                            -- the .elixir_ls directory and restarting your editor.
                            fetchDeps = false
                        }
                    }
                },
                bashls = {mason = false},
                clangd = {mason = false},
                gopls = {mason = false},
                ruff_lsp = {mason = false},
                pyright = {mason = false},
                elmls = {mason = false, cmd = {"elm-language-server"}},
                yamlls = {mason = false},
                hls = {mason = false},
                emmet_ls = {
                    mason = false,
                    filetypes = {
                        "css", "eruby", "html", "javascript", "javascriptreact",
                        "less", "sass", "scss", "svelte", "pug",
                        "typescriptreact", "vue", "eex", "heex"
                    }
                },
                html = {mason = false},
                tailwindcss = {mason = false, filetypes_exclude = {"markdown"}},
                julials = {mason = false},
                jsonls = {mason = false}
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
                    local tw = require("lspconfig.configs.tailwindcss")
                    --- @param ft string
                    opts.filetypes = vim.tbl_filter(function(ft)
                        return not vim.tbl_contains(
                                   opts.filetypes_exclude or {}, ft)
                    end, tw.default_config.filetypes)
                end
            }
        }
    end
}
