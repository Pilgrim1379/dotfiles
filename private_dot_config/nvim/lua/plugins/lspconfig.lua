return {
    {
        "neovim/nvim-lspconfig",
        opts = function()
            -- Ensure LSP UI windows (like :LspInfo) use your global border style
            require("lspconfig.ui.windows").default_options.border = BORDER_STYLE

            ---@type PluginLspOpts
            local opts = {
                -- ---------------------------------------------------------------------
                -- Diagnostics UI
                -- ---------------------------------------------------------------------
                diagnostics = {
                    underline = true,
                    update_in_insert = false,
                    float = { border = BORDER_STYLE },
                    virtual_text = false,
                    severity_sort = true,
                    signs = {
                        text = {
                            [vim.diagnostic.severity.ERROR] = LazyVim.config.icons.diagnostics.Error,
                            [vim.diagnostic.severity.WARN] = LazyVim.config.icons.diagnostics.Warn,
                            [vim.diagnostic.severity.HINT] = LazyVim.config.icons.diagnostics.Hint,
                            [vim.diagnostic.severity.INFO] = LazyVim.config.icons.diagnostics.Info,
                        },
                    },
                },

                -- Built-in inlay hints (server must support them)
                inlay_hints = {
                    enabled = true,
                    exclude = { "vue" },
                },

                codelens = { enabled = false },
                folds = { enabled = true },

                format = {
                    formatting_options = nil,
                    timeout_ms = nil,
                },

                -- ---------------------------------------------------------------------
                -- LSP Servers
                -- ---------------------------------------------------------------------
                ---@type table<string, lazyvim.lsp.Config|boolean>
                servers = {
                    -- Global server defaults (keymaps etc.)
                    ["*"] = {
                        keys = {
                            { "<leader>cl", function() Snacks.picker.lsp_config() end,          desc = "Lsp Info" },

                            { "gd",         vim.lsp.buf.definition,                             desc = "Goto Definition",            has = "definition" },
                            { "gr",         vim.lsp.buf.references,                             desc = "References",                 nowait = true },
                            { "gI",         vim.lsp.buf.implementation,                         desc = "Goto Implementation" },
                            { "gy",         vim.lsp.buf.type_definition,                        desc = "Goto T[y]pe Definition" },
                            { "gD",         vim.lsp.buf.declaration,                            desc = "Goto Declaration" },

                            { "K",          function() return vim.lsp.buf.hover() end,          desc = "Hover" },
                            { "gK",         function() return vim.lsp.buf.signature_help() end, desc = "Signature Help",             has = "signatureHelp" },
                            { "<c-k>",      function() return vim.lsp.buf.signature_help() end, mode = "i",                          desc = "Signature Help", has = "signatureHelp" },

                            { "<leader>ca", vim.lsp.buf.code_action,                            desc = "Code Action",                mode = { "n", "x" },     has = "codeAction" },
                            { "<leader>cc", vim.lsp.codelens.run,                               desc = "Run Codelens",               mode = { "n", "x" },     has = "codeLens" },
                            { "<leader>cC", vim.lsp.codelens.refresh,                           desc = "Refresh & Display Codelens", mode = { "n" },          has = "codeLens" },

                            { "<leader>cR", function() Snacks.rename.rename_file() end,         desc = "Rename File",                mode = { "n" },          has = { "workspace/didRenameFiles", "workspace/willRenameFiles" } },
                            { "<leader>cr", vim.lsp.buf.rename,                                 desc = "Rename",                     has = "rename" },
                            { "<leader>cA", LazyVim.lsp.action.source,                          desc = "Source Action",              has = "codeAction" },
                        },
                    },

                    -- You had this disabled
                    stylua = { enabled = false },

                    -- Lua LSP (keep your settings)
                    lua_ls = {
                        mason = false,
                        settings = {
                            Lua = {
                                workspace = { checkThirdParty = false },
                                codeLens = { enable = true },
                                completion = { callSnippet = "Replace" },
                                doc = { privateName = { "^_" } },
                                hint = {
                                    enable = true,
                                    setType = false,
                                    paramType = true,
                                    paramName = "Disable",
                                    semicolon = "Disable",
                                    arrayIndex = "Disable",
                                },
                            },
                        },
                    },

                    -- -------------------------------------------------------------------
                    -- Python: Ruff + basedpyright
                    -- -------------------------------------------------------------------
                    ruff = {
                        mason = false,
                        init_options = {
                            settings = {
                                configuration = vim.fn.expand("~/.config/ruff/ruff.toml"),
                                configurationPreference = "filesystemFirst",
                                showSyntaxErrors = true,
                                logLevel = "info",
                            },
                        },
                    },

                    basedpyright = {
                        mason = false,
                        cmd = { "basedpyright-langserver", "--stdio" },
                        single_file_support = true,
                        settings = {
                            python = {
                                analysis = {
                                    typeCheckingMode = "standard",
                                    diagnosticMode = "openFilesOnly",
                                    useLibraryCodeForTypes = true,
                                    autoSearchPaths = true,
                                    inlayHints = {
                                        variableTypes = true,
                                        functionReturnTypes = true,
                                        callArgumentNames = true,
                                        genericTypes = true,
                                    },
                                },
                            },
                        },
                    },

                    -- -------------------------------------------------------------------
                    -- Other servers you had
                    -- -------------------------------------------------------------------
                    vtsls = { mason = false },
                    elixirls = {
                        mason = false,
                        cmd = { "language_server.sh" },
                        settings = {
                            elixirLS = {
                                dialyzerEnabled = false,
                                fetchDeps = false,
                            },
                        },
                    },
                    bashls = { mason = false },
                    clangd = { mason = false },
                    gopls = { mason = false },

                    elmls = { mason = false, cmd = { "elm-language-server" } },
                    yamlls = { mason = false },
                    hls = { mason = false },

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
                },

                -- ---------------------------------------------------------------------
                -- Setup hooks (capabilities merge, tailwind filtering)
                -- ---------------------------------------------------------------------
                setup = {
                    -- Merge capabilities (extend, don't replace)
                    ["*"] = function(_, server_opts)
                        server_opts.capabilities = vim.tbl_deep_extend("force", server_opts.capabilities or {}, {
                            general = { positionEncodings = { "utf-16" } },
                            workspace = { fileOperations = { didRename = true, willRename = true } },
                        })
                    end,

                    -- Tailwind filetype filtering helper
                    tailwindcss = function(_, server_opts)
                        local tw = require("lspconfig.configs.tailwindcss")
                        server_opts.filetypes = vim.tbl_filter(function(ft)
                            return not vim.tbl_contains(server_opts.filetypes_exclude or {}, ft)
                        end, tw.default_config.filetypes)
                    end,
                },
            }

            return opts
        end,
    },
}
