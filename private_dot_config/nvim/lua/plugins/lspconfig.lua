return {
    {
        "neovim/nvim-lspconfig",

        -- ---------------------------------------------------------------------
        -- Use opts = function(_, opts) instead of opts = function()
        --
        -- The two-argument form receives the already-accumulated opts table
        -- from LazyVim core and all active extras (e.g. lang.python, lang.vue).
        -- We mutate and return it so our overrides WIN without discarding
        -- anything the extras contributed.
        --
        -- The no-argument form silently drops every extra's LSP contribution.
        -- ---------------------------------------------------------------------
        opts = function(_, opts)
            -- Apply global border style to the :LspInfo window
            require("lspconfig.ui.windows").default_options.border = BORDER_STYLE

            -- -----------------------------------------------------------------
            -- Diagnostics UI
            -- -----------------------------------------------------------------
            opts.diagnostics                                       = {
                underline = true,
                update_in_insert = false,
                float = { border = BORDER_STYLE },
                -- virtual_text disabled: we use the CursorHold float in autocmds.lua
                virtual_text = false,
                severity_sort = true,
                signs = {
                    text = {
                        [vim.diagnostic.severity.ERROR] = LazyVim.config.icons.diagnostics.Error,
                        [vim.diagnostic.severity.WARN]  = LazyVim.config.icons.diagnostics.Warn,
                        [vim.diagnostic.severity.HINT]  = LazyVim.config.icons.diagnostics.Hint,
                        [vim.diagnostic.severity.INFO]  = LazyVim.config.icons.diagnostics.Info,
                    },
                },
            }

            -- Inlay hints on by default; disabled for Vue (too noisy with template refs)
            opts.inlay_hints                                       = {
                enabled = true,
                exclude = { "vue" },
            }

            opts.codelens                                          = { enabled = false }
            opts.folds                                             = { enabled = true }

            opts.format                                            = {
                formatting_options = nil,
                timeout_ms = 3000,
            }

            -- -----------------------------------------------------------------
            -- LSP Servers
            --
            -- Deep-merge on top of whatever LazyVim extras have registered.
            -- "force" means our values win on conflict.
            -- -----------------------------------------------------------------
            ---@type table<string, lazyvim.lsp.Config|boolean>
            opts.servers                                           = vim.tbl_deep_extend("force", opts.servers or {}, {

                -- -------------------------------------------------------------
                -- Explicitly disable basedpyright
                --
                -- Setting to `false` guarantees it will NEVER start, even if
                -- it ends up back on PATH. We use Ruff + Ty instead.
                -- Any basedpyright-related code in autocmds.lua has been
                -- removed since this ensures the client never attaches.
                -- -------------------------------------------------------------
                basedpyright = false,

                -- -------------------------------------------------------------
                -- Global server defaults (keymaps etc.)
                --
                -- servers.*.keys uses opts_extend in LazyVim core so these
                -- MERGE with LazyVim's defaults — not replace them.
                --
                -- NOTE ON gd / gr / gI / K:
                -- These are set to vim.lsp.buf.* instead of LazyVim's default
                -- Snacks picker equivalents. This is intentional if you prefer
                -- native LSP navigation. If you want picker results (e.g. a
                -- list when there are multiple definitions), remove these entries
                -- and let LazyVim's defaults take over.
                --
                -- NOTE ON <C-k> INSERT MODE:
                -- The insert-mode <C-k> (signature help) is explicitly disabled
                -- here. Buffer-local LSP keymaps shadow global ones, so leaving
                -- it enabled would silently kill the mini.snippets <C-k>
                -- prev-placeholder binding in keymaps.lua.
                -- Signature help remains available via gK in normal mode.
                -- -------------------------------------------------------------
                ["*"]        = {
                    keys = {
                        { "<leader>cl", function() Snacks.picker.lsp_config() end,          desc = "Lsp Info" },

                        { "gd",         vim.lsp.buf.definition,                             desc = "Goto Definition",            has = "definition" },
                        { "gr",         vim.lsp.buf.references,                             desc = "References",                 nowait = true },
                        { "gI",         vim.lsp.buf.implementation,                         desc = "Goto Implementation" },
                        { "gy",         vim.lsp.buf.type_definition,                        desc = "Goto T[y]pe Definition" },
                        { "gD",         vim.lsp.buf.declaration,                            desc = "Goto Declaration" },

                        { "K",          function() return vim.lsp.buf.hover() end,          desc = "Hover" },
                        { "gK",         function() return vim.lsp.buf.signature_help() end, desc = "Signature Help",             has = "signatureHelp" },
                        -- <C-k> insert-mode intentionally DISABLED — see note above.
                        -- mini.snippets uses <C-k> for prev-placeholder (keymaps.lua).
                        { "<c-k>",      false,                                              mode = "i" },

                        { "<leader>ca", vim.lsp.buf.code_action,                            desc = "Code Action",                mode = { "n", "x" },  has = "codeAction" },
                        { "<leader>cc", vim.lsp.codelens.run,                               desc = "Run Codelens",               mode = { "n", "x" },  has = "codeLens" },
                        { "<leader>cC", vim.lsp.codelens.refresh,                           desc = "Refresh & Display Codelens", mode = { "n" },       has = "codeLens" },

                        { "<leader>cR", function() Snacks.rename.rename_file() end,         desc = "Rename File",                mode = { "n" },       has = { "workspace/didRenameFiles", "workspace/willRenameFiles" } },
                        { "<leader>cr", vim.lsp.buf.rename,                                 desc = "Rename",                     has = "rename" },
                        { "<leader>cA", LazyVim.lsp.action.source,                          desc = "Source Action",              has = "codeAction" },
                    },
                },

                -- -------------------------------------------------------------
                -- Lua LSP
                -- -------------------------------------------------------------
                lua_ls       = {
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

                -- -------------------------------------------------------------
                -- Python: Ruff + Ty (Astral toolchain)
                --
                -- Ruff: linting, import sorting, quick fixes
                -- Ty:   static type checking
                --
                -- Conform owns all formatting (see conform.lua). The LSP
                -- formatting capabilities for Ruff are disabled in autocmds.lua.
                --
                -- vim.g.lazyvim_python_lsp = "ty" in init.lua tells LazyVim's
                -- python extra which type-checker server to set up.
                -- -------------------------------------------------------------
                ruff         = {
                    mason = false,
                    filetypes = { "python" },
                    init_options = {
                        settings = {
                            configuration = vim.fn.expand("~/.config/ruff/ruff.toml"),
                            configurationPreference = "filesystemFirst",
                            showSyntaxErrors = true,
                            organizeImports = true,
                            logLevel = "info",
                        },
                    },
                },

                -- Ty is Astral's new type checker (alpha as of early 2025).
                -- Minimal config: just point LazyVim at it. Ty reads pyproject.toml.
                ty           = {
                    mason = false,
                    filetypes = { "python" },
                },

                -- -------------------------------------------------------------
                -- Other servers
                -- -------------------------------------------------------------
                vtsls        = { mason = false },

                elixirls     = {
                    mason = false,
                    cmd = { "language_server.sh" },
                    settings = {
                        elixirLS = {
                            dialyzerEnabled = false,
                            fetchDeps = false,
                        },
                    },
                },

                bashls       = { mason = false },
                clangd       = { mason = false },
                gopls        = { mason = false },
                biome        = {
                    mason = false, -- managed by mise, consistent with your setup
                },
                -- elmls        = { mason = false, cmd = { "elm-language-server" } },
                yamlls       = { mason = false },
                hls          = { mason = false },

                emmet_ls     = {
                    mason = false,
                    filetypes = {
                        "css", "eruby", "html",
                        "javascript", "javascriptreact",
                        "less", "sass", "scss",
                        "svelte", "pug",
                        "typescriptreact", "vue",
                        "eex", "heex",
                    },
                },

                html         = { mason = false },
                tailwindcss  = { mason = false, filetypes_exclude = { "markdown" } },
                julials      = { mason = false },
                jsonls       = { mason = false },
            })

            -- -----------------------------------------------------------------
            -- Setup hooks (run when each server is configured)
            -- -----------------------------------------------------------------
            opts.setup                                             = vim.tbl_deep_extend("force", opts.setup or {}, {

                -- Merge capabilities for every server at attach time
                ["*"] = function(_, server_opts)
                    server_opts.capabilities = vim.tbl_deep_extend("force", server_opts.capabilities or {}, {
                        general = { positionEncodings = { "utf-16" } },
                        workspace = { fileOperations = { didRename = true, willRename = true } },
                    })
                end,

                -- -------------------------------------------------------------
                -- Ruff setup hook
                --
                -- We do NOT register a BufWritePre code action here. Reason:
                -- vim.lsp.buf.code_action() is ASYNC — it resolves after the
                -- file has already been written, leaving the buffer "dirty"
                -- (modified-but-not-saved indicator reappears after every write).
                --
                -- Ruff fixes (import sorting, auto-fixable lint rules) are
                -- handled synchronously by Conform (see conform.lua):
                --   ruff check --fix --select=I  →  import sorting
                --   ruff_format                  →  code formatting
                --
                -- Interactive/on-demand fixes still work via:
                --   <leader>ca  →  Code Action  (shows all available fixes)
                -- -------------------------------------------------------------
                ruff = function(_, _)
                    -- Intentionally empty: Conform owns ruff on-save.
                    -- Return nothing (falsy) so LazyVim completes normal ruff setup.
                end,

                -- -------------------------------------------------------------
                -- vtsls: disable snippet completions
                --
                -- vtsls occasionally returns malformed LSP snippets (e.g. {}~)
                -- that crash Neovim's strict snippet parser. Disabling snippet
                -- support at the capability level is more reliable than the
                -- transform_items filter in blink.lua — use both as defence-in-depth.
                -- -------------------------------------------------------------
                vtsls = function(_, server_opts)
                    server_opts.capabilities = vim.tbl_deep_extend("force", server_opts.capabilities or {}, {
                        textDocument = {
                            completion = {
                                completionItem = {
                                    snippetSupport = false,
                                },
                            },
                        },
                    })
                end,

                -- Filter tailwindcss to only the filetypes we actually want
                tailwindcss = function(_, server_opts)
                    local tw = require("lspconfig.configs.tailwindcss")
                    server_opts.filetypes = vim.tbl_filter(function(ft)
                        return not vim.tbl_contains(server_opts.filetypes_exclude or {}, ft)
                    end, tw.default_config.filetypes)
                end,
            })

            return opts
        end,
    },
}
