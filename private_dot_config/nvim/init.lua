--[[

=====================================================================
==================== READ THIS BEFORE CONTINUING ====================
=====================================================================
========                                    .-----.          ========
========         .----------------------.   | === |          ========
========         |.-""""""""""""""""""-.|   |-----|          ========
========         ||                    ||   | === |          ========
========         ||   KICKSTART.NVIM   ||   |-----|          ========
========         ||                    ||   | === |          ========
========         ||                    ||   |-----|          ========
========         ||:Tutor              ||   |:::::|          ========
========         |'-..................-'|   |____o|          ========
========         `"")----------------(""`   ___________      ========
========        /::::::::::|  |::::::::::\  \ no mouse \     ========
========       /:::========|  |==hjkl==:::\  \ required \    ========
========      '""""""""""""'  '""""""""""""'  '""""""""""'   ========
========                                                     ========
=====================================================================
=====================================================================

What is Kickstart?

  Kickstart.nvim is *not* a distribution.

  Kickstart.nvim is a starting point for your own configuration.
    The goal is that you can read every line of code, top-to-bottom, understand
    what your configuration is doing, and modify it to suit your needs.

    Once you've done that, you can start exploring, configuring and tinkering to
    make Neovim your own! That might mean leaving Kickstart just the way it is for a while
    or immediately breaking it into modular pieces. It's up to you!

    If you don't know anything about Lua, I recommend taking some time to read through
    a guide. One possible example which will only take 10-15 minutes:
      - https://learnxinyminutes.com/docs/lua/

    After understanding a bit more about Lua, you can use `:help lua-guide` as a
    reference for how Neovim integrates Lua.
    - :help lua-guide
    - (or HTML version): https://neovim.io/doc/user/lua-guide.html

Kickstart Guide:

  TODO: The very first thing you should do is to run the command `:Tutor` in Neovim.

    If you don't know what this means, type the following:
      - <escape key>
      - :
      - Tutor
      - <enter key>

    (If you already know the Neovim basics, you can skip this step.)

  Once you've completed that, you can continue working through **AND READING** the rest
  of the kickstart init.lua.

  Next, run AND READ `:help`.
    This will open up a help window with some basic information
    about reading, navigating and searching the builtin help documentation.

    This should be the first place you go to look when you're stuck or confused
    with something. It's one of my favorite Neovim features.

    MOST IMPORTANTLY, we provide a keymap "<space>sh" to [s]earch the [h]elp documentation,
    which is very useful when you're not exactly sure of what you're looking for.

  I have left several `:help X` comments throughout the init.lua
    These are hints about where to find more information about the relevant settings,
    plugins or Neovim features used in Kickstart.

   NOTE: Look for lines like this

    Throughout the file. These are for you, the reader, to help you understand what is happening.
    Feel free to delete them once you know what you're doing, but they should serve as a guide
    for when you are first encountering a few different constructs in your Neovim config.

If you experience any errors while trying to install kickstart, run `:checkhealth` for more info.

I hope you enjoy your Neovim journey,
- TJ

P.S. You can delete this when you're done too. It's your config now! :)
--]] BORDER_STYLE = "single"

-- Set <space> as the leader key
-- See `:help mapleader`
--  NOTE: Must happen before plugins are loaded (otherwise wrong leader will be used)
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- Set to true if you have a Nerd Font installed and selected in the terminal
vim.g.have_nerd_font = true

-- [[ Setting options ]]
-- See `:help vim.opt`
-- NOTE: You can change these options as you wish!
--  For more options, you can see `:help option-list`

-- Make line numbers default
vim.opt.number = true
-- You can also add relative line numbers, to help with jumping.
--  Experiment for yourself to see if you like it!
vim.opt.relativenumber = true

-- Enable mouse mode, can be useful for resizing splits for example!
vim.opt.mouse = 'a'

-- Don't show the mode, since it's already in the status line
vim.opt.showmode = false

-- Sync clipboard between OS and Neovim.
--  Remove this option if you want your OS clipboard to remain independent.
--  See `:help 'clipboard'`
vim.opt.clipboard = 'unnamedplus'

-- Enable break indent
vim.opt.breakindent = true

-- Save undo history
vim.opt.undofile = true

-- Case-insensitive searching UNLESS \C or one or more capital letters in the search term
vim.opt.ignorecase = true
vim.opt.smartcase = true

-- Keep signcolumn on by default
vim.opt.signcolumn = 'yes'

-- Decrease update time
vim.opt.updatetime = 250

-- Decrease mapped sequence wait time
-- Displays which-key popup sooner
vim.opt.timeoutlen = 300

-- Configure how new splits should be opened
vim.opt.splitright = true
vim.opt.splitbelow = true

-- Sets how neovim will display certain whitespace characters in the editor.
--  See `:help 'list'`
--  and `:help 'listchars'`
vim.opt.list = true
vim.opt.listchars = {tab = '» ', trail = '·', nbsp = '␣'}

-- Preview substitutions live, as you type!
vim.opt.inccommand = 'split'

-- Show which line your cursor is on
vim.opt.cursorline = true

-- Minimal number of screen lines to keep above and below the cursor.
vim.opt.scrolloff = 10

-- [[ Basic Keymaps ]]
--  See `:help vim.keymap.set()`

-- Set highlight on search, but clear on pressing <Esc> in normal mode
vim.opt.hlsearch = true
vim.keymap.set('n', '<Esc>', '<cmd>nohlsearch<CR>')

-- Diagnostic keymaps
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist,
               {desc = 'Open diagnostic [Q]uickfix list'})

-- Exit terminal mode in the builtin terminal with a shortcut that is a bit easier
-- for people to discover. Otherwise, you normally need to press <C-\><C-n>, which
-- is not what someone will guess without a bit more experience.
--
-- NOTE: This won't work in all terminal emulators/tmux/etc. Try your own mapping
-- or just use <C-\><C-n> to exit terminal mode
vim.keymap.set('t', '<Esc><Esc>', '<C-\\><C-n>', {desc = 'Exit terminal mode'})

-- TIP: Disable arrow keys in normal mode
-- vim.keymap.set('n', '<left>', '<cmd>echo "Use h to move!!"<CR>')
-- vim.keymap.set('n', '<right>', '<cmd>echo "Use l to move!!"<CR>')
-- vim.keymap.set('n', '<up>', '<cmd>echo "Use k to move!!"<CR>')
-- vim.keymap.set('n', '<down>', '<cmd>echo "Use j to move!!"<CR>')

-- Keybinds to make split navigation easier.
--  Use CTRL+<hjkl> to switch between windows
--
--  See `:help wincmd` for a list of all window commands
vim.keymap.set('n', '<C-h>', '<C-w><C-h>',
               {desc = 'Move focus to the left window'})
vim.keymap.set('n', '<C-l>', '<C-w><C-l>',
               {desc = 'Move focus to the right window'})
vim.keymap.set('n', '<C-j>', '<C-w><C-j>',
               {desc = 'Move focus to the lower window'})
vim.keymap.set('n', '<C-k>', '<C-w><C-k>',
               {desc = 'Move focus to the upper window'})

-- [[ Basic Autocommands ]]
--  See `:help lua-guide-autocommands`

-- Highlight when yanking (copying) text
--  Try it with `yap` in normal mode
--  See `:help vim.highlight.on_yank()`
vim.api.nvim_create_autocmd('TextYankPost', {
    desc = 'Highlight when yanking (copying) text',
    group = vim.api.nvim_create_augroup('kickstart-highlight-yank',
                                        {clear = true}),
    callback = function() vim.highlight.on_yank() end
})

-- Diagnostic windows behaviour
vim.api.nvim_create_autocmd({"CursorHold", "CursorHoldI"}, {
    callback = function() vim.diagnostic.open_float(nil, {focus = false}) end
})

-- revert terminal cursor to vertical bar upon exit or suspend
vim.api.nvim_create_autocmd({"VimLeave", "VimSuspend"},
                            {command = "set guicursor=a:ver1"})
-- vim.cmd([[
--     augroup RestoreCursorShapeOnExit
--         autocmd!
--         autocmd VimLeave,VimSuspend * set guicursor=a:ver1
--     augroup END
-- ]])

vim.api.nvim_create_autocmd('TermOpen', {
    group = vim.api.nvim_create_augroup('custom-term-open', {clear = true}),
    callback = function()
        vim.opt.number = false
        vim.opt.relativenumber = false
    end
})

vim.keymap.set('n', "<space>st", function()
    vim.cmd.vnew()
    vim.cmd.term()
    vim.cmd.wincmd("J")
    vim.api.nvim_win_set_height(0, 15)
end, {desc = 'Open Terminal'})

-- [[ Basic Commands ]]
vim.cmd("silent! Copilot disable") -- Disable Copilot on startup

-- Configure diagnostics
vim.diagnostic.config({virtual_text = false, float = {border = BORDER_STYLE}})

-- To appropriately highlight codefences returned from denols
vim.g.markdown_fenced_languages = {"ts=typescript"}

-- [[ Install `lazy.nvim` plugin manager ]]
--    See `:help lazy.nvim.txt` or https://github.com/folke/lazy.nvim for more info
local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not (vim.uv or vim.loop).fs_stat(lazypath) then
    local lazyrepo = 'https://github.com/folke/lazy.nvim.git'
    local out = vim.fn.system {
        'git', 'clone', '--filter=blob:none', '--branch=stable', lazyrepo,
        lazypath
    }
    if vim.v.shell_error ~= 0 then error('Error cloning lazy.nvim:\n' .. out) end
end ---@diagnostic disable-next-line: undefined-field
vim.opt.rtp:prepend(lazypath)

-- [[ Configure and install plugins ]]
--
--  To check the current status of your plugins, run
--    :Lazy
--
--  You can press `?` in this menu for help. Use `:q` to close the window
--
--  To update plugins you can run
--    :Lazy update
--
-- NOTE: Here is where you install your plugins.
require('lazy').setup({
    -- NOTE: Plugins can be added with a link (or for a github repo: 'owner/repo' link).
    'tpope/vim-sleuth', -- Detect tabstop and shiftwidth automatically
    -- NOTE: Plugins can also be added by using a table,
    -- with the first argument being the link and the following
    -- keys can be used to configure plugin behavior/loading/etc.
    --
    -- Use `opts = {}` to force a plugin to be loaded.
    --
    --  This is equivalent to:
    --    require('Comment').setup({})
    -- "gc" to comment visual regions/lines
    {'numToStr/Comment.nvim', opts = {}},

    -- Here is a more advanced example where we pass configuration
    -- options to `gitsigns.nvim`. This is equivalent to the following Lua:
    --    require('gitsigns').setup({ ... })
    --
    -- See `:help gitsigns` to understand what the configuration keys do
    { -- Adds git related signs to the gutter, as well as utilities for managing changes
        'lewis6991/gitsigns.nvim',
        opts = {
            signs = {
                add = {text = '+'},
                change = {text = '~'},
                delete = {text = '_'},
                topdelete = {text = '‾'},
                changedelete = {text = '~'}
            }
        }
    },

    -- NOTE: Plugins can also be configured to run Lua code when they are loaded.
    --
    -- This is often very useful to both group configuration, as well as handle
    -- lazy loading plugins that don't need to be loaded immediately at startup.
    --
    -- For example, in the following configuration, we use:
    --  event = 'VimEnter'
    --
    -- which loads which-key before all the UI elements are loaded. Events can be
    -- normal autocommands events (`:help autocmd-events`).
    --
    -- Then, because we use the `config` key, the configuration only runs
    -- after the plugin has been loaded:
    --  config = function() ... end

    { -- Useful plugin to show you pending keybinds.
        'folke/which-key.nvim',
        event = 'VimEnter', -- Sets the loading event to 'VimEnter'
        opts = {
            win = {
                -- don't allow the popup to overlap with the cursor
                no_overlap = true,
                -- width = 1,
                -- height = { min = 4, max = 25 },
                -- col = 0,
                -- row = math.huge,
                border = BORDER_STYLE,
                padding = {1, 2}, -- extra window padding [top/bottom, right/left]
                title = true,
                title_pos = "center",
                zindex = 1000,
                -- Additional vim.wo and vim.bo options
                bo = {},
                wo = {
                    -- winblend = 10, -- value between 0-100 0 for fully opaque and 100 for fully transparent
                }
            },
            icons = {
                -- set icon mappings to true if you have a Nerd Font
                mappings = vim.g.have_nerd_font,
                -- If you are using a Nerd Font: set icons.keys to an empty table which will use the
                -- default whick-key.nvim defined Nerd Font icons, otherwise define a string table
                keys = vim.g.have_nerd_font and {} or {
                    Up = '<Up> ',
                    Down = '<Down> ',
                    Left = '<Left> ',
                    Right = '<Right> ',
                    C = '<C-…> ',
                    M = '<M-…> ',
                    D = '<D-…> ',
                    S = '<S-…> ',
                    CR = '<CR> ',
                    Esc = '<Esc> ',
                    ScrollWheelDown = '<ScrollWheelDown> ',
                    ScrollWheelUp = '<ScrollWheelUp> ',
                    NL = '<NL> ',
                    BS = '<BS> ',
                    Space = '<Space> ',
                    Tab = '<Tab> ',
                    F1 = '<F1>',
                    F2 = '<F2>',
                    F3 = '<F3>',
                    F4 = '<F4>',
                    F5 = '<F5>',
                    F6 = '<F6>',
                    F7 = '<F7>',
                    F8 = '<F8>',
                    F9 = '<F9>',
                    F10 = '<F10>',
                    F11 = '<F11>',
                    F12 = '<F12>'
                }
            },

            -- Document existing key chains
            spec = {
                {'<leader>c', group = '[C]ode', mode = {'n', 'x'}},
                {'<leader>d', group = '[D]ocument'},
                {'<leader>r', group = '[R]ename'},
                {'<leader>s', group = '[S]earch'},
                {'<leader>w', group = '[W]orkspace'},
                {'<leader>t', group = '[T]oggle'},
                {'<leader>h', group = 'Git [H]unk', mode = {'n', 'v'}}
            }
        }
    }, -- NOTE: Plugins can specify dependencies.
    --
    -- The dependencies are proper plugin specifications as well - anything
    -- you do for a plugin at the top level, you can do for a dependency.
    --
    -- Use the `dependencies` key to specify the dependencies of a particular plugin
    { -- Fuzzy Finder (files, lsp, etc)
        'nvim-telescope/telescope.nvim',
        event = 'VimEnter',
        branch = '0.1.x',
        dependencies = {
            'nvim-lua/plenary.nvim',
            { -- If encountering errors, see telescope-fzf-native README for installation instructions
                'nvim-telescope/telescope-fzf-native.nvim',

                -- `build` is used to run some command when the plugin is installed/updated.
                -- This is only run then, not every time Neovim starts up.
                build = 'make',

                -- `cond` is a condition used to determine whether this plugin should be
                -- installed and loaded.
                cond = function()
                    return vim.fn.executable 'make' == 1
                end
            }, {'nvim-telescope/telescope-ui-select.nvim'},

            -- Useful for getting pretty icons, but requires a Nerd Font.
            {'nvim-tree/nvim-web-devicons', enabled = vim.g.have_nerd_font}
        },
        config = function()
            -- Telescope is a fuzzy finder that comes with a lot of different things that
            -- it can fuzzy find! It's more than just a "file finder", it can search
            -- many different aspects of Neovim, your workspace, LSP, and more!
            --
            -- The easiest way to use Telescope, is to start by doing something like:
            --  :Telescope help_tags
            --
            -- After running this command, a window will open up and you're able to
            -- type in the prompt window. You'll see a list of `help_tags` options and
            -- a corresponding preview of the help.
            --
            -- Two important keymaps to use while in Telescope are:
            --  - Insert mode: <c-/>
            --  - Normal mode: ?
            --
            -- This opens a window that shows you all of the keymaps for the current
            -- Telescope picker. This is really useful to discover what Telescope can
            -- do as well as how to actually do it!

            -- [[ Configure Telescope ]]
            -- See `:help telescope` and `:help telescope.setup()`
            require('telescope').setup {
                -- You can put your default mappings / updates / etc. in here
                --  All the info you're looking for is in `:help telescope.setup()`
                --
                -- defaults = {
                --   mappings = {
                --     i = { ['<c-enter>'] = 'to_fuzzy_refine' },
                --   },
                -- },
                -- pickers = {}
                extensions = {
                    ['ui-select'] = {require('telescope.themes').get_dropdown()}
                }
            }

            -- Enable Telescope extensions if they are installed
            pcall(require('telescope').load_extension, 'fzf')
            pcall(require('telescope').load_extension, 'ui-select')

            -- See `:help telescope.builtin`
            local builtin = require 'telescope.builtin'
            vim.keymap.set('n', '<leader>sh', builtin.help_tags,
                           {desc = '[S]earch [H]elp'})
            vim.keymap.set('n', '<leader>sk', builtin.keymaps,
                           {desc = '[S]earch [K]eymaps'})
            vim.keymap.set('n', '<leader>sf', builtin.find_files,
                           {desc = '[S]earch [F]iles'})
            vim.keymap.set('n', '<leader>ss', builtin.builtin,
                           {desc = '[S]earch [S]elect Telescope'})
            vim.keymap.set('n', '<leader>sw', builtin.grep_string,
                           {desc = '[S]earch current [W]ord'})
            vim.keymap.set('n', '<leader>sg', builtin.live_grep,
                           {desc = '[S]earch by [G]rep'})
            vim.keymap.set('n', '<leader>sd', builtin.diagnostics,
                           {desc = '[S]earch [D]iagnostics'})
            vim.keymap.set('n', '<leader>sr', builtin.resume,
                           {desc = '[S]earch [R]esume'})
            vim.keymap.set('n', '<leader>s.', builtin.oldfiles,
                           {desc = '[S]earch Recent Files ("." for repeat)'})
            vim.keymap.set('n', '<leader><leader>', builtin.buffers,
                           {desc = '[ ] Find existing buffers'})

            -- Slightly advanced example of overriding default behavior and theme
            vim.keymap.set('n', '<leader>/', function()
                -- You can pass additional configuration to Telescope to change the theme, layout, etc.
                builtin.current_buffer_fuzzy_find(
                    require('telescope.themes').get_dropdown {
                        winblend = 10,
                        previewer = false
                    })
            end, {desc = '[/] Fuzzily search in current buffer'})

            -- It's also possible to pass additional configuration options.
            --  See `:help telescope.builtin.live_grep()` for information about particular keys
            vim.keymap.set('n', '<leader>s/', function()
                builtin.live_grep {
                    grep_open_files = true,
                    prompt_title = 'Live Grep in Open Files'
                }
            end, {desc = '[S]earch [/] in Open Files'})

            -- Shortcut for searching your Neovim configuration files
            vim.keymap.set('n', '<leader>sn', function()
                builtin.find_files {cwd = vim.fn.stdpath 'config'}
            end, {desc = '[S]earch [N]eovim files'})
        end
    }, -- LSP Plugins
    {
        -- `lazydev` configures Lua LSP for your Neovim config, runtime and plugins
        -- used for completion, annotations and signatures of Neovim apis
        'folke/lazydev.nvim',
        ft = 'lua',
        opts = {
            library = {
                -- Load luvit types when the `vim.uv` word is found
                {path = 'luvit-meta/library', words = {'vim%.uv'}}
            }
        }
    }, {'Bilal2453/luvit-meta', lazy = true}, { -- Main LSP Configuration
        'neovim/nvim-lspconfig',
        dependencies = {
            -- Automatically install LSPs and related tools to stdpath for Neovim
            {'williamboman/mason.nvim', config = true}, -- NOTE: Must be loaded before dependants
            'williamboman/mason-lspconfig.nvim',
            'WhoIsSethDaniel/mason-tool-installer.nvim',

            -- Useful status updates for LSP.
            -- NOTE: `opts = {}` is the same as calling `require('fidget').setup({})`
            {'j-hui/fidget.nvim', opts = {}},

            -- Allows extra capabilities provided by nvim-cmp
            -- 'hrsh7th/cmp-nvim-lsp'
            'saghen/blink.cmp' -- replacement for nvim-cmp
        },
        config = function()
            -- Brief aside: **What is LSP?**
            --
            -- LSP is an initialism you've probably heard, but might not understand what it is.
            --
            -- LSP stands for Language Server Protocol. It's a protocol that helps editors
            -- and language tooling communicate in a standardized fashion.
            --
            -- In general, you have a "server" which is some tool built to understand a particular
            -- language (such as `gopls`, `lua_ls`, `rust_analyzer`, etc.). These Language Servers
            -- (sometimes called LSP servers, but that's kind of like ATM Machine) are standalone
            -- processes that communicate with some "client" - in this case, Neovim!
            --
            -- LSP provides Neovim with features like:
            --  - Go to definition
            --  - Find references
            --  - Autocompletion
            --  - Symbol Search
            --  - and more!
            --
            -- Thus, Language Servers are external tools that must be installed separately from
            -- Neovim. This is where `mason` and related plugins come into play.
            --
            -- If you're wondering about lsp vs treesitter, you can check out the wonderfully
            -- and elegantly composed help section, `:help lsp-vs-treesitter`

            -- Configure lsp servers without Mason
            local lspconfig = require("lspconfig")

            require("lspconfig.ui.windows").default_options.border =
                BORDER_STYLE

            --  This function gets run when an LSP attaches to a particular buffer.
            --    That is to say, every time a new file is opened that is associated with
            --    an lsp (for example, opening `main.rs` is associated with `rust_analyzer`) this
            --    function will be executed to configure the current buffer
            vim.api.nvim_create_autocmd('LspAttach', {
                group = vim.api.nvim_create_augroup('kickstart-lsp-attach',
                                                    {clear = true}),
                callback = function(event)
                    -- NOTE: Remember that Lua is a real programming language, and as such it is possible
                    -- to define small helper and utility functions so you don't have to repeat yourself.
                    --
                    -- In this case, we create a function that lets us more easily define mappings specific
                    -- for LSP related items. It sets the mode, buffer and description for us each time.
                    local map = function(keys, func, desc, mode)
                        mode = mode or 'n'
                        vim.keymap.set(mode, keys, func, {
                            buffer = event.buf,
                            desc = 'LSP: ' .. desc
                        })
                    end

                    -- Jump to the definition of the word under your cursor.
                    --  This is where a variable was first declared, or where a function is defined, etc.
                    --  To jump back, press <C-t>.
                    map('gd', require('telescope.builtin').lsp_definitions,
                        '[G]oto [D]efinition')

                    -- Find references for the word under your cursor.
                    map('gr', require('telescope.builtin').lsp_references,
                        '[G]oto [R]eferences')

                    -- Jump to the implementation of the word under your cursor.
                    --  Useful when your language has ways of declaring types without an actual implementation.
                    map('gI', require('telescope.builtin').lsp_implementations,
                        '[G]oto [I]mplementation')

                    -- Jump to the type of the word under your cursor.
                    --  Useful when you're not sure what type a variable is and you want to see
                    --  the definition of its *type*, not where it was *defined*.
                    map('<leader>D',
                        require('telescope.builtin').lsp_type_definitions,
                        'Type [D]efinition')

                    -- Fuzzy find all the symbols in your current document.
                    --  Symbols are things like variables, functions, types, etc.
                    map('<leader>ds',
                        require('telescope.builtin').lsp_document_symbols,
                        '[D]ocument [S]ymbols')

                    -- Fuzzy find all the symbols in your current workspace.
                    --  Similar to document symbols, except searches over your entire project.
                    map('<leader>ws',
                        require('telescope.builtin').lsp_dynamic_workspace_symbols,
                        '[W]orkspace [S]ymbols')

                    -- Rename the variable under your cursor.
                    --  Most Language Servers support renaming across files, etc.
                    map('<leader>rn', vim.lsp.buf.rename, '[R]e[n]ame')

                    -- Execute a code action, usually your cursor needs to be on top of an error
                    -- or a suggestion from your LSP for this to activate.
                    map('<leader>ca', vim.lsp.buf.code_action,
                        '[C]ode [A]ction', {'n', 'x'})

                    -- WARN: This is not Goto Definition, this is Goto Declaration.
                    --  For example, in C this would take you to the header.
                    map('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')

                    -- The following two autocommands are used to highlight references of the
                    -- word under your cursor when your cursor rests there for a little while.
                    --    See `:help CursorHold` for information about when this is executed
                    --
                    -- When you move your cursor, the highlights will be cleared (the second autocommand).
                    local client = vim.lsp
                                       .get_client_by_id(event.data.client_id)
                    if client and
                        client.server_capabilities.documentHighlightProvider then
                        local highlight_augroup =
                            vim.api.nvim_create_augroup(
                                'kickstart-lsp-highlight', {clear = false})
                        vim.api.nvim_create_autocmd({
                            'CursorHold', 'CursorHoldI'
                        }, {
                            buffer = event.buf,
                            group = highlight_augroup,
                            callback = vim.lsp.buf.document_highlight
                        })

                        vim.api.nvim_create_autocmd({
                            'CursorMoved', 'CursorMovedI'
                        }, {
                            buffer = event.buf,
                            group = highlight_augroup,
                            callback = vim.lsp.buf.clear_references
                        })

                        vim.api.nvim_create_autocmd('LspDetach', {
                            group = vim.api.nvim_create_augroup(
                                'kickstart-lsp-detach', {clear = true}),
                            callback = function(event2)
                                vim.lsp.buf.clear_references()
                                vim.api.nvim_clear_autocmds {
                                    group = 'kickstart-lsp-highlight',
                                    buffer = event2.buf
                                }
                            end
                        })
                    end

                    -- The following code creates a keymap to toggle inlay hints in your
                    -- code, if the language server you are using supports them
                    --
                    -- This may be unwanted, since they displace some of your code
                    if client and
                        client.supports_method(
                            vim.lsp.protocol.Methods.textDocument_inlayHint) then
                        map('<leader>th', function()
                            vim.lsp.inlay_hint.enable(
                                not vim.lsp.inlay_hint.is_enabled {
                                    bufnr = event.buf
                                })
                        end, '[T]oggle Inlay [H]ints')
                    end
                end
            })

            -- LSP servers and clients are able to communicate to each other what features they support.
            --  By default, Neovim doesn't support everything that is in the LSP specification.
            --  When you add nvim-cmp, luasnip, etc. Neovim now has *more* capabilities.
            --  So, we create new capabilities with nvim cmp, and then broadcast that to the servers.
            local capabilities = vim.lsp.protocol.make_client_capabilities()

            -- capabilities = vim.tbl_deep_extend('force', capabilities, require(
            --                                        'cmp_nvim_lsp').default_capabilities())

            capabilities = vim.tbl_deep_extend('force', capabilities, require(
                                                   'blink.cmp').get_lsp_capabilities())

            -- Enable the following language servers
            --  Feel free to add/remove any LSPs that you want here. They will automatically be installed.
            --
            --  Add any additional override configuration in the following tables. Available keys are:
            --  - cmd (table): Override the default command used to start the server
            --  - filetypes (table): Override the default list of associated filetypes for the server
            --  - capabilities (table): Override fields in capabilities. Can be used to disable certain LSP features.
            --  - settings (table): Override the default settings passed when initializing the server.
            --        For example, to see the options for `lua_ls`, you could go to: https://luals.github.io/wiki/settings/
            local servers = {
                clangd = {},
                gopls = {},
                pyright = {
                    -- If you'd like to use Ruff exclusively for linting, formatting, and organizing imports,
                    -- you can disable those capabilities for Pyright:
                    settings = {
                        pyright = {
                            -- Using Ruff's import organizer
                            disableOrganizeImports = true
                        },
                        python = {
                            analysis = {
                                -- Ignore all files for analysis to exclusively use Ruff for linting
                                ignore = {'*'}
                            }
                        }
                    }
                },
                ruff = {init_options = {settings = {logLevel = 'info'}}},
                rust_analyzer = {},
                elmls = {},
                nim_langserver = {},
                bashls = {},
                -- ... etc. See `:help lspconfig-all` for a list of all the pre-configured LSPs
                --
                -- Some languages (like typescript) have entire language plugins that can be useful:
                --    https://github.com/pmizio/typescript-tools.nvim
                --
                -- But for many setups, the LSP (``) will work just fine
                ts_ls = {},
                denols = {},
                -- lexical = {
                --     filetypes = {"elixir", "eelixir", "heex"},
                --     cmd = {
                --         vim.fn.expand(
                --             "~/github/language-servers/lexical/bin/start_lexical.sh")
                --     },
                --     settings = {},
                --     root_dir = function(fname)
                --         return lspconfig.util.root_pattern("mix.exs", ".git")(
                --                    fname) or vim.loop.os_homedir()
                --     end
                -- },
                nextls = {
                    filetypes = {"elixir", "eelixir", "heex"},
                    cmd = {
                        vim.fn
                            .expand("~/github/language-servers/next-ls/nextls"),
                        "--stdio"
                    },
                    init_options = {
                        extensions = {credo = {enable = true}},
                        experimental = {completions = {enable = true}}
                    }
                },
                -- elixirls = {
                --     filetypes = {"elixir", "eelixir", "heex"},
                --     cmd = {
                --         vim.fn.expand(
                --             "~/github/language-servers/elixir-ls/language_server.sh")
                --     },
                --     settings = {
                --         elixirLS = {
                --             -- I choose to disable dialyzer for personal reasons, but
                --             -- I would suggest you also disable it unless you are well
                --             -- acquainted with dialzyer and know how to use it.
                --             dialyzerEnabled = false,
                --             -- I also choose to turn off the auto dep fetching feature.
                --             -- It often get's into a weird state that requires deleting
                --             -- the .elixir_ls directory and restarting your editor.
                --             fetchDeps = false
                --             -- enableTestLenses = false,
                --             -- suggestSpecs = false,
                --         }
                --     },
                --     root_dir = function(fname)
                --         return lspconfig.util.root_pattern("mix.exs", ".git")(
                --                    fname) or vim.loop.os_homedir()
                --     end
                -- },
                lua_ls = {
                    -- cmd = {...},
                    -- filetypes = { ...},
                    -- capabilities = {},
                    settings = {
                        Lua = {
                            completion = {callSnippet = 'Replace'}
                            -- You can toggle below to ignore Lua_LS's noisy `missing-fields` warnings
                            -- diagnostics = { disable = { 'missing-fields' } },
                        }
                    }
                }
            }

            -- Ensure the servers and tools above are installed
            --  To check the current status of installed tools and/or manually install
            --  other tools, you can run
            --    :Mason
            --
            --  You can press `g?` for help in this menu.
            require('mason').setup({opts = {ui = {border = BORDER_STYLE}}})

            -- You can add other tools here that you want Mason to install
            -- for you, so that they are available from within Neovim.
            -- local ensure_installed = vim.tbl_keys(servers or {})
            -- vim.list_extend(ensure_installed, {
            --     'stylua', -- Used to format Lua code
            -- })
            require('mason-tool-installer').setup {}

            -- require('mason-lspconfig').setup {
            --     handlers = {
            --         function(server_name)
            --             local server = servers[server_name] or {}
            --             -- This handles overriding only values explicitly passed
            --             -- by the server configuration above. Useful when disabling
            --             -- certain features of an LSP (for example, turning off formatting for ts_ls)
            --             server.capabilities = vim.tbl_deep_extend('force', {}, capabilities, server.capabilities or {})
            --             require('lspconfig')[server_name].setup(server)
            --         end,
            --     },
            -- }
            --

            -- [[ custom config ]]
            -- Loop through each server in the servers table and set it up
            for server_name, config in pairs(servers) do
                -- config.handlers = handlers
                config.capabilities = capabilities
                lspconfig[server_name].setup(config)
            end
        end
    }, { -- Autoformat
        'stevearc/conform.nvim',
        lazy = false,
        keys = {
            {
                '<leader>f',
                function()
                    require('conform').format {
                        async = true,
                        lsp_fallback = true
                    }
                end,
                mode = '',
                desc = '[F]ormat buffer'
            }
        },
        opts = {
            notify_on_error = false,
            format_on_save = function(bufnr)
                -- Disable "format_on_save lsp_fallback" for languages that don't
                -- have a well standardized coding style. You can add additional
                -- languages here or re-enable it for the disabled ones.
                local disable_filetypes = {c = true, cpp = true}
                local lsp_format_opt
                if disable_filetypes[vim.bo[bufnr].filetype] then
                    lsp_format_opt = 'never'
                else
                    lsp_format_opt = 'fallback'
                end
                return {timeout_ms = 500, lsp_format = lsp_format_opt}
            end,
            formatters_by_ft = {
                c = {"clang-format"},
                cpp = {"clang-format"},
                nim = {"nimpretty"},
                elixir = {"mix"},
                zig = {"zig fmt"},
                lua = {'stylua'},
                sh = {"shfmt"},
                rust = {"rustfmt"},
                -- Conform can also run multiple formatters sequentially
                -- python = { "isort", "black" },
                python = function(bufnr)
                    if require("conform").get_formatter_info("ruff_format",
                                                             bufnr).available then
                        return {"ruff_format"}
                    else
                        return {"isort", "black"}
                    end
                end,
                -- You can use a sub-list to tell conform to run *until* a formatter
                -- is found.
                javascript = {{"prettierd", "prettier"}}
            }
        }
    }, { -- Autocompletion
        'saghen/blink.cmp',
        -- optional: provides snippets for the snippet source
        dependencies = 'rafamadriz/friendly-snippets',

        -- use a release tag to download pre-built binaries
        version = '*',
        ---@module 'blink.cmp'
        ---@type blink.cmp.Config
        opts = {
            -- 'default' for mappings similar to built-in completion
            -- 'super-tab' for mappings similar to vscode (tab to accept, arrow keys to navigate)
            -- 'enter' for mappings similar to 'super-tab' but with 'enter' to accept
            -- See the full "keymap" documentation for information on defining your own keymap.
            keymap = {preset = 'enter'},

            completion = {
                menu = {
                    enabled = true,
                    min_width = 15,
                    max_height = 10,
                    border = 'single',
                    winblend = 0,
                    winhighlight = 'Normal:BlinkCmpMenu,FloatBorder:BlinkCmpMenuBorder,CursorLine:BlinkCmpMenuSelection,Search:None',
                    -- Keep the cursor X lines away from the top/bottom of the window
                    scrolloff = 2,
                    -- Note that the gutter will be disabled when border ~= 'none'
                    scrollbar = true,
                    -- Which directions to show the window,
                    -- falling back to the next direction when there's not enough space
                    direction_priority = {'s', 'n'},

                    -- Whether to automatically show the window when new completion items are available
                    auto_show = true,

                    -- Screen coordinates of the command line
                    cmdline_position = function()
                        if vim.g.ui_cmdline_pos ~= nil then
                            local pos = vim.g.ui_cmdline_pos -- (1, 0)-indexed
                            return {pos[1] - 1, pos[2]}
                        end
                        local height = (vim.o.cmdheight == 0) and 1 or
                                           vim.o.cmdheight
                        return {vim.o.lines - height, 0}
                    end
                }
            },

            appearance = {
                -- Sets the fallback highlight groups to nvim-cmp's highlight groups
                -- Useful for when your theme doesn't support blink.cmp
                -- Will be removed in a future release
                use_nvim_cmp_as_default = true,
                -- Set to 'mono' for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
                -- Adjusts spacing to ensure icons are aligned
                nerd_font_variant = 'normal'
            },

            -- Default list of enabled providers defined so that you can extend it
            -- elsewhere in your config, without redefining it, due to `opts_extend`
            sources = {
                default = {'lsp', 'path', 'snippets', 'buffer'},
                -- Disable cmdline completions
                cmdline = {}
            },
            signature = {enabled = true}
        },
        opts_extend = {"sources.default"}
    }, { -- You can easily change to a different colorscheme.
        -- Change the name of the colorscheme plugin below, and then
        -- change the command in the config to whatever the name of that colorscheme is.
        --
        -- If you want to see what colorschemes are already installed, you can use `:Telescope colorscheme`.
        "catppuccin/nvim",
        priority = 1000, -- Make sure to load this before all the other start plugins.
        init = function()
            require("catppuccin").setup({
                flavour = "mocha" -- latte, frappe, macchiato, mocha
            })
            -- Load the colorscheme here.
            -- Like many other themes, this one has different styles, and you could load
            -- any other, such as 'tokyonight-storm', 'tokyonight-moon', or 'tokyonight-day'.
            vim.cmd.colorscheme 'catppuccin-mocha'

            -- You can configure highlights by doing something like:
            vim.cmd.hi 'Comment gui=none'
        end
    }, -- Highlight todo, notes, etc in comments
    {
        'folke/todo-comments.nvim',
        event = 'VimEnter',
        dependencies = {'nvim-lua/plenary.nvim'},
        opts = {signs = false}
    }, { -- Collection of various small independent plugins/modules
        'echasnovski/mini.nvim',
        config = function()
            -- Better Around/Inside textobjects
            --
            -- Examples:
            --  - va)  - [V]isually select [A]round [)]paren
            --  - yinq - [Y]ank [I]nside [N]ext [']quote
            --  - ci'  - [C]hange [I]nside [']quote
            require('mini.ai').setup {n_lines = 500}

            -- Add/delete/replace surroundings (brackets, quotes, etc.)
            --
            -- - saiw) - [S]urround [A]dd [I]nner [W]ord [)]Paren
            -- - sd'   - [S]urround [D]elete [']quotes
            -- - sr)'  - [S]urround [R]eplace [)] [']
            require('mini.surround').setup()

            -- -- Simple and easy statusline.
            -- --  You could remove this setup call if you don't like it,
            -- --  and try some other statusline plugin
            -- local statusline = require 'mini.statusline'
            -- -- set use_icons to true if you have a Nerd Font
            -- statusline.setup { use_icons = vim.g.have_nerd_font }

            -- -- You can configure sections in the statusline by overriding their
            -- -- default behavior. For example, here we set the section for
            -- -- cursor location to LINE:COLUMN
            -- ---@diagnostic disable-next-line: duplicate-set-field
            -- statusline.section_location = function()
            --     return '%2l:%-2v'
            -- end

            -- ... and there is more!
            --  Check out: https://github.com/echasnovski/mini.nvim
        end
    }, -- { -- 🎉 Python Venv Selector
    --     "linux-cultist/venv-selector.nvim",
    --     dependencies = {
    --         "neovim/nvim-lspconfig",
    --         "mfussenegger/nvim-dap", "mfussenegger/nvim-dap-python", --optional
    --         { "nvim-telescope/telescope.nvim", branch = "0.1.x", dependencies = { "nvim-lua/plenary.nvim" } },
    --     },
    --     lazy = false,
    --     branch = "regexp", -- This is the regexp branch, use this for the new version
    --     config = function()
    --         require("venv-selector").setup()
    --     end,
    --     --  Call config for python files and load the cached venv automatically
    --     ft = "python",
    --     keys = { { "<leader>cv", "<cmd>:VenvSelect<cr>", desc = "Select VirtualEnv", ft = "python" } },
    -- },
    { -- Lualine
        'nvim-lualine/lualine.nvim',
        dependencies = {'nvim-tree/nvim-web-devicons'},
        config = function()
            -- Function to get the virtual environment name
            local function get_venv(variable)
                local venv = os.getenv(variable)
                if venv ~= nil then
                    -- Handle conda directly since it provides the environment name
                    if variable == "CONDA_DEFAULT_ENV" then
                        return venv
                    end

                    -- If it's a virtual environment (VIRTUAL_ENV), extract the parent directory if the path ends with '.venv'
                    if variable == "VIRTUAL_ENV" then
                        local parent_dir = venv:match("(.+)/[^/]+$") -- Get the parent directory path
                        local venv_name = parent_dir:match("([^/]+)$") -- Get the last part of the parent directory (the project name)
                        return venv_name
                    end
                end
                return nil -- Return nil if no virtual environment is found
            end

            -- Lualine setup
            require('lualine').setup {
                options = {
                    icons_enabled = true,
                    theme = 'auto',
                    component_separators = {left = '', right = ''},
                    section_separators = {left = '', right = ''},
                    disabled_filetypes = {statusline = {}, winbar = {}},
                    ignore_focus = {},
                    always_divide_middle = true,
                    globalstatus = false,
                    refresh = {statusline = 1000, tabline = 1000, winbar = 1000}
                },
                sections = {
                    lualine_a = {'mode'},
                    lualine_b = {'branch', 'diff', 'diagnostics'},
                    lualine_c = {'filename'},
                    lualine_x = {
                        -- Add the virtual environment display here
                        {
                            function()
                                local venv =
                                    get_venv("CONDA_DEFAULT_ENV") or
                                        get_venv("VIRTUAL_ENV")
                                if venv then
                                    return " " .. venv -- Return virtual environment with Python icon
                                else
                                    return "" -- Return nothing if no virtual environment is found
                                end
                            end,
                            cond = function()
                                return vim.bo.filetype == "python"
                            end -- Only show in Python files
                        }, 'filetype'
                    },
                    lualine_y = {'progress'},
                    lualine_z = {'location'}
                },
                inactive_sections = {
                    lualine_a = {},
                    lualine_b = {},
                    lualine_c = {'filename'},
                    lualine_x = {'location', 'encoding', 'fileformat'},
                    lualine_y = {},
                    lualine_z = {}
                },
                tabline = {},
                winbar = {},
                inactive_winbar = {},
                extensions = {}
            }
        end
    }, { -- Highlight, edit, and navigate code
        'nvim-treesitter/nvim-treesitter',
        build = ':TSUpdate',
        main = 'nvim-treesitter.configs', -- Sets main module to use for opts
        -- [[ Configure Treesitter ]] See `:help nvim-treesitter`
        opts = {
            ensure_installed = {
                'bash', 'c', 'diff', 'html', 'lua', 'luadoc', 'markdown',
                'markdown_inline', 'query', 'vim', 'vimdoc'
            },
            -- Autoinstall languages that are not installed
            auto_install = true,
            highlight = {
                enable = true,
                -- Some languages depend on vim's regex highlighting system (such as Ruby) for indent rules.
                --  If you are experiencing weird indenting issues, add the language to
                --  the list of additional_vim_regex_highlighting and disabled languages for indent.
                additional_vim_regex_highlighting = {'ruby'}
            },
            indent = {enable = true, disable = {'ruby'}}
        }
        -- There are additional nvim-treesitter modules that you can use to interact
        -- with nvim-treesitter. You should go explore a few and see what interests you:
        --
        --    - Incremental selection: Included, see `:help nvim-treesitter-incremental-selection-mod`
        --    - Show your current context: https://github.com/nvim-treesitter/nvim-treesitter-context
        --    - Treesitter + textobjects: https://github.com/nvim-treesitter/nvim-treesitter-textobjects
    }, { -- Better escape
        "max397574/better-escape.nvim",
        config = function() require("better_escape").setup() end
    },
    { -- Highly experimental plugin that completely replaces the UI for messages, cmdline and the popupmenu.
        "folke/noice.nvim",
        event = "VeryLazy",
        opts = {
            -- add any options here
        },
        dependencies = {
            -- if you lazy-load any plugin below, make sure to add proper `module="..."` entries
            "MunifTanjim/nui.nvim", -- OPTIONAL:
            --   `nvim-notify` is only needed, if you want to use the notification view.
            --   If not available, we use `mini` as the fallback
            "rcarriga/nvim-notify"
        },
        config = function()
            require("noice").setup({
                lsp = {
                    -- override markdown rendering so that **cmp** and other plugins use **Treesitter**
                    override = {
                        ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
                        ["vim.lsp.util.stylize_markdown"] = true,
                        ["cmp.entry.get_documentation"] = true -- requires hrsh7th/nvim-cmp
                    }
                },
                -- you can enable a preset for easier configuration
                presets = {
                    bottom_search = true, -- use a classic bottom cmdline for search
                    command_palette = true, -- position the cmdline and popupmenu together
                    long_message_to_split = true, -- long messages will be sent to a split
                    inc_rename = false, -- enables an input dialog for inc-rename.nvim
                    lsp_doc_border = true -- add a border to hover docs and signature help
                }
            })
        end
    },

    -- The following two comments only work if you have downloaded the kickstart repo, not just copy pasted the
    -- init.lua. If you want these files, they are in the repository, so you can just download them and
    -- place them in the correct locations.

    -- NOTE: Next step on your Neovim journey: Add/Configure additional plugins for Kickstart
    --
    --  Here are some example plugins that I've included in the Kickstart repository.
    --  Uncomment any of the lines below to enable them (you will need to restart nvim).
    --
    require 'kickstart.plugins.debug', require 'kickstart.plugins.indent_line',
    require 'kickstart.plugins.lint', require 'kickstart.plugins.autopairs',
    require 'kickstart.plugins.neo-tree', require 'kickstart.plugins.gitsigns' -- adds gitsigns recommend keymaps

    -- NOTE: The import below can automatically add your own plugins, configuration, etc from `lua/custom/plugins/*.lua`
    --    This is the easiest way to modularize your config.
    --
    --  Uncomment the following line and add your plugins to `lua/custom/plugins/*.lua` to get going.
    --    For additional information, see `:help lazy.nvim-lazy.nvim-structuring-your-plugins`
    -- { import = 'custom.plugins' },
}, {
    ui = {
        border = BORDER_STYLE,
        -- If you are using a Nerd Font: set icons to an empty table which will use the
        -- default lazy.nvim defined Nerd Font icons, otherwise define a unicode icons table
        icons = vim.g.have_nerd_font and {} or {
            cmd = '⌘',
            config = '🛠',
            event = '📅',
            ft = '📂',
            init = '⚙',
            keys = '🗝',
            plugin = '🔌',
            runtime = '💻',
            require = '🌙',
            source = '📄',
            start = '🚀',
            task = '📌',
            lazy = '💤 '
        }
    }
})

-- The line beneath this is called `modeline`. See `:help modeline`
-- vim: ts=2 sts=2 sw=2 et
