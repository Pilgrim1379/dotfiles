#!/usr/bin/env zsh

set -e

echo "🔧 Ensuring Node.js is installed via mise (required for npm backend)..."
mise use -g node

echo "📦 Installing global npm tools via mise's npm backend..."

# Each item in this associative array maps: tool => description
declare -A tools=(
  # 🧰 Editor/Plugin Runtime Tools
  [neovim]="Node.js support for Neovim plugins (e.g., coc.nvim, Treesitter integrations)"
  [typescript]="TypeScript compiler (tsc) and core tooling"

  # 🧹 Code Formatters / Linters / Beautifiers
  [prettier]="Code formatter for many languages"
  [prettier-plugin-elm]="Elm plugin for Prettier"
  [@fsouza/prettierd]="Prettier daemon for fast formatting (editor integration)"
  [eslint]="JavaScript/TypeScript linter"
  [stylelint]="Linter for CSS and other stylesheets"
  [js-beautify]="Formatter for HTML, CSS, JS"fneovim
  [elm-format]="Elm code formatter"

  # 📦 Package Utilities / Dev Tools
  [cliui]="CLI user interface building tool (used by other CLIs)"
  [npm-check]="See which npm packages are outdated"
  [npm-check-updates]="Upgrade `package.json` deps to latest versions"
  [np]="Helper for safely publishing npm packages"
  [npm-name-cli]="Checks npm package name availability"
  [pnpm]="Fast, disk-efficient package manager (alternative to npm/yarn)"

  # 📡 Network / Comm Tools
  [wscat]="WebSocket CLI client"
  [git-recent]="Lists recently used Git branches"

  # 🌐 Language Servers (for LSP clients like Neovim/VSCode)
  [typescript-language-server]="LSP server for TypeScript"
  [bash-language-server]="LSP server for Bash"
  [@elm-tooling/elm-language-server]="Elm language server"
  [@tailwindcss/language-server]="Tailwind CSS language server"
  [vscode-langservers-extracted]="Bundle: HTML, CSS, JSON LSP servers"
  [yaml-language-server]="LSP server for YAML"
  [diagnostic-languageserver]="Adapter for formatters/linters as LSP"
  [basedpyright]="Lightweight Pyright LSP (Python)"
  [emmet-ls]="LSP for Emmet (HTML/CSS shortcuts)"
  [vscode-markdown-languageserver]="Markdown LSP from VSCode"
  [@vue/language-server]="LSP for Vue.js"

  # 🧪 Elm Dev Tools
  [elm-live]="Dev server for Elm projects"
  [elm-oracle]="Provides editor autocomplete for Elm"
  [elm-test]="Elm test runner"
  [elm-review]="Linting/review tool for Elm"

  # 🧰 Misc Developer Tools
  [import-js]="Auto-import JS modules based on usage"
  [neovim]="Node.js host for Neovim (needed for some plugins)"
  [typescript]="TypeScript compiler and tools"
  [less]="CSS preprocessor"
  [stylus]="Another CSS preprocessor"
)

for tool in "${!tools[@]}"; do
  echo "➡️  Installing npm:$tool – ${tools[$tool]}"
  mise use -g "npm:$tool"
done

echo "✅ All npm tools installed and globally available via mise!"
