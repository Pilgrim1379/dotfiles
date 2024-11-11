#!/usr/bin/env zsh

## Install default global package for node using pnpm

## upgrade npm
npm install -g npm

## importing helper
npm install -g import-js

## neovim
npm install -g neovim

## prettier
npm install -g prettier prettier-plugin-elm
npm install -g @fsouza/prettierd

npm install -g cliui@latest
npm install -g npm-check@latest
npm install -g npm-check-updates@latest

## Corepack and package managers
#
corepack enable

corepack enable pnpm
corepack use pnpm@latest

# corepack enable yarn
# corepack use yarn@latest

npm install -g less

## elm
npm install -g elm-format
npm install -g elm-live
npm install -g elm-oracle
npm install -g elm-test
npm install -g elm-review

## language servers
npm install -g typescript-language-server
npm install -g bash-language-server
npm install -g @elm-tooling/elm-language-server
npm install -g @tailwindcss/language-server
npm install -g vscode-langservers-extracted ## consolidated for html, css, json and eslint
# npm install -g vscode-languageserver
npm install -g yaml-language-server
npm install -g diagnostic-languageserver
npm install -g pyright
npm install -g emmet-ls
npm install -g vscode-markdown-languageserver
npm install -g @vue/language-server

# languages
npm install -g typescript

# ## when using asdf, this is required for pnpm commands to work
# # asdf reshim nodejs

if command -v pnpm &> /dev/null
then
    echo -e "Installing PNPM packages ..."
    # working with npm
    # pnpm add -g npx
    pnpm add -g np
    pnpm add -g npm-name-cli

    # misc
    pnpm add -g wscat # Permits connecting to, sending data to, and receiving data from a WebSocket

    # fancy listing of recent branches
    pnpm add -g git-recent

    # ghost
    pnpm add -g ghost-cli@latest

    # dev frameworks
    # pnpm add -g @quasar/cli
    # pnpm add -g cordova
    # pnpm add -g framework7-cli
    
    pnpm add -g stylus

    # formatter and linters
    pnpm add -g eslint
    pnpm add -g js-beautify
    pnpm add -g stylelint

    echo -e "Finished installing PNPM packages"
fi

# mise
if (( $+commands[mise] )); then
    mise reshim
fi

# echo -e "\nPackage installation complete - Don't forget to reshim so that new packages can be recognised"
echo -e "\nIf required run 'pnpm install-completion' to get completions working or pnpm."
echo -e "Installation complete."
