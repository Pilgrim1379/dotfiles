#!/bin/zsh

## Install default global package for bun

## importing helper
bun add -g import-js

## neovim
# bun add -g neovim

## prettier
bun add -g prettier prettier-plugin-elm

bun add -g cliui@latest

## elm
bun add -g elm-format
bun add -g elm-live
bun add -g elm-oracle
bun add -g elm-test
bun add -g elm-review

## language servers
bun add -g typescript-language-server
bun add -g bash-language-server
bun add -g @elm-tooling/elm-language-server
bun add -g @tailwindcss/language-server
# bun add -g vscode-langservers-extracted ## consolidated for html, css, json and eslint
bun add -g vscode-languageserver
bun add -g yaml-language-server
bun add -g diagnostic-languageserver
bun add -g pyright
bun add -g emmet-ls

## typescript
bun add -g typescript

## working with npm
#bun add -g npx
#bun add -g np
#bun add -g npm-name-cli

## misc
bun add -g spoof # Easily spoof your MAC address in macOS, Windows, & Linux!
bun add -g wscat # Permits connecting to, sending data to, and receiving data from a WebSocket

## Type `git open` to open the GitHub page or website for a repository.
bun add -g git-open

## fancy listing of recent branches
bun add -g git-recent

## ghost
# bun add -g ghost-cli@latest

## dev frameworks
# bun add -g @quasar/cli
# bun add -g cordova
# bun add -g framework7-cli

bun add -g less
bun add -g stylus

## formatter and linters
bun add -g eslint
bun add -g js-beautify
bun add -g stylelint

## live server
bun add -g live-server

## remix/solidity/etherium
# Usage: remixd -s <shared folder> --remix-ide https://rddddddemix.ethereum.org
# bun add -g @remix-project/remixd

## languages
# bun add -g rescript

# echo -e "\nPackage installation complete - Don't forget to reshim so that new packages can be recognised"
echo -e "\nIf required run 'bun completions' to get completions working."
echo -e "Installation complete."
