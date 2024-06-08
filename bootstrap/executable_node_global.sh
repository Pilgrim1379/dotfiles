#!/bin/zsh

## Install default global package for node using pnpm

## upgrade npm
npm install -g npm

## importing helper
npm install -g import-js

## neovim
# npm install -g neovim

## prettier
npm install -g prettier prettier-plugin-elm

npm install -g cliui@latest
npm install -g npm-check@latest
npm install -g npm-check-updates@latest

## Corepack and package managers
#
corepack enable
corepack prepare pnpm@latest --activate
corepack prepare yarn@stable --activate

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
# npm install -g vscode-langservers-extracted ## consolidated for html, css, json and eslint
npm install -g vscode-json-languageservice
npm install -g yaml-language-server
npm install -g diagnostic-languageserver
npm install -g pyright
npm install -g emmet-ls
# npm install -g vscode-markdown-languageserver


# ## when using asdf, this is required for pnpm commands to work
# # asdf reshim nodejs

if command -v pnpm &> /dev/null
then
    ## typescript
    pnpm add -g typescript


    ## working with npm
    #pnpm add -g npx
    #pnpm add -g np
    #pnpm add -g npm-name-cli

    ## misc
    pnpm add -g spoof # Easily spoof your MAC address in macOS, Windows, & Linux!
    pnpm add -g wscat # Permits connecting to, sending data to, and receiving data from a WebSocket

    ## Type `git open` to open the GitHub page or website for a repository.
    pnpm add -g git-open

    ## fancy listing of recent branches
    pnpm add -g git-recent

    ## ghost
    # pnpm add -g ghost-cli@latest

    ## dev frameworks
    # pnpm add -g @quasar/cli
    # pnpm add -g cordova
    # pnpm add -g framework7-cli
    
    
    pnpm add -g less
    pnpm add -g stylus

    ## formatter and linters
    pnpm add -g eslint
    pnpm add -g js-beautify
    pnpm add -g stylelint

    ## live server
    pnpm add -g live-server

    ## remix/solidity/etherium
    # Usage: remixd -s <shared folder> --remix-ide https://rddddddemix.ethereum.org
    # pnpm add -g @remix-project/remixd

    ## languages
    # pnpm add -g rescript

    ## build tools
    # Not recommended to install webpack globaly (option -D is same as --save-dev for npm)
    #pnpm add -D webpack
    #pnpm add -D webpack-cli
fi

# mise
if (( $+commands[mise] )); then
    mise reshim
fi

# echo -e "\nPackage installation complete - Don't forget to reshim so that new packages can be recognised"
echo -e "\nIf required run 'pnpm install-completion' to get completions working or pnpm."
echo -e "Installation complete."
