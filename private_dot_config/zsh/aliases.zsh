#!/bin/zsh
# Always set aliases _last,_ so they don't class with function definitions.
#
# This lets you change to any dir without having to type `cd`, that is, by just
# typing its name. Be warned, though: This can misfire if there exists an alias,
# function, builtin or command with the same name.
# In general, I would recommend you use only the following without `cd`:
#   ..  to go one dir up
#   ~   to go to your home dir
#   ~-2 to go to the 2nd mostly recently visited dir
#   /   to go to the root dir
setopt AUTO_CD

# Note that, unlike Bash, there's no need to inform Zsh's completion system
# of your aliases. It will figure them out automatically.

## Set $PAGER if it hasn't been set yet. We need it below.
## `:` is a builtin command that does nothing. We use it here to stop Zsh from
## evaluating the value of our $expansion as a command.
: ${PAGER:=less}

## Use `< file` to quickly view the contents of any file.
READNULLCMD=$PAGER  # Set the program to use for this.

## Pattern matching support for `cp`, `ln` and `mv`
## See http://zsh.sourceforge.net/Doc/Release/User-Contributions.html#index-zmv
## Tip: Use -n for no execution. (Print what would happen, but don’t do it.)
autoload -Uz zmv
alias \
    zmv='zmv -v' \
    zcp='zmv -Cv' \
    zln='zmv -Lv'


## Easier navigation: .., ..., ...., ....., ~ and -
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ~='cd ~' # `cd` is probably faster to type though
alias -- -='cd -'

## Neovim switcher
alias v="NVIM_APPNAME=LazyVim nvim"
alias lazyvim="NVIM_APPNAME=LazyVim nvim"
# alias kickstnvim="NVIM_APPNAME=kickstart nvim"
# alias chadnvim="NVIM_APPNAME=NvChad nvim"
# alias astronvim="NVIM_APPNAME=AstroNvim nvim"

alias vim="echo 'Just use v'"
alias vi="echo 'Just use v'"

## PNPM
alias pnpmupg='pnpm up -g -L'
alias pnpmup='pnpm up'
alias pnpmupdev='pnpm up --dev'
alias pnpmlsg='pnpm ls -g --depth 0'
alias pnpmiupg='pnpm up -g -i -L'
alias pnpmoutdated='pnpm outdated -g'

## NPM
alias npmlsg='npm ls -g --depth 0'
# alias npmup='npm install -g npm'

## Update all ppackages
# echo '\nstarting PNPM update ...' && pnpm list -g --json | jq '.[] | .dependencies | keys | .[]' -r  | xargs pnpm add -g && \
alias updallapps="caffeinate \
echo 'starting HOMEBREW update ...' && brew update && brew upgrade --formula && brew cu -aqy --no-brew-update --cleanup && \
echo '\nstarting NPM update ...' && npm -g update && corepack prepare pnpm@latest --activate && \
echo '\nstarting PNPM update ...' && pnpm update -g --latest && \
echo '\nstarting PIPX update ...' && pipx upgrade-all && \
echo '\nstarting Rust update ...' && rustup update && \
echo '\nstarting Ruby update ...' && gem update --system && gem update \
"

## without asdf
alias brewupd="\
echo 'starting HOMEBREW update ...' && brew update && brew upgrade --formula && brew cu -aqy --no-brew-update --cleanup \
"

alias npmupd="\
echo 'starting NPM update ...' && npm -g update && corepack prepare pnpm@latest --activate && \
echo '\nstarting PNPM update ...' && pnpm update -g --latest \
"

alias pipxupd="echo 'starting PIPX update ...' && pipx upgrade-all"

alias gemupd="\
echo 'starting Ruby gem update ...' && gem update --system && gem update
"

alias brweupd="\
echo 'starting HOMEBREW update ...' && brew update && brew upgrade --formula
"

alias rustupd="\
echo 'starting Rust update ...' && rustup update
"

## with asdf
# alias brewupd="\
# echo 'starting HOMEBREW update ...' && brew update && brew upgrade --formula && brew cu -ay --no-brew-update --cleanup \
# echo '\nstarting NPM update ...' && npm -g update && corepack prepare pnpm@latest --activate && asdf reshim nodejs && \
# echo '\nstarting PNPM update ...' && pnpm update -g -L && \
# echo '\nstarting NCU update ...' && ncu -g \
# "

# alias brweupd="\
# echo 'starting HOMEBREW update ...' && brew update && brew upgrade --formula && brew cu -ay --no-brew-update --cleanup \
# echo '\nstarting NPM update ...' && npm -g update && corepack prepare pnpm@latest --activate && asdf reshim nodejs && \
# echo '\nstarting PNPM update ...' && pnpm update -g -L && \
# echo '\nstarting NCU update ...' && ncu -g \
# "

## Vim/Neovim
alias nvimsync="nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'"
alias resetnvim="rm -rf ~/.local/share/nvim && rm -rf ~/.local/state/nvim && rm -rf ~/.cache/nvim && echo 'All Neovim data deleted successfully ...'"
alias resetlazyvim="rm -rf ~/.local/share/LazyVim && rm -rf ~/.local/state/LazyVim && rm -rf ~/.cache/LazyVim && echo 'All LazyVim data deleted successfully ...'"
alias updnvim='brew upgrade neovim --fetch-HEAD'

## Emacs
# alias emacs="emacs -nw"
alias emd='emacs --daemon' # run emacs as daemon mode with
#alias ec="emacsclient -cnqua ''" # the void string '' is IMPORTANT

## Asdf
#alias asdfup="asdf update --head"
#alias asdfupall="asdf update --head && asdf plugin-update --all"
# alias asdfplugup='asdf plugin-update --all'

## Pipx/Pip
#alias pipx="python -m pipx"
alias pipuninstallall="pip uninstall -y -r <(pip freeze)"
alias pipupgradeall="pip list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install -U"

## Lances/htop
alias htop='glances'

alias pfzf="fzf --preview 'bat --color=always --style=numbers --line-range=:500 {}'"

## Jupyter
alias jl='jupyter lab'
alias jn='jupyter notebook'

## Chezmoi
alias cmcd='chezmoi cd'
alias cmedit='chezmoi edit'
alias cmapply='chezmoi apply'
alias cmdiff='chezmoi diff'
alias cmadd='chezmoi add'
alias cmdoc='chezmoi doctor'
alias cmfget='chezmoi forget'
