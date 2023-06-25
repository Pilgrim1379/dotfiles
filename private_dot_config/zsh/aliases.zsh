#!/bin/zsh

# aliases
#
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

alias \
    diff='diff --color' \
    grep='grep --color' \
    egrep='egrep --color' \
    make='make -j' \
    {\$,%}=  # For pasting command line examples


## Pattern matching support for `cp`, `ln` and `mv`
## See http://zsh.sourceforge.net/Doc/Release/User-Contributions.html#index-zmv
## Tip: Use -n for no execution. (Print what would happen, but don’t do it.)
autoload -Uz zmv
alias \
    zmv='zmv -v' \
    zcp='zmv -Cv' \
    zln='zmv -Lv'

## Paging & colors for `ls`
# ls() {
#   command ${${OSTYPE:#linux-gnu}:+g}ls --width=$COLUMNS "$@" | $PAGER
#   return $pipestatus[1]  # Return exit status of ls, not $PAGER
# }
# 

# Git
alias gs='git status'
alias ga='git add'
alias gp='git push'
alias gpo='git push origin'
alias gtd='git tag --delete'
alias gtdr='git tag --delete origin'
alias gr='git branch -r'
alias gplo='git pull origin'
alias gb='git branch'
alias gc='git commit'
alias gd='git diff'
alias gco='git checkout '
alias gl='git log'
alias gr='git remote'
alias grs='git remote show'
alias glo='git log --pretty="oneline"'
alias glol='git log --graph --oneline --decorate'


zstyle ':completion:*:ls:*:options' ignored-patterns --width
if [ "$OSTYPE" != linux-gnu ]; then  # Is this the macOS system?
  alias ls='gls -AFBvx --group-directories-first --color=auto'
fi

## List all files colorized in long format, including dot files
alias ll='ls -lhF'
alias lt='du -sh * | sort -h'
alias lc='ls -CF'
## Show hidden files ##
alias l.='ls -dF .*'
alias left='ls -t -1' # find where you left off:

## Easier navigation: .., ..., ...., ....., ~ and -
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ~='cd ~' # `cd` is probably faster to type though
alias -- -='cd -'

alias zshconfig="cd ${ZDOTDIR}"
alias zshplugins="cd ${ZDOTDIR}/plugins"

## mkdir: create parent directories
alias mkdir='mkdir -pv' 

# Make and cd into directory
#  - from: http://alias.sh/make-and-cd-directory
mkcd() {
  mkdir "$1" && cd "$1"
}

## ping: stop after 5 pings
alias ping='ping -c 5' 

## Help command allows using "help export" eg to get help on built in commands
alias help='run-help'

## Get week number
alias week='date +%V'

## Neovim switcher
alias lazynvim="NVIM_APPNAME=LazyVim nvim"
# alias kickstnvim="NVIM_APPNAME=kickstart nvim"
# alias chadnvim="NVIM_APPNAME=NvChad nvim"
# alias astronvim="NVIM_APPNAME=AstroNvim nvim"

## PNPM
alias pnpmupg='pnpm up -g -L'
alias pnpmup='pnpm up'
alias pnpmupdev='pnpm up --dev'
alias pnpmlsg='pnpm ls -g --depth 0'
alias pnpmiupg='pnpm up -g -i -L'
alias pnpmoutdated='pnpm outdated -g'

## NPM
alias npmlsg='npm ls -g --depth 0'
alias npmup='npm install -g npm'


## Update all ppackages
# echo '\nstarting PNPM update ...' && pnpm list -g --json | jq '.[] | .dependencies | keys | .[]' -r  | xargs pnpm add -g && \
alias updallapps="\
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

alias brewupgc='brew upgrade --cask'
alias brweupgc='brew upgrade --cask'
alias brewupgcg='brew upgrade --cask --greedy'
alias brweupgcg='brew upgrade --cask --greedy'
alias brewcu='brew cu -ay --no-brew-update --cleanup'
alias brwecu='brew cu -ay --no-brew-update --cleanup'
alias brewdoc='brew doctor'
alias brwedoc='brew doctor'
alias brewclean='brew cleanup -s'
alias brweclean='brew cleanup -s'

## Reload the shell (i.e. invoke as a login shell)
alias reload='exec ${SHELL} -l'

## Print each PATH entry on a separate line
alias path='echo -e ${PATH//:/\\n}'

## Mongodb
alias mongod='mongod --config /usr/local/etc/mongod.conf'

## File size
alias fs="stat -f \"%z bytes\""

## Vim/Neovim
alias nvimsync="nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'"
alias rmnvim="rm -rf ~/.config/local/share/nvim && rm -rf ~/.local/share/nvim && rm -rf ~/.cache/nvim && rm -rf ~/.config/nvim/plugin && echo 'All Neovim configs deleted successfully ...'"
alias updnvim='brew upgrade neovim --fetch-HEAD'

## zip/unzip
alias ungz='gunzip -k'

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

## Gnu command replacement
alias tar='gtar'
alias indent='gindent'
alias getopt='/usr/local/opt/gnu-getopt/bin/getopt'

## Disk usage (find size of folder or disk)
alias dus='du -sh'
alias duas='du -ash'

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
alias cmforget='chezmoi forget'

# Show/hide hidden files in Finder
alias show='defaults write com.apple.finder AppleShowAllFiles -bool true && killall Finder'
alias hide='defaults write com.apple.finder AppleShowAllFiles -bool false && killall Finder'

# Hide/show all desktop icons (useful when presenting)
alias hidedesktop='defaults write com.apple.finder CreateDesktop -bool false && killall Finder'
alias showdesktop='defaults write com.apple.finder CreateDesktop -bool true && killall Finder'

# Trim new lines and copy to clipboard
alias c="tr -d '\n' | pbcopy"

# Recursively delete `.DS_Store` files
alias cleanup="find . -type f -name '*.DS_Store' -ls -delete"

# Empty the Trash on all mounted volumes and the main HDD.
# Also, clear Apple’s System Logs to improve shell startup speed.
# Finally, clear download history from quarantine. https://mths.be/bum
alias emptytrash="sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl; sqlite3 ~/Library/Preferences/com.apple.LaunchServices.QuarantineEventsV* 'delete from LSQuarantineEvent'"

# URL-encode strings
alias urlencode='python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1]);"'

# Disable Spotlight
alias spotoff='sudo mdutil -a -i off'

# Enable Spotlight
alias spoton='sudo mdutil -a -i on'

# PlistBuddy alias, because sometimes `defaults` just doesn’t cut it
alias plistbuddy='/usr/libexec/PlistBuddy'

# Clean up LaunchServices to remove duplicates in the “Open With” menu
alias lscleanup="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user && killall Finder"

# Kill all the tabs in Chrome to free up memory
# [C] explained: http://www.commandlinefu.com/commands/view/402/exclude-grep-from-your-grepped-output-of-ps-alias-included-in-description
alias chromekill="ps ux | grep '[C]hrome Helper --type=renderer' | grep -v extension-process | tr -s ' ' | cut -d ' ' -f2 | xargs kill"

# luamake
alias luamake='~/lang-servers/lua-language-server/3rd/luamake/luamake'

# Improved myip alias. Echoed to avoid strange character at end in ZSH.
myip() {
  echo "$(curl -s whatismyip.akamai.com)"
}