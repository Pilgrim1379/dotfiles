# Always set aliases _last,_ so they don't class with function definitions.

## Easier navigation: .., ..., ...., ....., ~ and -
alias ..='cd ..'
alias ...='cd ../..'

# Create alias override commands using 'eza'
alias ls='eza --group-directories-first --icons'

# Use the --git flag if the installed version of eza supports git
# Related to https://github.com/ogham/exa/issues/978
if eza --version | grep -q '+git';
then
	alias ll='ls -lh --git'
else
	alias ll='ls -lh'
fi

alias la='ll -a'
alias tree='ll --tree --level=3'

## Neovim switcher
alias v="NVIM_APPNAME=LazyVim nvim"
alias lazyvim="NVIM_APPNAME=LazyVim nvim"

alias vim="echo 'Just use v'"
alias vi="echo 'Just use v'"

## NPM
alias npmlsg='npm ls -g --depth 0'

## Update all ppackages
# echo '\nstarting PNPM update ...' && pnpm list -g --json | jq '.[] | .dependencies | keys | .[]' -r  | xargs pnpm add -g && \
# echo '\nstarting NPM update ...' && npm -g update && corepack prepare pnpm@latest --activate && \
# echo '\nstarting PNPM update ...' && pnpm update -g --latest && \
# echo '\nstarting Julia update ...' && juliaup self update && juliaup update && juliaup gc &&\
alias updallapps="caffeinate \
echo 'starting HOMEBREW update ...' && brew update && brew upgrade --formula && brew cu -aqy --no-brew-update --cleanup && \
echo '\nstarting NPM update ...' && npm -g update && corepack prepare pnpm@latest --activate && \
echo '\nstarting Rust update ...' && rustup update && \
echo '\nstarting Ruby update ...' && gem update --system && gem update && \
echo '\nstarting Go update ...' && gup update &&\
echo '\nstarting UV update ...' && uv self update && uv tool upgrade --all \
"

## App updates

alias bunupd="\
echo '\nstarting BUN update ...' && bun update -g \
"

alias miseupd="\
echo 'starting MISE update ...' && mise self-update && mise upgrade \
"
alias brewupd="\
echo 'starting HOMEBREW update ...' && brew update && brew upgrade --formula && brew cu -aqy --no-brew-update --cleanup \
"

alias npmupd="\
echo 'starting NPM update ...' && npm -g update && corepack prepare pnpm@latest --activate && \
echo '\nstarting PNPM update ...' && pnpm update -g --latest \
"

# alias pipxupd="echo 'starting PIPX update ...' && pipx upgrade-all"

alias gemupd="\
echo 'starting Ruby gem update ...' && gem update --system && gem update
"

alias brweupd="\
echo 'starting HOMEBREW update ...' && brew update && brew upgrade --formula
"

alias rustupd="\
echo 'starting Rust update ...' && rustup update
"
alias bunupd="\
echo 'starting Bun update ...' && bun update -g
"

alias gupd="\
echo 'starting Go update ...' && gup update
"

## Vim/Neovim
alias nvimsync="nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'"
alias resetnvim="rm -rf ~/.local/share/nvim && rm -rf ~/.local/state/nvim && rm -rf ~/.cache/nvim && echo 'All Neovim data deleted successfully ...'"
alias resetlazyvim="rm -rf ~/.local/share/LazyVim && rm -rf ~/.local/state/LazyVim && rm -rf ~/.cache/LazyVim && echo 'All LazyVim data deleted successfully ...'"
alias updnvim='brew upgrade neovim --fetch-HEAD'

## Emacs
# alias emacs="emacs -nw"
alias emd='emacs --daemon' # run emacs as daemon mode with
#alias ec="emacsclient -cnqua ''" # the void string '' is IMPORTANT

## Pipx/Pip
#alias pipx="python -m pipx"
alias pipuninstallall="pip uninstall -y -r <(pip freeze)"
alias pipupgradeall="pip list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install -U"

## Lances/htop
alias htop='btop'

## Chezmoi
alias cmcd='chezmoi cd'
alias cmedit='chezmoi edit'
alias cmapply='chezmoi apply'
alias cmdiff='chezmoi diff'
alias cmadd='chezmoi add'
alias cmdoc='chezmoi doctor'
alias cmfget='chezmoi forget'

## brew
alias brewc='brew cleanup -s'
alias brewcc='brew cleanup --prune=all'

# npm cache clean
alias npmcc='npm cache clean --force && npm cache verify'

# clean all caches
alias cleancaches="\
echo 'cleaning homebrew cache ...' && brewcc && \
echo 'cleaning npm cache ...' && npmcc
"

# +----+
# | cp |
# +----+

alias cp='cp -iv'
alias mv='mv -iv'
alias rm='rm -iv'

## Gnu command replacement
alias tar='gtar'
alias indent='gindent'
alias getopt='/usr/local/opt/gnu-getopt/bin/getopt'

# +-----+
# | Git |
# +-----+

alias gs='git status'
alias gss='git status -s'
alias ga='git add'
alias gp='git push'
alias gpraise='git blame'
alias gpo='git push origin'
alias gpof='git push origin --force-with-lease'
alias gpofn='git push origin --force-with-lease --no-verify'
alias gpt='git push --tag'
alias gtd='git tag --delete'
alias gtdr='git tag --delete origin'
alias grb='git branch -r'                                                                           # display remote branch
alias gplo='git pull origin'
alias gb='git branch '
alias gc='git commit'
alias gd='git diff'
alias gco='git checkout '
alias gl='git log --oneline'
alias gr='git remote'
alias grs='git remote show'
alias glol='git log --graph --abbrev-commit --oneline --decorate'
alias gclean="git branch --merged | grep  -v '\\*\\|master\\|develop' | xargs -n 1 git branch -d" # Delete local branch merged with master
alias gblog="git for-each-ref --sort=committerdate refs/heads/ --format='%(HEAD) %(color:red)%(refname:short)%(color:reset) - %(color:yellow)%(objectname:short)%(color:reset) - %(contents:subject) - %(authorname) (%(color:blue)%(committerdate:relative)%(color:reset))'"                                                             # git log for each branches
alias gsub="git submodule update --remote"                                                        # pull submodules
alias gj="git-jump"

# Folders
alias work="$HOME/workspace"
alias doc="$HOME/Documents"
alias dow="$HOME/Downloads"
# alias dot="$HOME/.dotfiles"

# +--------+
# | docker |
# +--------+
alias dockls="docker container ls | awk 'NR > 1 {print \$NF}'"                  # display names of running containers
alias dockRr='docker rm $(docker ps -a -q)'                                     # delete every containers / images
alias dockRr='docker rm $(docker ps -a -q) && docker rmi $(docker images -q)'   # delete every containers / images
alias dockstats='docker stats $(docker ps -q)'                                  # stats on images
alias dockimg='docker images'                                                   # list images installed
alias dockprune='docker system prune -a'                                        # prune everything
alias dockceu='docker-compose run --rm -u $(id -u):$(id -g)'                    # run as the host user
alias dockce='docker-compose run --rm'


# Canonical hex dump; some systems have this symlinked
command -v hd >/dev/null || alias hd='hexdump -C'

# macOS has no `md5sum`, so use `md5` as a fallback
command -v md5sum >/dev/null || alias md5sum='md5'

# macOS has no `sha1sum`, so use `shasum` as a fallback
command -v sha1sum >/dev/null || alias sha1sum='shasum'

# System maintainance aliases

# Show/hide hidden files in Finder
alias show='defaults write com.apple.finder AppleShowAllFiles -bool true && killall Finder'
alias hide='defaults write com.apple.finder AppleShowAllFiles -bool false && killall Finder'

# Hide/show all desktop icons (useful when presenting)
alias hidedesktop='defaults write com.apple.finder CreateDesktop -bool false && killall Finder'
alias showdesktop='defaults write com.apple.finder CreateDesktop -bool true && killall Finder'

# Trim new lines and copy to clipboard
# alias c="tr -d '\n' | pbcopy"

# Recursively delete `.DS_Store` files
alias cleanup="find . -type f -name '*.DS_Store' -ls -delete"

# Empty the Trash on all mounted volumes and the main HDD.
# Also, clear Apple’s System Logs to improve shell startup speed.
# Finally, clear download history from quarantine. https://mths.be/bum
alias emptytrash="sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl; sqlite3 ~/Library/Preferences/com.apple.LaunchServices.QuarantineEventsV* 'delete from LSQuarantineEvent'"

# URL-encode strings
alias urlencode='python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1]);"'

# PlistBuddy alias, because sometimes `defaults` just doesn’t cut it
alias plistbuddy='/usr/libexec/PlistBuddy'

# Clean up LaunchServices and Open with list to remove duplicates in the “Open With” menu
alias cleanupopenwith="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user && killall Finder"

# Launchpad reset
alias cleanuplaunchpad="defaults write com.apple.dock ResetLaunchPad -bool true && killall Dock"

# Kill all the tabs in Chrome to free up memory
# [C] explained: http://www.commandlinefu.com/commands/view/402/exclude-grep-from-your-grepped-output-of-ps-alias-included-in-description
# alias chromekill="ps ux | grep '[C]hrome Helper --type=renderer' | grep -v extension-process | tr -s ' ' | cut -d ' ' -f2 | xargs kill"

# mkdir: create parent directories
alias mkdir='mkdir -pv'

## ping: stop after 5 pings
alias ping='ping -c 5'

## Help command allows using "help export" eg to get help on built in commands
alias help='run-help'

## Get week number
alias week='date +%V'

## zip/unzip
alias ungz='gunzip -k'

## Reload the shell (i.e. invoke as a login shell)
alias reload='exec ${SHELL} -l'

## Print each PATH entry on a separate line
alias path='echo -e ${PATH//:/\\n}'

## Mongodb
alias mongod='mongod --config /usr/local/etc/mongod.conf'

## File size
alias fs="stat -f \"%z bytes\""

# REF: https://github.com/ohmyzsh/ohmyzsh/blob/master/plugins/common-aliases/common-aliases.plugin.zsh
# Advanced Aliases.
# Use with caution
#

alias zshrc='${=EDITOR} ${ZDOTDIR:-$HOME}/.zshrc' # Quick access to the .zshrc file

alias \
    diff='diff --color' \
    grep='grep --color' \
    egrep='egrep --color' \
    make='make -j' \
    {\$,%}=  # For pasting command line examples

alias sgrep='grep -R -n -H -C 5 --exclude-dir={.git,.svn,CVS} '

# alias t='tail -f'

alias dud='du -d 1 -h'
(( $+commands[duf] )) || alias duf='du -sh *'
(( $+commands[fd] )) || alias fd='find . -type d -name'
alias ff='find . -type f -name'


alias hgrep="fc -El 0 | grep"
alias ps='ps -f'
alias sortnr='sort -n -r'
alias unexport='unset'

alias pfzf="fzf --preview 'bat --color=always --style=numbers --line-range=:500 {}'"

## Jupyter
alias jl='jupyter lab'
alias jn='jupyter notebook'
