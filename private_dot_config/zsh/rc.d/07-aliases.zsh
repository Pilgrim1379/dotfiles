#!/bin/zsh

##
# Commands, funtions and aliases
#
# Always set aliases _last,_ so they don't class with function definitions.
#

# Note that, unlike Bash, there's no need to inform Zsh's completion system
# of your aliases. It will figure them out automatically.

# Set $PAGER if it hasn't been set yet. We need it below.
# `:` is a builtin command that does nothing. We use it here to stop Zsh from
# evaluating the value of our $expansion as a command.
: ${PAGER:=less}

# Associate file .extensions with programs.
# This lets you open a file just by typing its name and pressing enter.
# Note that the dot is implicit. So, `gz` below stands for files ending in .gz
alias -s {css,gradle,html,js,json,md,patch,properties,txt,xml,yml}=$PAGER
alias -s gz='gzip -l'
alias -s {log,out}='tail -F'


# Use `< file` to quickly view the contents of any file.
READNULLCMD=$PAGER  # Set the program to use for this.

alias \
    diff='diff --color' \
    grep='grep --color' \
    egrep='egrep --color' \
    make='make -j' \
    {\$,%}=  # For pasting command line examples

# File type associations
alias -s \
    gz='gzip -l' \
    {gradle,json,md,patch,properties,txt,xml,yml}=$PAGER

if [[ $VENDOR == apple ]]; then
  alias -s \
      {log,out}='open -a Console'
else
  alias -s \
      {log,out}='code' \
      deb='deb'
  deb() {
    sudo apt install "$@[1,-2]" "$@[-1]:P"
  }
fi

# Pattern matching support for `cp`, `ln` and `mv`
# See http://zsh.sourceforge.net/Doc/Release/User-Contributions.html#index-zmv
# Tip: Use -n for no execution. (Print what would happen, but don’t do it.)
autoload -Uz zmv
alias \
    zmv='zmv -v' \
    zcp='zmv -Cv' \
    zln='zmv -Lv'

# Paging & colors for `ls`
# ls() {
#   command ${${OSTYPE:#linux-gnu}:+g}ls --width=$COLUMNS "$@" | $PAGER
#   return $pipestatus[1]  # Return exit status of ls, not $PAGER
# }
# 

zstyle ':completion:*:ls:*:options' ignored-patterns --width
if [ "$OSTYPE" != linux-gnu ]; then  # Is this the macOS system?
  alias ls='gls -AFvx --group-directories-first --color=auto'
fi

# alias \
#     ls='gls --group-directories-first --color -AFvx'

# # List all files colorized in long format, including dot files
alias la="ls -lahF"
alias ll='ls -Flh'
alias lt='du -sh * | sort -h'
## Show hidden files ##
alias l.="ls -dF .*"
alias left='ls -t -1' # find where you left off:

# Safer alternatives to `rm`
if [[ $VENDOR == apple ]]; then
  trash() {
    local -aU items=( $^@(N) )
    local -aU missing=( ${@:|items} )
    (( $#missing )) &&
        print -u2 "trash: no such file(s): $missing"
    (( $#items )) ||
        return 66
    print Moving $( eval ls -d -- ${(q)items[@]%/} ) to Trash.
    items=( '(POSIX file "'${^items[@]:A}'")' )
    osascript -e 'tell application "Finder" to delete every item of {'${(j:, :)items}'}' \
        > /dev/null
  }
elif command -v gio > /dev/null; then
  # gio is available for macOS, but gio trash DOES NOT WORK correctly there.
  alias \
      trash='gio trash'
fi

#
#### My Own #####
#
# Easier navigation: .., ..., ...., ....., ~ and -
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ~="cd ~" # `cd` is probably faster to type though
alias -- -="cd -"

# mkdir: create parent directories
alias mkdir="mkdir -pv" 

# ping: stop after 5 pings
alias ping="ping -c 5" 

# coreutils
# alias dircolors='gdircolors'

# findutils

# Help command allows using "help export" eg to get help on built in commands
alias help='run-help'

# Get week number
alias week='date +%V'

# Pnpm
alias pnpmupg='pnpm up -g -L'
alias pnpmup='pnpm up'
alias pnpmupdev='pnpm up --dev'
alias pnpmlsg='pnpm ls -g --depth 0'
alias pnpmiupg='pnpm up -g -i -L'

# npm
alias npmlsg='npm ls -g --depth 0'
alias npmup='npm install -g npm'

# Get macOS Software Updates, and update installed Ruby gems, Homebrew, npm, and their installed packages
alias sudo_update_system='sudo softwareupdate -i -a && \
brew update && brew upgrade && brew cleanup && npm install -g npm && \
pnpm up -g && sudo gem update --system && sudo gem update && sudo gem cleanup'

alias updatesystem="brew upgrade --formula && echo '\nstarting NPM update ...' && \
npm -g update && echo '\nstarting PNPM update ...' && pnpm -g -L update && \
echo '\nstarting NCU update ...' && ncu -g && echo '\nstarting PIPX update ...' pipx upgrade-all && \
echo '\nstarting GEM update ...' gem update --system && gem update && gem cleanup"

# Update installed Ruby gems, Homebrew, npm, and their installed packages
#alias brew_update="brew -v update; brew upgrade --force-bottle; brew cleanup; npm-check -gu"
alias brew_update="brew -v update && brew upgrade && pnpm up -g -L && ncu -g"
alias brewupd="brew -v update"
alias brewupg="brew upgrade --formula && echo '\nstarting NPM update ...' && npm -g update && \
corepack prepare pnpm@latest --activate && asdf reshim nodejs && echo '\nstarting PNPM update ...' && \
pnpm -g -L update && echo '\nstarting NCU update ...' && ncu -g"

alias brewupgc="brew upgrade --cask"
alias brewupgcg="brew upgrade --cask --greedy"
alias brewcu="brew cu -a -y --no-brew-update"

# Google Chrome
alias chrome='/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome'
alias canary='/Applications/Google\ Chrome\ Canary.app/Contents/MacOS/Google\ Chrome\ Canary'

# Clean up LaunchServices to remove duplicates in the “Open With” menu
alias lscleanup="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user && killall Finder"

# Canonical hex dump; some systems have this symlinked
command -v hd >/dev/null || alias hd="hexdump -C"

# macOS has no `md5sum`, so use `md5` as a fallback
command -v md5sum >/dev/null || alias md5sum="md5"

# macOS has no `sha1sum`, so use `shasum` as a fallback
command -v sha1sum >/dev/null || alias sha1sum="shasum"

# JavaScriptCore REPL
jscbin="/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc"
[ -e "${jscbin}" ] && alias jsc="${jscbin}"
unset jscbin

# Trim new lines and copy to clipboard
alias c="tr -d '\n' | pbcopy"

# Recursively delete `.DS_Store` files
alias cleanup="find . -type f -name '*.DS_Store' -ls -delete"

# Empty the Trash on all mounted volumes and the main HDD.
# Also, clear Apple’s System Logs to improve shell startup speed.
# Finally, clear download history from quarantine. https://mths.be/bum
alias emptytrash="sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl; sqlite3 ~/Library/Preferences/com.apple.LaunchServices.QuarantineEventsV* 'delete from LSQuarantineEvent'"

# Show/hide hidden files in Finder
alias show="defaults write com.apple.finder AppleShowAllFiles -bool true && killall Finder"
alias hide="defaults write com.apple.finder AppleShowAllFiles -bool false && killall Finder"

# Hide/show all desktop icons (useful when presenting)
alias hidedesktop="defaults write com.apple.finder CreateDesktop -bool false && killall Finder"
alias showdesktop="defaults write com.apple.finder CreateDesktop -bool true && killall Finder"

# URL-encode strings
alias urlencode='python -c "import sys, urllib as ul; print ul.quote_plus(sys.argv[1]);"'

# Disable Spotlight
alias spotoff="sudo mdutil -a -i off"
# Enable Spotlight
alias spoton="sudo mdutil -a -i on"

# PlistBuddy alias, because sometimes `defaults` just doesn’t cut it
alias plistbuddy="/usr/libexec/PlistBuddy"

# Kill all the tabs in Chrome to free up memory
# [C] explained: http://www.commandlinefu.com/commands/view/402/exclude-grep-from-your-grepped-output-of-ps-alias-included-in-description
alias chromekill="ps ux | grep '[C]hrome Helper --type=renderer' | grep -v extension-process | tr -s ' ' | cut -d ' ' -f2 | xargs kill"

# Reload the shell (i.e. invoke as a login shell)
alias reload='exec ${SHELL} -l'

# Print each PATH entry on a separate line
alias path='echo -e ${PATH//:/\\n}'

# elm
alias elm-update-registry='elm diff elm/core 1.0.0 1.0.1 > /dev/null'

# mongodb
alias mongod="mongod --config /usr/local/etc/mongod.conf"

# File size
alias fs="stat -f \"%z bytes\""

# vim
alias vim="lvim"
alias nvim="lvim"

# zip/unzip
alias ungz="gunzip -k"

# emacs
alias emd="emacs --daemon" # run emacs as daemon mode with
#alias ec="emacsclient -cnqua ''" # the void string '' is IMPORTANT

# asdf
#alias asdfup="asdf update --head"
#alias asdfupall="asdf update --head && asdf plugin-update --all"
alias asdfplugup="asdf plugin-update --all"

# pipx / pip
#alias pipx="python -m pipx"
alias pipuninstallall="pip uninstall -y -r <(pip freeze)"
alias pipupgradeall="pip list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install -U"

# gnu command replacement
alias tar="gtar"
alias indent="gindent"
alias getopt="/usr/local/opt/gnu-getopt/bin/getopt"

# disk usage (find size of folder or disk)
alias dus="du -sh"
alias duas="du -ash"

# glances/htop
alias htop="glances"

alias pfzf="fzf --preview 'bat --color=always --style=numbers --line-range=:500 {}'"

# luamake
alias luamake=/Users/nqa/lang-servers/lua-language-server/3rd/luamake/luamake

# Jupyter
alias jl="jupyter lab"
alias jn="jupyter notebook"

# Neovim
alias nvimsync="nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'"
alias rmnvim="rm -rf ~/.config/local/share/nvim && rm -rf ~/.local/share/nvim && rm -rf ~/.cache/nvim && rm -rf ~/.config/nvim/plugin && echo 'All Neovim configs deleted successfully ...'"
alias nvimupd="brew reinstall neovim"
