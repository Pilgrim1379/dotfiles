#!/bin/zsh

# Commands, functions and aliases
#

## Neovim config switcher
function nvims() {
  items=(
    "default"
    "LazyVim"
    )
  config=$(printf "%s\n" "${items[@]}" | fzf --prompt=" Neovim Config 󰄾 " --height=~50% --layout=reverse --border --exit-0)
  if [[ -z $config ]]; then
    echo "Nothing selected"
    return 0
  elif [[ $config == "default" ]]; then
    config=""
  fi
  NVIM_APPNAME=$config nvim $@
}

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

# Canonical hex dump; some systems have this symlinked
command -v hd >/dev/null || alias hd='hexdump -C'

# macOS has no `md5sum`, so use `md5` as a fallback
command -v md5sum >/dev/null || alias md5sum='md5'

# macOS has no `sha1sum`, so use `shasum` as a fallback
command -v sha1sum >/dev/null || alias sha1sum='shasum'


# Search through all man pages
function fman() {
    man -k . | fzf -q "$1" --prompt='man> '  --preview $'echo {} | tr -d \'()\' | awk \'{printf "%s ", $2} {print $1}\' | xargs -r man' | tr -d '()' | awk '{printf "%s ", $2} {print $1}' | xargs -r man
}

# Improved myip alias. Echoed to avoid strange character at end in ZSH.
myip() {
  echo "$(curl -s whatismyip.akamai.com)"
}

# System maintainance aliases

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

alias zshconfig="cd ${ZDOTDIR}"
alias zshplugins="cd ${ZDOTDIR}/plugins"

## These don't change frequently enough to be sourced in precmd
# mkdir: create parent directories
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