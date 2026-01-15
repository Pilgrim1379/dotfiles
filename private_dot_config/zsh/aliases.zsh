# Only load aliases in interactive shells
[[ -o interactive ]] || return 0

# -------------------------
# Navigation
# -------------------------
alias ..='cd ..'
alias ...='cd ../..'

# -------------------------
# eza (ls replacement)
# -------------------------
if (( $+commands[eza] )); then
  alias ls='eza --group-directories-first --icons'

  # Use --git if supported by installed eza
  if eza --version 2>/dev/null | command grep -q '+git'; then
    alias ll='eza --group-directories-first --icons -lh --git'
  else
    alias ll='eza --group-directories-first --icons -lh'
  fi

  alias la='ll -a'
  alias tree='ll --tree --level=3'
fi

# -------------------------
# NPM / Node
# -------------------------
alias npmlsg='npm ls -g --depth 0'

# -------------------------
# Vim/Neovim
# -------------------------
alias nvimsync="nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'"
alias resetnvim='rm -rf -- ~/.local/share/nvim ~/.local/state/nvim ~/.cache/nvim && echo "All Neovim data deleted successfully ..."'
alias resetlazyvim='rm -rf -- ~/.local/share/LazyVim ~/.local/state/LazyVim ~/.cache/LazyVim && echo "All LazyVim data deleted successfully ..."'
alias updnvim='brew upgrade neovim --fetch-HEAD'

# -------------------------
# Emacs
# -------------------------
alias emd='emacs --daemon'

# -------------------------
# Pip / Python
# -------------------------
alias pipuninstallall="pip uninstall -y -r <(pip freeze)"
alias pipupgradeall="pip list --outdated --format=freeze | command grep -v '^\-e' | cut -d = -f 1 | xargs -n1 pip install -U"

# -------------------------
# Top / Monitor
# -------------------------
(( $+commands[btop] )) && alias htop='btop'

# -------------------------
# Chezmoi
# -------------------------
alias cmcd='chezmoi cd'
alias cmedit='chezmoi edit'
alias cmapply='chezmoi apply'
alias cmdiff='chezmoi diff'
alias cmadd='chezmoi add'
alias cmdoc='chezmoi doctor'
alias cmfget='chezmoi forget'

# -------------------------
# Homebrew cleanup
# -------------------------
alias brewc='brew cleanup -s'
alias brewcc='brew cleanup --prune=all'

# npm cache clean
alias npmcc='npm cache clean --force && npm cache verify'

# -------------------------
# Safer file ops (interactive)
# -------------------------
alias cp='nocorrect cp -iv'
alias mv='nocorrect mv -iv'
alias rm='nocorrect rm -iv'

# -------------------------
# GNU command replacement (guarded)
# -------------------------
command -v gtar >/dev/null 2>&1 && alias tar='gtar'
command -v gindent >/dev/null 2>&1 && alias indent='gindent'
[[ -x /opt/homebrew/opt/gnu-getopt/bin/getopt ]] && alias getopt='/opt/homebrew/opt/gnu-getopt/bin/getopt'
[[ -x /usr/local/opt/gnu-getopt/bin/getopt ]] && alias getopt='/usr/local/opt/gnu-getopt/bin/getopt'

# -------------------------
# Git
# -------------------------
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
alias grb='git branch -r'
alias gplo='git pull origin'
alias gb='git branch'
alias gc='git commit'
alias gd='git diff'
alias gco='git checkout'
alias gl='git log --oneline'
alias gr='git remote'
alias grs='git remote show'
alias glol='git log --graph --abbrev-commit --oneline --decorate'
alias gclean="git branch --merged | command grep -vE '^\*|master|main|develop' | xargs -n 1 git branch -d"
alias gblog="git for-each-ref --sort=committerdate refs/heads/ --format='%(HEAD) %(color:red)%(refname:short)%(color:reset) - %(color:yellow)%(objectname:short)%(color:reset) - %(contents:subject) - %(authorname) (%(color:blue)%(committerdate:relative)%(color:reset))'"
alias gsub='git submodule update --remote'
alias gj='git-jump'

# -------------------------
# Docker (simple aliases only; smart ones are functions)
# -------------------------
alias dockls="docker container ls | awk 'NR > 1 {print \$NF}'"
alias dockimg='docker images'
alias dockprune='docker system prune -a'

# Support both docker-compose and docker compose
if (( $+commands[docker-compose] )); then
  alias dockceu='docker-compose run --rm -u $(id -u):$(id -g)'
  alias dockce='docker-compose run --rm'
else
  alias dockceu='docker compose run --rm -u $(id -u):$(id -g)'
  alias dockce='docker compose run --rm'
fi

# -------------------------
# Hash / hex compat
# -------------------------
command -v hd >/dev/null 2>&1 || alias hd='hexdump -C'
command -v md5sum >/dev/null 2>&1 || alias md5sum='md5'
command -v sha1sum >/dev/null 2>&1 || alias sha1sum='shasum'

# -------------------------
# macOS maintenance
# -------------------------
alias show='defaults write com.apple.finder AppleShowAllFiles -bool true && killall Finder'
alias hide='defaults write com.apple.finder AppleShowAllFiles -bool false && killall Finder'
alias hidedesktop='defaults write com.apple.finder CreateDesktop -bool false && killall Finder'
alias showdesktop='defaults write com.apple.finder CreateDesktop -bool true && killall Finder'
alias cleanupds_store="find . -type f -name '*.DS_Store' -ls -delete"
alias emptytrash="sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl; sqlite3 ~/Library/Preferences/com.apple.LaunchServices.QuarantineEventsV* 'delete from LSQuarantineEvent'"
alias urlencode='python3 -c "import sys, urllib.parse as up; print(up.quote_plus(sys.argv[1]))"'
alias plistbuddy='/usr/libexec/PlistBuddy'
alias cleanupopenwith="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user && killall Finder"
alias cleanuplaunchpad="defaults write com.apple.dock ResetLaunchPad -bool true && killall Dock"

# -------------------------
# General
# -------------------------
alias mkdir='mkdir -pv'
alias ping='ping -c 5'
alias help='run-help'
alias week='date +%V'
alias ungz='gunzip -k'
alias reload='exec ${SHELL} -l'

# Print each PATH entry on a separate line (zsh-native)
alias path='print -rl -- ${(s.:.)PATH}'

# Mongodb
alias mongod='mongod --config /usr/local/etc/mongod.conf'

# Mise
# eg: mise-debug install --jobs=1
alias mise-debug='MISE_DEBUG=1 MISE_VERBOSE=1 mise'

# File size (BSD stat)
alias fs='stat -f "%z bytes"'

# Quick access to .zshrc
alias zshrc='${=EDITOR} ${ZDOTDIR:-$HOME}/.zshrc'

# Find helpers (don’t shadow fd)
alias fdd='find . -type d -name'
alias ff='find . -type f -name'

alias hgrep="fc -El 0 | grep"
alias ps='ps -f'
alias sortnr='sort -n -r'
alias unexport='unset'

# fzf preview with bat (guarded)
(( $+commands[fzf] && $+commands[bat] )) && alias pfzf="fzf --preview 'bat --color=always --style=numbers --line-range=:500 {}'"

# -------------------------
# Jupyter
# -------------------------
alias jl='jupyter lab'
alias jn='jupyter notebook'
