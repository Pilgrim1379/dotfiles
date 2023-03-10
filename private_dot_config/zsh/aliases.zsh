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

# Use `< file` to quickly view the contents of any file.
READNULLCMD=$PAGER  # Set the program to use for this.

alias \
    diff='diff --color' \
    grep='grep --color' \
    egrep='egrep --color' \
    make='make -j' \
    {\$,%}=  # For pasting command line examples


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
alias la='ls -lahF'
alias ll='ls -lhF'
alias lt='du -sh * | sort -h'
## Show hidden files ##
alias l.='ls -dF .*'
alias left='ls -t -1' # find where you left off:

# Easier navigation: .., ..., ...., ....., ~ and -
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ~='cd ~' # `cd` is probably faster to type though
alias -- -='cd -'

# mkdir: create parent directories
alias mkdir='mkdir -pv' 

# ping: stop after 5 pings
alias ping='ping -c 5' 

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
alias pnpmoutdated='pnpm -g outdated'

# npm
alias npmlsg='npm ls -g --depth 0'
alias npmup='npm install -g npm'

# Update installed Ruby gems, Homebrew, npm, and their installed packages
#alias brew_update="brew -v update; brew upgrade --force-bottle; brew cleanup; npm-check -gu"
alias brewupdate='brew -v update && brew upgrade && pnpm up -g -L && ncu -g'
alias brweupdate='brew -v update && brew upgrade && pnpm up -g -L && ncu -g'
alias brewupd='brew -v update'
alias brweupd='brew -v update'

alias brewupg="brew upgrade --formula && echo '\nstarting NPM update ...' && npm -g update && \
corepack prepare pnpm@latest --activate && asdf reshim nodejs && echo '\nstarting PNPM update ...' && \
pnpm -g -L update && echo '\nstarting NCU update ...' && ncu -g"

alias brweupg="brew upgrade --formula && echo '\nstarting NPM update ...' && npm -g update && \
corepack prepare pnpm@latest --activate && corepack prepare yarn@stable --activate && asdf reshim nodejs && \
echo '\nstarting PNPM update ...' && pnpm -g -L update && echo '\nstarting NCU update ...' && ncu -g"

alias brewupgc='brew upgrade --cask'
alias brweupgc='brew upgrade --cask'
alias brewupgcg='brew upgrade --cask --greedy'
alias brweupgcg='brew upgrade --cask --greedy'
alias brewcu='brew cu -a -y --no-brew-update --cleanup'
alias brwecu='brew cu -a -y --no-brew-update --cleanup'

# Reload the shell (i.e. invoke as a login shell)
alias reload='exec ${SHELL} -l'

# Print each PATH entry on a separate line
alias path='echo -e ${PATH//:/\\n}'

# mongodb
alias mongod='mongod --config /usr/local/etc/mongod.conf'

# File size
alias fs="stat -f \"%z bytes\""

# vim
alias vim='lvim'
alias nvim='lvim'

# zip/unzip
alias ungz='gunzip -k'

# emacs
alias emd='emacs --daemon' # run emacs as daemon mode with
#alias ec="emacsclient -cnqua ''" # the void string '' is IMPORTANT

# asdf
#alias asdfup="asdf update --head"
#alias asdfupall="asdf update --head && asdf plugin-update --all"
alias asdfplugup='asdf plugin-update --all'

# pipx / pip
#alias pipx="python -m pipx"
alias pipuninstallall="pip uninstall -y -r <(pip freeze)"
alias pipupgradeall="pip list --outdated --format=freeze | grep -v '^\-e' | cut -d = -f 1  | xargs -n1 pip install -U"

# gnu command replacement
alias tar='gtar'
alias indent='gindent'
alias getopt='/usr/local/opt/gnu-getopt/bin/getopt'

# disk usage (find size of folder or disk)
alias dus='du -sh'
alias duas='du -ash'

# glances/htop
alias htop='glances'

alias pfzf="fzf --preview 'bat --color=always --style=numbers --line-range=:500 {}'"

# Jupyter
alias jl='jupyter lab'
alias jn='jupyter notebook'

# Neovim
alias nvimsync="nvim --headless -c 'autocmd User PackerComplete quitall' -c 'PackerSync'"
alias rmnvim="rm -rf ~/.config/local/share/nvim && rm -rf ~/.local/share/nvim && rm -rf ~/.cache/nvim && rm -rf ~/.config/nvim/plugin && echo 'All Neovim configs deleted successfully ...'"
alias nvimupd='brew reinstall neovim'

# Chezmoi
alias cmcd='chezmoi cd'
alias cmedit='chezmoi edit'
alias cmapply='chezmoi apply'
alias cmdiff='chezmoi diff'
alias cmadd='chezmoi add'
alias cmdoc='chezmoi doctor'
