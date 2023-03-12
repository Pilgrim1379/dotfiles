#!/bin/zsh
##
# Plugins

#
# Syntax highlight config 
# 
ZSH_HIGHLIGHT_HIGHLIGHTERS=( main brackets ) # # Command-line syntax highlighting config

#
# In-line suggestions
# 
ZSH_AUTOSUGGEST_ACCEPT_WIDGETS=()
ZSH_AUTOSUGGEST_PARTIAL_ACCEPT_WIDGETS+=( forward-char forward-word end-of-line )
ZSH_AUTOSUGGEST_STRATEGY=( history )
ZSH_AUTOSUGGEST_HISTORY_IGNORE=$'(*\n*|?(#c80,)|*\\#:hist:push-line:)'

#
# Autocomplete config
# The code below sets all of Autocomplete's settings to their default values. To
# change a setting, copy it into your `.zshrc` file and modify it there.
# 
zstyle ':autocomplete:*' default-context ''
# '': Start each new command line with normal autocompletion.
# history-incremental-search-backward: Start in live history search mode.

zstyle ':autocomplete:*' min-delay 0.05  # seconds (float)
# Wait this many seconds for typing to stop, before showing completions.

zstyle ':autocomplete:*' min-input 1  # characters (int)
# Wait until this many characters have been typed, before showing completions.

zstyle ':autocomplete:*' ignored-input '' # extended glob pattern
# '':     Always show completions.
# '..##': Don't show completions for the current word, if it consists of two
#         or more dots.

zstyle ':autocomplete:*' list-lines 16  # int
# If there are fewer than this many lines below the prompt, move the prompt up
# to make room for showing this many lines of completions (approximately).

zstyle ':autocomplete:history-search:*' list-lines 16  # int
# Show this many history lines when pressing ↑.

zstyle ':autocomplete:history-incremental-search-*:*' list-lines 16  # int
# Show this many history lines when pressing ⌃R or ⌃S.

zstyle ':autocomplete:*' insert-unambiguous no
# no:  Tab inserts the top completion.
# yes: Tab first inserts a substring common to all listed completions, if any.

# Add a space after these completions:
zstyle ':autocomplete:*' add-space \
    executables aliases functions builtins reserved-words commands

##
# Config in this section should come BEFORE sourcing Autocomplete and cannot be
# changed at runtime.
#

zstyle ':autocomplete:*' fzf-completion no
# no:  Tab uses Zsh's completion system only.
# yes: Tab first tries Fzf's completion, then falls back to Zsh's.
# ⚠️ NOTE: This setting can NOT be changed at runtime and requires that you
# have installed Fzf's shell extensions.

# Autocomplete automatically selects a backend for its recent dirs completions.
# So, normally you won't need to change this.
# However, you can set it if you find that the wrong backend is being used.
zstyle ':autocomplete:recent-dirs' backend zoxide
# cdr:  Use Zsh's `cdr` function to show recent directories as completions.
# no:   Don't show recent directories.
# zsh-z|zoxide|z.lua|z.sh|autojump|fasd: Use this instead (if installed).
# ⚠️ NOTE: This setting can NOT be changed at runtime.

zstyle ':autocomplete:*' widget-style complete-word
# complete-word: (Shift-)Tab inserts the top (bottom) completion.
# menu-complete: Press again to cycle to next (previous) completion.
# menu-select:   Same as `menu-complete`, but updates selection in menu.
# ⚠️ NOTE: This setting can NOT be changed at runtime.


## Sheldon load plugins
# eval "$(sheldon source)" # comment out if not using znap


## Help with manual completion config instead of allowind zsh-autocomplete to
## to it : 
## https://stackoverflow.com/questions/67136714/how-to-properly-call-compinit-and-bashcompinit-in-zsh

# autoload -Uz compinit
# compinit

## or

# autoload -Uz compinit bashcompinit
# compinit
# bashcompinit

## 
# Speed up the first startup by cloning all plugins in parallel.
# This won't clone plugins that we already have.

#
## Znap load plugins
# For more info on each plugin, visit its repo at github.com/<plugin>
# -a sets the variable's type to array.
local -a plugins=(
    marlonrichert/zsh-autocomplete      # Real-time type-ahead completion
    zsh-users/zsh-autosuggestions       # Inline suggestions
    zsh-users/zsh-syntax-highlighting   # Command-line syntax highlighting
    zsh-users/zsh-completions           # Additional completion definitions for Zsh
    romkatv/powerlevel10k               # Powerlevel10k is a prompt theme for Zsh
)
znap clone $plugins

# Load each plugin, one at a time.
local p=
for p in $plugins; do
    znap source $p
done

# Auto-installed by Brew, but far worse than the one supplied by Zsh
rm -f $HOMEBREW_PREFIX/share/zsh/site-functions/_git{,.zwc}

# Use `znap fpath` to add generated completion functions:
if (( $+commands[rustup] )); then
    znap fpath _rustup 'rustup completions zsh'
fi

if (( $+commands[cargo] )); then
    znap fpath _cargo 'rustup completions zsh cargo'
fi

# if (( $+commands[solana] )); then
    # znap fpath _solana 'solana completion --shell zsh'
# fi

if (( $+commands[pdm] )); then
    znap fpath _pdm 'pdm completion zsh'
fi

##
# Config in this section should come AFTER sourcing Autocomplete and cannot be
# changed at runtime.
#

# Better cding like z.lua but faster
if (( $+commands[zoxide] )); then
    # eval "$(zoxide init zsh)"  # comment out if not using znap
    # Better cding like z.lua but faster
    znap eval zoxide 'zoxide init zsh'
fi

# Direnv hooked into asdf
# if (( $+commands[direnv] )); then
#     # eval "$(asdf exec direnv hook zsh)"  # comment out if not using znap
#     znap eval asdf-community/asdf-direnv "asdf exec $(asdf which direnv) hook zsh"
# fi

# Pipenv
if (( $+commands[pipenv] )); then
    # eval "$(_PIPENV_COMPLETE=zsh_source pipenv)"  # comment out if not using znap
    znap function _pipenv_completion pipenv 'eval "$( _PIPENV_COMPLETE=zsh_source pipenv )"'
    compdef _pipenv_completion pipenv
fi

# Pipx
if (( $+commands[pipx] )); then
    # eval "$(register-python-argcomplete pipx)"  # comment out if not using znap
    znap function _pipx_completion pipx 'eval "$( register-python-argcomplete pipx )"'
    compdef _pipx_completion pipx
fi

# Pnpm completion
# tabtab source for packages
# uninstall by removing these lines
# [[ -f ~/.config/tabtab/zsh/__tabtab.zsh ]] && . ~/.config/tabtab/zsh/__tabtab.zsh || true
# 
