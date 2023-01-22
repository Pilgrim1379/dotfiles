#!/bin/zsh
##
# Plugins
#
ZSH_HIGHLIGHT_HIGHLIGHTERS=( main brackets pattern ) # # Command-line syntax highlighting config

ZSH_AUTOSUGGEST_ACCEPT_WIDGETS=()
ZSH_AUTOSUGGEST_PARTIAL_ACCEPT_WIDGETS+=(forward-char forward-word end-of-line)
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
ZSH_AUTOSUGGEST_HISTORY_IGNORE=$'(*\n*|?(#c80,)|*\\#:hist:push-line:)'

##
# Completion config
#

# The code below sets all of `zsh-autocomplete`'s settings to their default
# values.
zstyle ':autocomplete:*' default-context ''
# '': Start each new command line with normal autocompletion.
# history-incremental-search-backward: Start in live history search mode.

zstyle ':autocomplete:*' min-delay 0.05  # seconds (float)
# Wait this many seconds for typing to stop, before showing completions.

zstyle ':autocomplete:*' min-input 0  # characters (int)
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

zstyle ':autocomplete:*' recent-dirs zoxide
# cdr:  Use Zsh's `cdr` function to show recent directories as completions.
# no:   Don't show recent directories.
# zsh-z|zoxide|z.lua|z.sh|autojump|fasd: Use this instead (if installed).
# ⚠️ NOTE: This setting can NOT be changed at runtime.

zstyle ':autocomplete:*' insert-unambiguous yes
# no:  Tab inserts the top completion.
# yes: Tab first inserts a substring common to all listed completions, if any.

zstyle ':autocomplete:*' widget-style complete-word
# complete-word: (Shift-)Tab inserts the top (bottom) completion.
# menu-complete: Press again to cycle to next (previous) completion.
# menu-select:   Same as `menu-complete`, but updates selection in menu.
# ⚠️ NOTE: This setting can NOT be changed at runtime.

zstyle ':autocomplete:*' fzf-completion yes
# no:  Tab uses Zsh's completion system only.
# yes: Tab first tries Fzf's completion, then falls back to Zsh's.
# ⚠️ NOTE: This setting can NOT be changed at runtime and requires that you
# have installed Fzf's shell extensions.

# Add a space after these completions:
zstyle ':autocomplete:*' add-space \
    executables aliases functions builtins reserved-words commands

# Better cding like z.lua but faster
if (( $+commands[zoxide] )); then
    eval "$(zoxide init zsh)"
fi

# Direnv hooked into asdf
if (( $+commands[direnv] )); then
    eval "$(asdf exec direnv hook zsh)"
fi

# Pipenv
if (( $+commands[pipenv] )); then
    eval "$(_PIPENV_COMPLETE=zsh_source pipenv)"
fi

# Pipx
if (( $+commands[pipx] )); then
    eval "$( register-python-argcomplete pipx )"
fi

# Pnpm completion
# tabtab source for packages
# uninstall by removing these lines
# [[ -f ~/.config/tabtab/zsh/__tabtab.zsh ]] && . ~/.config/tabtab/zsh/__tabtab.zsh || true

