#!/bin/zsh

# +--------------+
# | EMACS KEYMAP |
# +--------------+
# bindkey -e

# +-----------+
# | VI KEYMAP |
# +-----------+
bindkey -v
export KEYTIMEOUT=1
# source $HOMEBREW_PREFIX/opt/zsh-vi-mode/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh

# Change cursor
source "$ZDOTDIR/plugins/cursor_mode.zsh"

# Add Vi text-objects for brackets and quotes
autoload -Uz select-bracketed select-quoted
zle -N select-quoted
zle -N select-bracketed
for km in viopp visual; do
  bindkey -M $km -- '-' vi-up-line-or-history
  for c in {a,i}${(s..)^:-\'\"\`\|,./:;=+@}; do
    bindkey -M $km $c select-quoted
  done
  for c in {a,i}${(s..)^:-'()[]{}<>bB'}; do
    bindkey -M $km $c select-bracketed
  done
done

# Emulation of vim-surround
autoload -Uz surround
zle -N delete-surround surround
zle -N add-surround surround
zle -N change-surround surround
bindkey -M vicmd cs change-surround
bindkey -M vicmd ds delete-surround
bindkey -M vicmd ys add-surround
bindkey -M visual S add-surround

# Use Ctrl-A to start neovim switcher
# bindkey -s '^a' "nvims\n"

# +---------+
# | BINDING |
# +---------+
# edit current command line with vim (vim-mode, then CTRL-v)
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey -M vicmd '^v' edit-command-line

bindkey '^X' autosuggest-execute
# For example, this would bind ctrl + space to accept the current suggestion.
bindkey '^ ' autosuggest-accept
# bindkey '^E' autosuggest-accept
bindkey '^A' vi-beginning-of-line
bindkey '^E' vi-end-of-line


# # zsh-edit
# # Bind shell commands directly to keyboard shortcuts. What's more, when using these, your current command line will be left intact.
# bindkey '^[:' 'cd ..'
# bindkey '^[-' 'pushd -1' \
#         '^[=' 'pushd +0'
# bindkey '^[q' push-line-or-edit
# bindkey -r '^Q' '^[Q'