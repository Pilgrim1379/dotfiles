bindkey -v
export KEYTIMEOUT=1

# Change cursor
plug "$ZDOTDIR/.config/zsh/plugins/cursor_mode"

# Add Vi text-objects for brackets and quotes
autoload -Uz select-bracketed select-quoted
zle -N select-quoted
zle -N select-bracketed
for km in viopp visual; do
  bindkey -M $km -- '-' vi-up-line-or-history
  for c in {a,i}${(s..)^:-\'\"\`\|,./:;=+@}; do
    bindkey -M $km "$c" select-quoted
  done
  for c in {a,i}${(s..)^:-'()[]{}<>bB'}; do
    bindkey -M $km "$c" select-bracketed
  done
done
# Editing Command Lines In Vim
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey -M vicmd '^v' edit-command-line

# Emulation of vim-surround
autoload -Uz surround
zle -N delete-surround surround
zle -N add-surround surround
zle -N change-surround surround
bindkey -M vicmd cs change-surround
bindkey -M vicmd ds delete-surround
bindkey -M vicmd ys add-surround
bindkey -M visual S add-surround

zmodload zsh/complist
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

bindkey '^X' autosuggest-execute
# For example, this would bind ctrl + space to accept the current suggestion.
bindkey '^ ' autosuggest-accept
bindkey '^A' vi-beginning-of-line
bindkey '^E' vi-end-of-line

bindkey '^p' history-search-backward
bindkey '^n' history-search-forward
