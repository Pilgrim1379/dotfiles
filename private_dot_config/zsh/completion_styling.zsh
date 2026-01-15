# Only load completion styling in interactive shells
[[ -o interactive ]] || return 0

# Case-insensitive completion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

# Completion menu behaviour (use menu selection, but don't auto-cycle)
zstyle ':completion:*' menu no

# fzf-tab previews
# Use a portable directory listing command for previews.
if [[ "$OSTYPE" == darwin* ]]; then
  zstyle ':fzf-tab:complete:cd:*' fzf-preview 'command ls -G -- "$realpath"'
  zstyle ':fzf-tab:complete:__zoxide_z:*' fzf-preview 'command ls -G -- "$realpath"'
else
  zstyle ':fzf-tab:complete:cd:*' fzf-preview 'command ls --color=auto -- "$realpath"'
  zstyle ':fzf-tab:complete:__zoxide_z:*' fzf-preview 'command ls --color=auto -- "$realpath"'
fi

# Better ssh/scp host completion (uses ~/.ssh/config)
zstyle ':completion:*:ssh:argument-1:'       tag-order hosts users
zstyle ':completion:*:scp:argument-rest:'    tag-order hosts files users
zstyle ':completion:*:(ssh|scp|rdp):*:hosts' hosts