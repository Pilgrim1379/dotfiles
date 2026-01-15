# History (interactive only)
[[ -o interactive ]] || return 0

# Tell zsh where to store history.
icloud="$HOME/Library/Mobile Documents/com~apple~CloudDocs"

if [[ "$VENDOR" == apple && -d "$icloud" ]]; then
  export HISTFILE="$icloud/zsh_history"
else
  export HISTFILE="${XDG_DATA_HOME:-$HOME/.local/share}/zsh/history"
fi

# Ensure parent dir exists
[[ -d ${HISTFILE:h} ]] || mkdir -p -- "${HISTFILE:h}"

# Sizes
SAVEHIST=100000
HISTSIZE=$(( SAVEHIST * 12 / 10 ))

# History behaviour
setopt BANG_HIST
setopt HIST_FCNTL_LOCK
setopt EXTENDED_HISTORY
setopt APPEND_HISTORY
setopt SHARE_HISTORY

setopt HIST_IGNORE_SPACE
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_SAVE_NO_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_NO_STORE

# Optional safety:
# setopt HIST_VERIFY  # don't run history-expanded command immediately
