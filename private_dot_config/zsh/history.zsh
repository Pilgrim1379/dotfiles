# History
# Always set these first, so history is preserved, no matter what happens.
#
# Tell zsh where to store history.
# $VENDOR and $OSTYPE let us check what kind of machine we're on.
local icloud=~/Library/Mobile\ Documents/com\~apple\~CloudDocs
if [[ -d $icloud && $VENDOR == apple ]]; then
  # If using iCloud on macOS, store it there, so it syncs across multiple Macs.
  HISTFILE=$icloud/zsh_history

else
  # := assigns the variable and then substitutes the expression with its value.
  HISTFILE=${XDG_DATA_HOME:=~/.local/share}/zsh/history
fi

# Just in case: If the parent directory doesn't exist, create it.
[[ -d $HISTFILE:h ]] || mkdir -p "$HISTFILE":h

# Max number of entries to keep in history file.
SAVEHIST=$(( 100 * 1000 ))      # Use multiplication for readability.
# Max number of history entries to keep in memory.
HISTSIZE=$(( 1.2 * SAVEHIST ))  # Zsh recommended value
HISTORY_IGNORE="(ls|pwd|exit|..|...)*"
HIST_STAMPS="dd-mm-yyyy"
HISTDUP=erase

setopt bang_hist # Treat the '!' character specially during expansion.
setopt hist_fcntl_lock # Use modern file-locking mechanisms, for better safety & performance.
setopt extended_history # Write the history file in the ':start:elapsed;command' format.
setopt appendhistory
setopt sharehistory # Share history between all sessions.
setopt hist_ignore_space
setopt hist_ignore_all_dups
setopt hist_save_no_dups
setopt hist_ignore_dups
setopt hist_find_no_dups
setopt hist_reduce_blanks
setopt hist_no_store # Don't store history commands
# setopt hist_verify # Do not execute immediately upon history expansion.