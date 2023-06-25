#!/bin/zsh

# +---------+
# | HISTORY |
# +---------+
# Always set these first, so history is preserved, no matter what happens.
#

# Tell zsh where to store history.
# $VENDOR and $OSTYPE let us check what kind of machine we're on.
local icloud=~/Library/Mobile\ Documents/com\~apple\~CloudDocs
if [[ -d $icloud && $VENDOR == apple ]]; then
  # If using iCloud on macOS, store it there, so it syncs across multiple Macs.
  HISTFILE=$icloud/zsh_history

  # Sometimes (probably due to concurrency issues), when the histfile is kept in
  # iCloud, it is empty when Zsh starts up. However, there should always be a
  # backup file we can copy.

  # Move the largest "$HISTFILE <number>" file to $HISTFILE.
  # \ escapes/quotes the space behind it.
  # (O):  Sort descending.
  # (OL): Sort by size, descending.
  local -a files=( $HISTFILE(|\ <->)(OL) )
  [[ -r $files[1] ]] &&
      mv $files[1] $HISTFILE
else
  # := assigns the variable and then substitutes the expression with its value.
  HISTFILE=${XDG_DATA_HOME:=~/.local/share}/zsh/history
fi

# Just in case: If the parent directory doesn't exist, create it.
[[ -d $HISTFILE:h ]] || mkdir -p $HISTFILE:h

# Max number of entries to keep in history file.
SAVEHIST=$(( 100 * 1000 ))      # Use multiplication for readability.

# Max number of history entries to keep in memory.
HISTSIZE=$(( 1.2 * SAVEHIST ))  # Zsh recommended value

setopt HIST_FCNTL_LOCK           # Use modern file-locking mechanisms, for better safety & performance.
setopt EXTENDED_HISTORY          # Write the history file in the ':start:elapsed;command' format.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire a duplicate event first when trimming history.
setopt HIST_IGNORE_DUPS          # Do not record an event that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete an old recorded event if a new event is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a previously found event.
setopt HIST_IGNORE_SPACE         # Do not record an event starting with a space.
setopt HIST_SAVE_NO_DUPS         # Do not write a duplicate event to the history file.
setopt HIST_VERIFY               # Do not execute immediately upon history expansion.
setopt HIST_REDUCE_BLANKS		 # Remove superfluous blanks from each command line being added to the history list.
