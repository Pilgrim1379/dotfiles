#!/bin/zsh

##
# History settings
#
# Always set these first, so history is preserved, no matter what happens.
#

# Tell zsh where to store history.
# $VENDOR and $OSTYPE let us check what kind of machine we're on.
if [[ $VENDOR == apple ]]; then
  # On macOS, store it in iCloud, so it syncs across multiple Macs.
  HISTFILE=~/Library/Mobile\ Documents/com\~apple\~CloudDocs/zsh_history
else
  HISTFILE=${XDG_DATA_HOME:=~/.local/share}/zsh/history
fi

# Just in case: If the parent directory doesn't exist, create it.
[[ -d $HISTFILE:h ]] || mkdir -p $HISTFILE:h

# Max number of entries to keep in history file.
SAVEHIST=$(( 100 * 1000 ))      # Use multiplication for readability.

# Max number of history entries to keep in memory.
HISTSIZE=$(( 1.2 * SAVEHIST ))  # Zsh recommended value

# Use modern file-locking mechanisms, for better safety & performance.
setopt HIST_FCNTL_LOCK

# Keep only the most recent copy of each duplicate entry in history.
setopt HIST_IGNORE_ALL_DUPS

# Auto-sync history between concurrent sessions.
setopt SHARE_HISTORY

# Expire duplicate entries first when trimming history.
setopt HIST_EXPIRE_DUPS_FIRST    

# Do not enter command lines into the history list if they are duplicates of the previous event. 
setopt HIST_IGNORE_DUPS

# Remove command lines from the history list when the first character on the line is a space, 
# or when one of the expanded aliases contains a leading space.
setopt HIST_IGNORE_SPACE

# When writing out the history file, older commands that duplicate newer ones are omitted. 
setopt HIST_SAVE_NO_DUPS

# Remove superfluous blanks from each command line being added to the history list.
setopt HIST_REDUCE_BLANKS
