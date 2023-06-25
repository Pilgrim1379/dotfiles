#!/bin/zsh

setopt AUTO_PUSHD           # Push the old directory onto the stack on cd.
setopt PUSHD_IGNORE_DUPS    # Do not store duplicates in the stack.
setopt PUSHD_SILENT         # Do not print the directory stack after pushd or popd.

setopt CORRECT              # Spelling correction
setopt CDABLE_VARS          # Change directory to a path stored in a variable.
setopt EXTENDED_GLOB        # Use extended globbing syntax.
setopt GLOB_DOTS            # Use extended globbing syntax.

setopt NO_CLOBBER           # Don't let > overwrite files. To overwrite, use >| instead.
setopt HASH_EXECUTABLES_ONLY # Don't treat non-executable files in your $path as commands.
setopt HIST_FCNTL_LOCK      # With this option locking is done by means of the system’s fcntl cal
setopt HIST_SAVE_NO_DUPS    # When writing out the history file, older commands that duplicate newer ones are omitted. 

# https://zsh.sourceforge.io/Doc/Release/Expansion.html#Recursive-Globbing
setopt GLOB_STAR_SHORT      # Enable ** and *** as shortcuts for **/* and ***/*, respectively.
setopt NUMERIC_GLOB_SORT    # Sort numbers numerically, not lexicographically.
setopt NO_AUTO_PARAM_SLASH  # If a parameter is completed whose content is the name of a directory, don't add a trailing slash. 
