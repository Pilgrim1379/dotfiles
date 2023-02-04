#!/bin/zsh
##
# Key bindings
#

##
# NOTE: All configuration below should come AFTER sourcing zsh-autocomplete!
#

# Emacs key map
bindkey -e

# Vim key map
# bindkey -v

# Enable the use of Ctrl-Q and Ctrl-S for keyboard shortcuts.
unsetopt FLOW_CONTROL

# Alt-H: Get help on your current command.
() {
  unalias $1 2> /dev/null   # Remove the default.

  # Load the more advanced version.
  # -R resolves the function immediately, so we can access the source dir.
  autoload -UzR $1

  # Load the hash table that maps each function to its source file.
  zmodload -F zsh/parameter p:functions_source

  # Lazy-load all the run-help-* helper functions from the same dir.
  autoload -Uz $functions_source[$1]-*~*.zwc  # Exclude .zwc files.
} run-help

# Alt-Q
# - On the main prompt: Push aside your current command line  so you can type a
#   new one. The old command line is restored when you press Alt-G or once
#   you've accepted the new command line.
# - On the continuation prompt: Return to the main prompt.
bindkey '^[q' push-line-or-edit

# Alt-V: Show the next key combo's terminal code and state what it does.
bindkey '^[v' describe-key-briefly

# Alt-Shift-S: Prefix the current or previous command line with `sudo`.
() {
  bindkey '^[S' $1  # Bind Alt-Shift-S to the widget below.
  zle -N $1         # Create a widget that calls the function below.
  $1() {            # Create the function.
    [[ -z $BUFFER ]] && zle .up-history
    LBUFFER="sudo $LBUFFER"   # Use $LBUFFER to preserve cursor position.
  }
} .sudo

# Up arrow:
bindkey '\e[A' up-line-or-search
bindkey '\eOA' up-line-or-search
# up-line-or-search:  Open history menu.
# up-line-or-history: Cycle to previous history line.

# Down arrow:
bindkey '\e[B' down-line-or-select
bindkey '\eOB' down-line-or-select
# down-line-or-select:  Open completion menu.
# down-line-or-history: Cycle to next history line.

# Control-Space:
bindkey '\0' list-expand
# list-expand:      Reveal hidden completions.
# set-mark-command: Activate text selection.

# Uncomment the following lines to disable live history search:
# zle -A {.,}history-incremental-search-forward
# zle -A {.,}history-incremental-search-backward

# Return key in completion menu & history menu:
bindkey -M menuselect '\r' .accept-line # disable this when not using zsh-autocomplete plugin
# .accept-line: Accept command line.
# accept-line:  Accept selection and exit menu.
