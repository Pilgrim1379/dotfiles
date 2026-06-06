# http://zsh.sourceforge.net/Doc/Release/Options.html.
unsetopt BEEP
setopt GLOB_DOTS                # no special treatment for file names with a leading dot
setopt AUTO_CD
setopt NOMATCH
setopt MENU_COMPLETE            # insert first match immediately on tab
setopt EXTENDED_GLOB
setopt INTERACTIVE_COMMENTS

# NOTE: AUTO_MENU removed — it conflicts with MENU_COMPLETE (MENU_COMPLETE wins),
# and fzf-tab overrides the menu anyway via `zstyle ':completion:*' menu no`.

# --- Light autocorrect (commands only) ---
# Correct only command names (e.g. gti -> git), not arguments
setopt CORRECT

# Optional: nicer correction prompt
# %R = original, %r = suggested correction
export SPROMPT="Correct %F{yellow}%R%f to %F{green}%r%f? [y/N/a/e]: "
