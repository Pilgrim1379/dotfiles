# http://zsh.sourceforge.net/Doc/Release/Options.html.
unsetopt BEEP
setopt GLOB_DOTS                # no special treatment for file names with a leading dot
# setopt NO_AUTO_MENU
setopt AUTO_MENU                # require an extra TAB press to open the completion menu
setopt AUTO_CD
setopt NOMATCH
setopt MENU_COMPLETE
setopt EXTENDED_GLOB
setopt INTERACTIVE_COMMENTS

# --- Light autocorrect (commands only) ---
# Correct only command names (e.g. gti -> git), not arguments
setopt CORRECT

# Optional: nicer correction prompt
# %R = original, %r = suggested correction
export SPROMPT="Correct %F{yellow}%R%f to %F{green}%r%f? [y/N/a/e]: "