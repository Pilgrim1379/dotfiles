##
# Plugins
#

# Add the plugins you want to use here.
# For more info on each plugin, visit its repo at github.com/<plugin>
# -a sets the variable's type to array.
local -a plugins=(
    marlonrichert/zsh-autocomplete      # Real-time type-ahead completion
    romkatv/powerlevel10k
    zsh-users/zsh-autosuggestions       # Inline suggestions
    zsh-users/zsh-syntax-highlighting   # Command-line syntax highlighting
)

# The Zsh Autocomplete plugin sends *a lot* of characters to your terminal.
# This is fine locally on modern machines, but if you're working through a slow
# ssh connection, you might want to add a slight delay before the
# autocompletion kicks in:
#   zstyle ':autocomplete:*' min-delay 0.5  # seconds
#
# If your connection is VERY slow, then you might want to disable
# autocompletion completely and use only tab completion instead:
#   zstyle ':autocomplete:*' async no


ZSH_HIGHLIGHT_HIGHLIGHTERS=( main brackets ) # # Command-line syntax highlighting config

# Speed up the first startup by cloning all plugins in parallel.
# This won't clone plugins that we already have.
znap clone $plugins

# Load each plugin, one at a time.
local p=
for p in $plugins; do
  znap source $p
done

# # Colors for files and directory
zstyle ':completion:*:*:*:*:default' list-colors ${(s.:.)LS_COLORS}
# Auto-installed by Brew, but far worse than the one supplied by Zsh
rm -f $HOMEBREW_PREFIX/share/zsh/site-functions/_git{,.zwc}

# `znap install` adds new commands and completions.
znap install zsh-users/zsh-completions

##
# Defer initilization code with lazily loaded functions created by
# `znap function`.
#

# For each of the examples below, the `eval` statement on the right is not
# executed until you try to execute the associated command or try to use
# completion on it.

znap function _pip_completion pip 'eval "$( pip completion --zsh )"'
compctl -K    _pip_completion pip

znap function _python_argcomplete pipx  'eval "$( register-python-argcomplete pipx  )"'
complete -o nospace -o default -o bashdefault \
           -F _python_argcomplete pipx

znap function _pipenv pipenv  'eval "$(_PIPENV_COMPLETE=zsh_source pipenv)"'
compdef       _pipenv pipenv

znap eval zoxide 'zoxide init zsh'

# # PNPM
# if (( $+commands[pnpm] )); then
#   # tabtab source for packages
#   # uninstall by removing these lines
#   [[ -f ~/.config/tabtab/zsh/__tabtab.zsh ]] && . ~/.config/tabtab/zsh/__tabtab.zsh || true
# fi

# Some commands generate output that should be loaded as a function.
# znap fpath _pdm 'pdm completion zsh'
znap fpath _rustup  'rustup  completions zsh'
znap fpath _cargo   'rustup  completions zsh cargo'