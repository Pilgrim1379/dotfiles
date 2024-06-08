# Environment variables
export TERM='xterm-256color'

# Workspace
export WORKSPACE=$HOME/workspace

export BAT_THEME="Catppuccin-mocha"

# Micro
export "MICRO_TRUECOLOR=1"

# Set default editor
export EDITOR=lazyvim
export VISUAL="code --wait"
[[ -v SSH_CONNECTION ]] && VISUAL=lazyvim

# Fzf
# Setting fd as the default source for fzf
export FZF_DEFAULT_COMMAND='fd --type f --strip-cwd-prefix --hidden'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_OPTS="--preview 'bat --line-range :500 {}' --prompt '∷ ' --pointer ▶ --marker ⇒"

export PAGER=less
# export MANPAGER='bat -l man'
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export READNULLCMD=bat
export LESS='-FiMr -j.5 --incsearch'
export LESSHISTFILE=$XDG_DATA_HOME/less/lesshst
mkdir -pm 0700 "$LESSHISTFILE":h

[[ $VENDOR == apple ]] && MANPAGER="col -bpx | $MANPAGER"
export QUOTING_STYLE=escape # Used by GNU ls

# Golang Exports
export GOPATH=$HOME/go

# Android Exports
# export ANDROID_SDK_ROOT=$HOME/Library/Android/sdk
# export ANDROID_HOME=$HOME/Library/Android/sdk
# export GRADLE_USER_HOME=$XDG_CONFIG_HOME/gradle

# Erlang Exports
ERL_AFLAGS="+pc unicode"
ERL_AFLAGS="$ERL_AFLAGS -kernel shell_history enabled"
ERL_AFLAGS="$ERL_AFLAGS -kernel shell_history_file_bytes 1024000"
# erlang 25.1 and newer support openssl@3
# https://github.com/asdf-vm/asdf-erlang#osx
# KERL_CONFIGURE_OPTIONS="--disable-debug --without-javac --with-ssl=$(brew --prefix openssl@1.1)" \

export KERL_CONFIGURE_OPTIONS="--disable-debug --without-javac"
export KERL_BUILD_DOCS=yes 
export ERL_AFLAGS

# Python
# export PIPENV_DEFAULT_PYTHON_VERSION=$(command -v python)
# export PIPENV_VENV_IN_PROJECT=1
export PNPM_HOME=$XDG_DATA_HOME/pnpm
export PYTHON_CONFIGURE_OPTS="--enable-framework"
export PIPX_BIN_DIR=~/.local/bin 
export BUN_INSTALL=$HOME/.bun # Bun
# export WORKON_HOME=$HOME/.virtualenvs

# Don't put anything below this line
