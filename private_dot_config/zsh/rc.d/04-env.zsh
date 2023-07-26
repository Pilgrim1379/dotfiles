#!/bin/zsh

# Environment variables
#
path=(
    $HOME/.bin(N)
    # $HOME/.local/share/rtx/shims(N)
    # $HOME/.local/bin(N)
    $path
)
# Add your functions to your $fpath, so you can autoload them.
fpath=(
  $ZDOTDIR/functions(N)
  $XDG_DATA_HOME/zsh/site-functions(N)
  $HOMEBREW_PREFIX/share/zsh/site-functions(N)
  $fpath
)

# if command -v brew > /dev/null; then
#   # Add dirs containing completion functions to your $fpath and they will be
#   # picked up automatically when the completion system is initialized.
#   # Here, we add it to the end of $fpath, so that we use brew's completions
#   # only for those commands that zsh doesn't already know how to complete.
#   fpath+=(
#     $HOMEBREW_PREFIX/share/zsh/site-functions
#   )
# fi

# Prefer GB English and use UTF-8
export LC_ALL=en_GB.UTF-8
export LANG=en_GB.UTF-8 # Not set on macOS.

[[ $OSTYPE == linux-gnu ]] &&
  export LC_COLLATE=C.UTF-8 # Other UTF-8 locales on Linux give weird whitespace sorting.

export TERM='xterm-256color'
# LS_COLORS Generator
export LS_COLORS=$(vivid generate ~/github/ls_colors/vivid/themes/catppuccin-mocha.yml)

# Workspace
export WORKSPACE=$HOME/workspace

# Homebrew
export \
  HOMEBREW_BAT=1 \
  HOMEBREW_COLOR=1 \
  HOMEBREW_AUTO_UPDATE_SECS=604800 \
  HOMEBREW_NO_ENV_HINTS=1

export BAT_THEME="Catppuccin-mocha"

# Android Exports
# export \
#   ANDROID_SDK_ROOT=$HOME/Library/Android/sdk \
#   ANDROID_HOME=$HOME/Library/Android/sdk \
#   GRADLE_USER_HOME=$XDG_CONFIG_HOME/gradle

# Golang Exports
export GOPATH=$HOME/go

# Erlang Exports
ERL_AFLAGS="+pc unicode"
ERL_AFLAGS="$ERL_AFLAGS -kernel shell_history enabled"
ERL_AFLAGS="$ERL_AFLAGS -kernel shell_history_file_bytes 1024000"
# erlang 25.1 and newer support openssl@3
# https://github.com/asdf-vm/asdf-erlang#osx
# KERL_CONFIGURE_OPTIONS="--disable-debug --without-javac --with-ssl=$(brew --prefix openssl@1.1)" \
export \
  KERL_CONFIGURE_OPTIONS="--disable-debug --without-javac" \
  KERL_BUILD_DOCS=yes \
  ERL_AFLAGS

# Vim Exports
export \
  VIMCONFIG=$HOME/.config/nvim \
  VIMDATA=$HOME/.local/share/nvim

# Micro
export "MICRO_TRUECOLOR=1"

# Set default editor
export \
  EDITOR=lazyvim \
  VISUAL="code --wait"
[[ -v SSH_CONNECTION ]] &&
  VISUAL=lazyvim

# Fzf
# Setting fd as the default source for fzf
export \
  FZF_DEFAULT_COMMAND='fd --type f --strip-cwd-prefix --hidden' \
  FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_OPTS="--preview 'bat \
  --color=always \
  --line-range :50 {}' \
  --prompt '∷ ' \
  --pointer ▶ \
  --marker ⇒"

export \
  PAGER=less \
  MANPAGER='bat -l man' \
  READNULLCMD=bat \
  LESS='-FiMr -j.5 --incsearch' \
  LESSHISTFILE=$XDG_DATA_HOME/less/lesshst
mkdir -pm 0700 $LESSHISTFILE:h

[[ $VENDOR == apple ]] &&
    MANPAGER="col -bpx | $MANPAGER"

export QUOTING_STYLE=escape # Used by GNU ls

############THESE MUST APPEAR AFTER PATH AND (ASDF/RTX CONFIG)####################
# Python Exports
# Pipenv default python
# export PIPENV_DEFAULT_PYTHON_VERSION=$(command -v python)
export \
  PIPENV_DEFAULT_PYTHON_VERSION=$(command -v python) \
  PIPENV_VENV_IN_PROJECT=1 \
  PNPM_HOME=$XDG_DATA_HOME/pnpm \
  PYTHON_CONFIGURE_OPTS="--enable-framework" \
  PIPX_BIN_DIR=~/.local/bin \
  WORKON_HOME=$HOME/.virtualenvs

# Rust
# export RUST_SRC_PATH="${HOME}/github/rust"
export \
  RUST_SRC_PATH=$(rustc --print sysroot)/lib/rustlib/src/rust/library

##############################

# if [[ $VENDOR == apple ]]; then
#     # export JAVA_HOME=$( /usr/libexec/java_home -v 1.8 )
#     [[ -f ~/.asdf/plugins/java/set-java-home.zsh ]] && . ~/.asdf/plugins/java/set-java-home.zsh # When using asdf-vm, set JAVA_HOME
# fi

# Don't put anything below this line
