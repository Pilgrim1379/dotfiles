#!/bin/zsh
##
# Environment variables
#

# Prefer GB English and use UTF-8
export LC_ALL=en_GB.UTF-8
export LANG=en_GB.UTF-8 # Not set on macOS.

[[ $OSTYPE == linux-gnu ]] &&
    export LC_COLLATE=C.UTF-8 # Other UTF-8 locales on Linux give weird whitespace sorting.

export \
    HOMEBREW_BAT=1 \
    HOMEBREW_COLOR=1 \
    HOMEBREW_NO_AUTO_UPDATE=1

export BAT_THEME="Catppuccin-mocha"

# export PYENV_ROOT=~/.pyenv PYENV_VERSION=3.7.10 # I don't use Pyenv
export PIPX_BIN_DIR=~/.local/bin

# Virtualenv
export WORKON_HOME=$HOME/.virtualenvs

# Android Sdk
export ANDROID_SDK_ROOT=$HOME/Library/Android/sdk
export ANDROID_HOME=$HOME/Library/Android/sdk
export GRADLE_USER_HOME=$XDG_CONFIG_HOME/gradle

# Golang
export GOPATH=$HOME/go

# For building Erlang
export KERL_CONFIGURE_OPTIONS="--disable-debug --without-javac --with-ssl=$(brew --prefix openssl@1.1)"
export KERL_BUILD_DOCS=yes

# Required to enable Python to build dynamic library
# See: https://github.com/danhper/asdf-python/issues/38 (for macos only)
export PYTHON_CONFIGURE_OPTS="--enable-framework"

# Vim
export VIMCONFIG=$HOME/.config/nvim
export VIMDATA=$HOME/.local/share/nvim

# Micro
export "MICRO_TRUECOLOR=1"

# Set default editor
export \
    EDITOR=micro \
    VISUAL="code --wait"
[[ -v SSH_CONNECTION ]] &&
    VISUAL=micro

# Fzf
# Setting fd as the default source for fzf
export FZF_DEFAULT_COMMAND='fd --type f --strip-cwd-prefix --hidden'
# export FZF_DEFAULT_OPTS='--no-height --color=bg+:#343d46,gutter:-1,pointer:#ff3c3c,info:#0dbc79,hl:#0dbc79,hl+:#23d18b'

export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_OPTS="--preview 'bat --color=always --line-range :50 {}'"

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

export PNPM_HOME=$XDG_DATA_HOME/pnpm # $HOME/.local/share

# -U ensures each entry in these is Unique (that is, discards duplicates).
# export -U PATH path FPATH fpath MANPATH manpath
# export -UT INFOPATH infopath  # -T creates a "tied" pair; see below.

# $PATH and $path (and also $FPATH and $fpath, etc.) are "tied" to each other.
# Modifying one will also modify the other.
# Note that each value in an array is expanded separately. Thus, we can use ~
# for $HOME in each $path entry.

# LS_COLORS Generator
export LS_COLORS="$(vivid generate ~/github/ls_colors/vivid/themes/catppuccin-mocha.yml)"

# Add your functions to your $fpath, so you can autoload them.
fpath=(
    $ZDOTDIR/functions(N)
    $fpath
    ~/.local/share/zsh/site-functions(N)
)

if (( $+commands[brew] )); then
    # if command -v brew > /dev/null; then
    # `znap eval <name> '<command>'` is like `eval "$( <command> )"` but with
    # caching and compilation of <command>'s output, making it 10 times faster.
    # znap eval brew-shellenv 'brew shellenv'

    # Add dirs containing completion functions to your $fpath and they will be
    # picked up automatically when the completion is initialized.
    # Here, we add it to the end of $fpath, so that we use brew's completions
    # only for those commands that zsh doesn't already know how to complete.
    fpath+=( $HOMEBREW_PREFIX/share/zsh/site-functions )
fi

manpath=(
    # $HOMEBREW_PREFIX/opt/coreutils/libexec/gnuman(N)
    # $HOMEBREW_PREFIX/opt/findutils/libexec/gnuman(N)
    $manpath
)

# Haskell ghcup-env
[ -f ~/.ghcup/env ] && source ~/.ghcup/env

################################################################################
## This should come after PATH has been set unless you know what you're doing ##
################################################################################
# # Asdf config
# export ASDF_CONFIG_FILE=$XDG_CONFIG_HOME/asdf/.asdfrc

# if (( $+commands[asdf] )); then
#     # Manage multiple runtime versions with a single CLI tool
#     # . $HOME/.asdf/asdf.sh # This option for when asdf is installed via git
#     . $(brew --prefix)/opt/asdf/libexec/asdf.sh # This option for when asdf is installed via homebrew
# fi

## RTX config
if (( $+commands[rtx] )); then
    eval "$(rtx activate zsh)"
fi
################################################################################

# pdm --pep582
if (( $+commands[pdm] )); then
    eval "$(pdm --pep582)"
fi


############THESE MUST APPEAR AFTER PATH AND (ASDF/RTX CONFIG)####################
# Pipenv default python
export PIPENV_DEFAULT_PYTHON_VERSION="$(which python)"

# Rust
# export RUST_SRC_PATH="${HOME}/github/rust"
export RUST_SRC_PATH=$(rustc --print sysroot)/lib/rustlib/src/rust/library

##############################

# if [[ $VENDOR == apple ]]; then
#     # export JAVA_HOME=$( /usr/libexec/java_home -v 1.8 )
#     [[ -f ~/.asdf/plugins/java/set-java-home.zsh ]] && . ~/.asdf/plugins/java/set-java-home.zsh # When using asdf-vm, set JAVA_HOME
# fi
# Don't put anything below this line
