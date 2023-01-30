# #!/bin/zsh

# ##
# # Environment variables
# #

# # Prefer GB English and use UTF-8
# export LC_ALL=en_GB.UTF-8
# export LANG=en_GB.UTF-8 # Not set on macOS.

# [[ $OSTYPE == linux-gnu ]] &&
#     export LC_COLLATE=C.UTF-8 # Other UTF-8 locales on Linux give weird whitespace sorting.

# export \
#     HOMEBREW_BAT=1 \
#     HOMEBREW_COLOR=1 \
#     HOMEBREW_NO_AUTO_UPDATE=1

# # export PYENV_ROOT=~/.pyenv PYENV_VERSION=3.7.10 # I don't use Pyenv
# export PIPX_BIN_DIR=~/.local/bin

# # Virtualenv
# export WORKON_HOME=$HOME/.virtualenvs

# # Android Sdk
# export ANDROID_SDK_ROOT=$HOME/Library/Android/sdk
# export ANDROID_HOME=$HOME/Library/Android/sdk
# export GRADLE_USER_HOME=$XDG_CONFIG_HOME/gradle

# # Flutter (Dart SDK)
# # export FLUTTER_ROOT=$HOME/sdks/flutter/bin

# # Golang
# export GOPATH=$HOME/go

# # Pipenv default python
# export PIPENV_DEFAULT_PYTHON_VERSION=$HOME/.asdf/shims/python

# # For building Erlang with asdf-vm
# export KERL_CONFIGURE_OPTIONS="--without-javac --with-ssl=$(brew --prefix openssl@1.1)"

# # Required to enable Python to build dynamic library
# # See: https://github.com/danhper/asdf-python/issues/38 (for macos only)
# export PYTHON_CONFIGURE_OPTS="--enable-framework"

# # Vim
# export VIMCONFIG=$HOME/.config/nvim
# export VIMDATA=$HOME/.local/share/nvim

# # Micor
# export "MICRO_TRUECOLOR=1"

# # Set default editor
# export \
#     EDITOR=micro \
#     VISUAL="code --wait"
# [[ -v SSH_CONNECTION ]] &&
#     VISUAL=micro


# export \
#     PAGER=less \
#     MANPAGER='bat -l man' \
#     READNULLCMD=bat \
#     LESS='-FiMr -j.5 --incsearch' \
#     LESSHISTFILE=$XDG_DATA_HOME/less/lesshst
# mkdir -pm 0700 $LESSHISTFILE:h
# [[ $VENDOR == apple ]] &&
#     MANPAGER="col -bpx | $MANPAGER"

# export QUOTING_STYLE=escape # Used by GNU ls

# export PNPM_HOME=$XDG_DATA_HOME/pnpm # $HOME/.local/share

# # export FZF_DEFAULT_OPTS="\
# # --color=bg+:#313244,bg:#1e1e2e,spinner:#f5e0dc,hl:#f38ba8 \
# # --color=fg:#cdd6f4,header:#f38ba8,info:#cba6f7,pointer:#f5e0dc \
# # --color=marker:#f5e0dc,fg+:#cdd6f4,prompt:#cba6f7,hl+:#f38ba8"

# # -U ensures each entry in these is Unique (that is, discards duplicates).
# export -U PATH path FPATH fpath MANPATH manpath
# export -UT INFOPATH infopath  # -T creates a "tied" pair; see below.

# # $PATH and $path (and also $FPATH and $fpath, etc.) are "tied" to each other.
# # Modifying one will also modify the other.
# # Note that each value in an array is expanded separately. Thus, we can use ~
# # for $HOME in each $path entry.

# # Shellenv
# if command -v brew > /dev/null; then
#     eval "$(`which brew` shellenv)"
# fi

# # LS_COLORS Generator
# export LS_COLORS="$(vivid generate ~/github/ls_colors/vivid/themes/catppuccin-mocha.yml)"

# path=(
#     ~/bin(N)
#     ~/.local/bin # this is added by znap much later but I need it earlier
#     $PNPM_HOME(N) # Pnp
#     ~/.nimble/bin(N) # Nim
#     ~/.cargo/bin(N) # Rust
#     ~/go/bin(N) # Golang
#     ~/sdks/flutter/bin(N) # Flutter
#     $lsp_servers/elixirls/elixir-ls(N) # Elixir
#     ~/language-servers/zls(N) # Zig
#     ~/.local/share/solana/install/active_release/bin(N) # Solana
#     /Applications/Sublime\ Text.app/Contents/SharedSupport/bin(N) # Sublime Text
#     /Applications/Postgres.app/Contents/Versions/latest/bin(N) # Postgress app
#     $ANDROID_SDK_ROOT/{emulator,platform-tools,tools}(N)
#     # $HOMEBREW_PREFIX/opt/coreutils/libexec/gnubin(N)
#     # $HOMEBREW_PREFIX/opt/findutils/libexec/gnubin(N)
#     $HOMEBREW_PREFIX/opt/{mariadb@10.3,ncurses,tomcat@9}/bin(N)
#     $HOMEBREW_PREFIX/opt/{coreutils/libexec/gnubin,findutils/libexec/gnubin,grep/libexec/gnubin}/bin(N)
#     $HOMEBREW_PREFIX/opt/{curl,gettext,sqlite,llvm}/bin(N)
#     /home/linuxbrew/.linuxbrew/bin(N)
#     $path
# )

# # Add your functions to your $fpath, so you can autoload them.
# fpath=(
#     $ZDOTDIR/functions(N)
#     $fpath
#     ~/.local/share/zsh/site-functions(N)
# )

# if command -v brew > /dev/null; then
#     # Add dirs containing completion functions to your $fpath and they will be
#     # picked up automatically when the completion is initialized.
#     # Here, we add it to the end of $fpath, so that we use brew's completions
#     # only for those commands that zsh doesn't already know how to complete.
#     fpath+=( $HOMEBREW_PREFIX/share/zsh/site-functions )
# fi

# manpath=(
#     # $HOMEBREW_PREFIX/opt/coreutils/libexec/gnuman(N)
#     # $HOMEBREW_PREFIX/opt/findutils/libexec/gnuman(N)
#     $manpath
# )

# # Haskell ghcup-env
# [ -f ~/.ghcup/env ] && source ~/.ghcup/env

# # Asdf config file location
# export ASDF_CONFIG_FILE=$XDG_CONFIG_HOME/asdf/.asdfrc

# if command -v asdf > /dev/null; then
#     # Manage multiple runtime versions with a single CLI tool
#     # # . $HOME/.asdf/asdf.sh # This option for when asdf is installed via git
#     . $(brew --prefix)/opt/asdf/libexec/asdf.sh # This option for when asdf is installed via homebrew
# fi

# # pdm --pep582
# if command -v pdm > /dev/null; then
#     eval "$(pdm --pep582)"
# fi


# ############THESE MUST APPEAR AFTER PATH AND (ASDF CONFIG)####################
# # Rust
# # export RUST_SRC_PATH="${HOME}/github/rust"
# export RUST_SRC_PATH=$(rustc --print sysroot)/lib/rustlib/src/rust/library

# ##############################

# if [[ $VENDOR == apple ]]; then
#     MANPAGER="col -bpx | $MANPAGER"
#     # export JAVA_HOME=$( /usr/libexec/java_home -v 1.8 )
#     [[ -f ~/.asdf/plugins/java/set-java-home.zsh ]] && . ~/.asdf/plugins/java/set-java-home.zsh # When using asdf-vm, set JAVA_HOME
# fi
# # Don't put anything below this line
