# -U ensures each entry in these is Unique (that is, discards duplicates).
export -U PATH path FPATH fpath MANPATH manpath
export -UT INFOPATH infopath  # -T creates a "tied" pair; see below.

# Homebrew
export HOMEBREW_BAT=1
export HOMEBREW_COLOR=1
export HOMEBREW_AUTO_UPDATE_SECS=604800
export HOMEBREW_NO_ENV_HINTS=1
export HOMEBREW_NO_ANALYTICS=1

# Shellenv
if (( $+commands[brew] )); then
    eval "$(brew shellenv)"
fi

# $PATH and $path (and also $FPATH and $fpath, etc.) are "tied" to each other.
# Modifying one will also modify the other.
# Note that each value in an array is expanded separately. Thus, we can use ~
# for $HOME in each $path entry.

path=(
    $HOME/.nimble/bin(N) # Nim
    $HOME/.cargo/bin(N) # Rust
    $HOME/.juliaup/bin(N) # Julia
    # $HOME/.local/share/solana/install/active_release/bin(N) # Solana
    $HOME/go/bin(N) # Golang
    $HOME/github/language-servers/elixir-ls(N) # Elixir
    /Applications/Sublime\ Text.app/Contents/SharedSupport/bin(N) # Sublime Text
    /Applications/Postgres.app/Contents/Versions/latest/bin(N) # Postgress app
    $HOME/Library/Application\ Support/Coursier/bin(N) # Scala
    $HOME/Library/Android/sdk/{emulator,platform-tools,tools}(N)
    $HOMEBREW_PREFIX/opt/{mariadb@10.3,ncurses,tomcat@9}/bin(N)
    $HOMEBREW_PREFIX/opt/{coreutils/libexec/gnubin,findutils/libexec/gnubin,grep/libexec/gnubin}/bin(N)
    $HOMEBREW_PREFIX/opt/{curl,gettext,sqlite,llvm}/bin(N)
    # /home/linuxbrew/.linuxbrew/bin(N)
    $path
)

# +-------------------+
# | Ocaml |
# +-------------------+
[[ ! -r ~/.opam/opam-init/init.zsh ]] || source ~/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null

# +-------------------+
# | Haskell ghcup-env |
# +-------------------+
[ -f ~/.ghcup/env ] && source ~/.ghcup/env

# Rust
export RUST_SRC_PATH=$(rustc --print sysroot)/lib/rustlib/src/rust/library

########################################################################################################
## This should come after all essential PATHs has been set unless you know what you're doing          ##
########################################################################################################
if (( $+commands[mise] )); then
    export MISE_USE_TOML=1
    # export MISE_DATA_DIR=$XDG_DATA_HOME/mise
    export MISE_EXPERIMENTAL=1
    # export MISE_PYTHON_COMPILE=1
    # export MISE_ASDF_COMPAT=1
    # export MISE_DEBUG=1
    # export MISE_LOG_LEVEL=debug
    # export RUST_BACKTRACE=1
    
    # eval "$(mise activate zsh --shims)" # should be first if using mise shims and disable line with mise hook-env
    eval "$(mise activate zsh)"
    eval "$(mise hook-env -s zsh)" # enable this if not using mise shims for sublime text to work
fi
########################################################################################################
