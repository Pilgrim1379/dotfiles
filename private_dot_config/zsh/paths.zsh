
# Shellenv
# if (( $+commands[brew] )); then
#     eval "$(brew shellenv)"
# fi

# $PATH and $path (and also $FPATH and $fpath, etc.) are "tied" to each other.
# Modifying one will also modify the other.
# Note that each value in an array is expanded separately. Thus, we can use ~
# for $HOME in each $path entry.

fpath=(
	~/.zfunc(N)
	$fpath
)

path=(
    $HOME/.nimble/bin(N) # Nim
    $HOME/.cargo/bin(N) # Rust
    $HOME/.juliaup/bin(N) # Julia
    $HOME/go/bin(N) # Golang
    $HOME/github/language-servers/lexical/bin(N) # Lexical
    $HOME/github/language-servers/elixir-ls(N) # Elixir
    $HOME/github/language-servers/next-ls(N) # Nextls
    /Applications/Sublime\ Text.app/Contents/SharedSupport/bin(N) # Sublime Text
    /Applications/Postgres.app/Contents/Versions/latest/bin(N) # Postgress app
    $HOME/Library/Application\ Support/Coursier/bin(N) # Scala
    $HOME/Library/Android/sdk/{emulator,platform-tools,tools}(N) # Android
    $HOMEBREW_PREFIX/opt/{mariadb@10.3,ncurses,tomcat@9}/bin(N)
    $HOMEBREW_PREFIX/opt/{coreutils/libexec/gnubin,findutils/libexec/gnubin,grep/libexec/gnubin}/bin(N)
    $HOMEBREW_PREFIX/opt/{curl,gettext,sqlite,llvm}/bin(N)
    # /home/linuxbrew/.linuxbrew/bin(N)
    $path
)

# +-------------------+
# | Ocaml |
# +-------------------+
# [[ ! -r ~/.opam/opam-init/init.zsh ]] || source ~/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null

# +-------------------+
# | Haskell ghcup-env |
# +-------------------+
# [ -f ~/.ghcup/env ] && source ~/.ghcup/env

# Bun path
# path=(
#     $HOME/.bun/bin(N)
#     $path
# )

########################################################################################################
## This should come after all essential PATHs has been set unless you know what you're doing          ##
########################################################################################################
if [ -e ~/.local/bin/mise ]; then
    export MISE_USE_TOML=1
    # export MISE_DATA_DIR=$XDG_DATA_HOME/mise
    export MISE_EXPERIMENTAL=1
    # export MISE_PYTHON_COMPILE=1
    # export MISE_ASDF_COMPAT=1
    # export MISE_DEBUG=1
    # export MISE_LOG_LEVEL=debug
    # export RUST_BACKTRACE=1

    # eval "$(mise activate zsh --shims)" # should be first if using mise shims and disable line with mise hook-env
    eval "$($HOME/.local/bin/mise activate zsh --shims)"
    # eval "$($HOME/.local/bin/mise hook-env -s zsh)" # enable this if not using mise shims for sublime text to work
fi
########################################################################################################
