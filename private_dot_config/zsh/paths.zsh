# $PATH and $path (and also $FPATH and $fpath, etc.) are "tied" to each other.
# Modifying one will also modify the other.
# Note that each value in an array is expanded separately. Thus, we can use ~
# for $HOME in each $path entry.

# Direnv shell integration
zinit as"program" make'!' atclone'./direnv hook zsh > zhook.zsh' \
    atpull'%atclone' pick"direnv" src"zhook.zsh" for \
        direnv/direnv

path=(
    $HOME/.nimble/bin(N) # Nim
    $HOME/.cargo/bin(N) # Rust
    $HOME/.juliaup/bin(N) # Julia
    $GOPATH/bin(N) # Golang
    $GOROOT/bin(N) # Golang
    $HOME/github/language_servers/lexical/bin(N) # Lexical
    $HOME/github/language_servers/elixir-ls(N) # Elixir
    $HOME/github/language_servers/next-ls(N) # Nextls
    /Applications/Sublime\ Text.app/Contents/SharedSupport/bin(N) # Sublime Text
    /Applications/Postgres.app/Contents/Versions/latest/bin(N) # Postgress app
    $HOME/Library/Application\ Support/Coursier/bin(N) # Scala
    $HOME/Library/Android/sdk/{emulator,platform-tools,tools}(N) # Android
    $HOMEBREW_PREFIX/opt/{mariadb@10.3,ncurses,tomcat@9}/bin(N)
    $HOMEBREW_PREFIX/opt/{coreutils/libexec/gnubin,findutils/libexec/gnubin,grep/libexec/gnubin}(N)
    $HOMEBREW_PREFIX/opt/{curl,gettext,sqlite,llvm}/bin(N)
    $path
)

# Rust
export RUST_SRC_PATH=$(rustc --print sysroot)/lib/rustlib/src/rust/library

# +-------------------+
# | Ocaml |
# +-------------------+
# [[ ! -r ~/.opam/opam-init/init.zsh ]] || source ~/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null

# +-------------------+
# | Haskell ghcup-env |
# +-------------------+
# [ -f ~/.ghcup/env ] && source ~/.ghcup/env

# direnv
# eval "$(direnv hook zsh)"

####### / https://github.com/romkatv/zsh4humans/issues/286 ######
# # Remove all subdirectories of /foo/bar from path.
# path=(${path:#/foo/bar/*})

# # Remove ~/.local/bin from path.
# # Note that you have to use $HOME instead of ~ if the tilde
# # isn't the first character of the pattern.
# path=(${path:#~/.local/bin})

# Remove a bunch of directories from path and then append them.
# This is so zsh installed by z4humans doesn't precede homebrew's version
# () {
#   path=(${path:|argv} $@)
# } /Users/nqa/.cache/zsh4humans/v5/zsh4humans/zb

########################################################################################################
## This should come after all essential PATHs has been set unless you know what you're doing          ##
########################################################################################################
if [ -e ~/.local/bin/mise ]; then
    # export MISE_COLOR=1
    export MISE_PIPX_UVX=true
    # export MISE_PYTHON_UV_VENV_AUTO=true
    # export MISE_DATA_DIR=$XDG_DATA_HOME/mise
    export MISE_EXPERIMENTAL=yes
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
