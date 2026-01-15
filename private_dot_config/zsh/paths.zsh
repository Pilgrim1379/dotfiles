[[ -o interactive ]] || return 0

path=(
    $HOME/.nimble/bin(N) # Nim
    $XDG_DATA_HOME/pnpm(N) # PNPM
    $HOME/.cargo/bin(N) # Rust
    $HOME/.juliaup/bin(N) # Julia
    ${GOPATH:-$HOME/go}/bin(N) # Golang
    ${GOROOT:-}/bin(N)         # Golang (only included if GOROOT set)
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

# -- Reset MANPATH to avoid errors from broken paths --
unset MANPATH

_cache_dir="${XDG_CACHE_HOME:-$HOME/.cache}"
_base_cache="$_cache_dir/manpath.base"         # Cached output of `manpath`
_sig_cache="$_cache_dir/manpath.base.sig"      # Signature of inputs that affect `manpath`

# Build a lightweight signature of things that commonly affect `manpath`
# (PATH and key config locations). If these change, rebuild the base MANPATH.
_sig="${PATH}
${MANPATH_MAPFILES-}
"

_old_sig=""
[[ -f "$_sig_cache" ]] && _old_sig="$(<"$_sig_cache")"

if [[ ! -s "$_base_cache" || "$_sig" != "$_old_sig" ]]; then
  # Compute the default/base MANPATH (this is the expensive call)
  manpath 2>/dev/null >| "$_base_cache" || : >| "$_base_cache"
  print -r -- "$_sig" >| "$_sig_cache"
fi

# Base MANPATH from cache
export MANPATH="$(<"$_base_cache")"

# ---- Your existing "prepend extra man dirs" logic (keep as-is) ----

# Zinit plugin manpages (direnv)
if [[ -d "$HOME/.local/share/zinit/plugins/direnv---direnv/man" ]]; then
  MANPATH="$HOME/.local/share/zinit/plugins/direnv---direnv/man:$MANPATH"
fi

# Homebrew / other extras
for extra in \
  "/opt/homebrew/opt/ncurses/share/man" \
  "/opt/homebrew/opt/coreutils/libexec/gnubin/man" \
  "/opt/homebrew/opt/findutils/libexec/gnubin/man" \
  "/opt/homebrew/opt/grep/libexec/gnubin/man" \
  "/opt/homebrew/opt/curl/share/man" \
  "/opt/homebrew/opt/gettext/share/man"; do
  [[ -d "$extra" ]] && MANPATH="$extra:$MANPATH"
done

# -- Rust --
if command -v rustc >/dev/null 2>&1; then
  _cache_dir="${XDG_CACHE_HOME:-$HOME/.cache}"
  _rust_cache="$_cache_dir/rustc-sysroot.cache"

  if [[ ! -s "$_rust_cache" ]]; then
    rustc --print sysroot >| "$_rust_cache" 2>/dev/null
  fi

  _sysroot="$(<"$_rust_cache")"
  if [[ -n "$_sysroot" ]]; then
    export RUST_SRC_PATH="$_sysroot/lib/rustlib/src/rust/library"
  fi
fi


# ########################################################################################################
# ## This should come after all essential PATHs has been set unless you know what you're doing          ##
# ########################################################################################################
# if [ -e ~/.local/bin/mise ]; then
#     # export MISE_COLOR=1
#     export MISE_PIPX_UVX=true
#     # export MISE_PYTHON_UV_VENV_AUTO=true
#     # export MISE_DATA_DIR=$XDG_DATA_HOME/mise
#     export MISE_EXPERIMENTAL=yes
#     # export MISE_PYTHON_COMPILE=1
#     # export MISE_ASDF_COMPAT=1
#     # export MISE_DEBUG=1
#     # export MISE_LOG_LEVEL=debug
#     # export RUST_BACKTRACE=1

#     # eval "$($HOME/.local/bin/mise activate zsh)" #
#     eval "$($HOME/.local/bin/mise activate zsh --shims)" # Use shims instead
# fi
# ########################################################################################################

# -------------------------------
# mise (cached activation)
# -------------------------------
# `mise activate zsh --shims` generates shell code dynamically.
# That generation can be non-trivial and runs every shell by default.
#
# Strategy:
# - Generate the activation script once
# - Cache it
# - Regenerate only when mise itself changes version

# ---- mise (Homebrew-managed) ----------------------------------------------

if command -v mise >/dev/null 2>&1; then
  export MISE_PIPX_UVX=true
  export MISE_EXPERIMENTAL=yes

  _cache_dir="${XDG_CACHE_HOME:-$HOME/.cache}"
  _mise_cache="$_cache_dir/mise-activate.zsh"
  _mise_ver_cache="$_cache_dir/mise-version.cache"

  # Resolve mise binary from PATH (Homebrew-safe)
  _mise_bin="$(command -v mise)"

  # Get current mise version
  _mise_ver="$("$_mise_bin" --version 2>/dev/null)"

  # Read previously cached version (if any)
  _old_ver=""
  [[ -f "$_mise_ver_cache" ]] && _old_ver="$(<"$_mise_ver_cache")"

  # Regenerate activation script if needed
  if [[ ! -s "$_mise_cache" || "$_mise_ver" != "$_old_ver" ]]; then
    "$_mise_bin" activate zsh --shims >| "$_mise_cache"
    print -r -- "$_mise_ver" >| "$_mise_ver_cache"
  fi

  # Source cached activation (fast path)
  source "$_mise_cache"
fi