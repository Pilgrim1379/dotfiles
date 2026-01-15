# =============================================================================
# functions.zsh
# =============================================================================
# Keep non-interactive-safe helpers available everywhere.
# Gate interactive-only functions (fzf/UI/update flows) to interactive shells.

# -------------------------
# Non-interactive safe helpers
# -------------------------

myip() {
  emulate -L zsh
  curl -fsS --max-time 5 whatismyip.akamai.com 2>/dev/null || \
  curl -fsS --max-time 5 https://api.ipify.org 2>/dev/null || \
  print -u2 "myip: failed"
}

mkcd() {
  emulate -L zsh
  local dir="${1:-}"
  [[ -z "$dir" ]] && { print -u2 "mkcd: missing directory name"; return 2; }
  mkdir -p -- "$dir" && cd -- "$dir"
}

activate() {
  emulate -L zsh

  local venv_name="${1:-.venv}"
  local dir="$PWD"
  local act=""

  while [[ "$dir" != "/" ]]; do
    act="$dir/$venv_name/bin/activate"
    if [[ -f "$act" ]]; then
      source "$act"
      return 0
    fi
    dir="${dir:h}"
  done

  print -u2 "activate: not found: $venv_name/bin/activate (searched up from $PWD)"
  return 1
}

ipython() {
  emulate -L zsh

  # 1) Prefer venv-local ipython (correct interpreter, no warning)
  if [[ -n "${VIRTUAL_ENV:-}" && -x "$VIRTUAL_ENV/bin/ipython" ]]; then
    exec "$VIRTUAL_ENV/bin/ipython" "$@"
  fi

  # 2) Prefer uv tool runner (consistent global tool env)
  if command -v uvx >/dev/null 2>&1; then
    uvx ipython "$@"
    return $?
  fi

  # 3) Fallback: try external ipython without recursion
  local ipy
  ipy="$(command -v ipython 2>/dev/null || true)"
  if [[ -n "$ipy" && -x "$ipy" && "$ipy" != "ipython" ]]; then
    "$ipy" "$@"
    return $?
  fi

  print -u2 "ipython: not found (no venv ipython, no uvx, no external ipython)"
  return 127
}

# =============================================================================
# Interactive-only helpers
# =============================================================================
[[ -o interactive ]] || return 0

# -------------------------
# Neovim config switcher (fzf UI)
# -------------------------
nvims() {
  emulate -L zsh

  command -v fzf >/dev/null 2>&1 || { print -u2 "nvims: fzf not found"; return 127; }

  local cfg_root="${XDG_CONFIG_HOME:-$HOME/.config}"
  local -a items found
  local config

  # Always offer default (no NVIM_APPNAME)
  items=(default)

  # Discover configs: directories that contain init.lua or init.vim
  if [[ -d "$cfg_root" ]]; then
    found=(
      "$cfg_root"/*(/N)
    )

    local d name
    for d in "${found[@]}"; do
      name="${d:t}"
      # Heuristic: treat as Neovim config if it has init.lua or init.vim
      if [[ -f "$d/init.lua" || -f "$d/init.vim" ]]; then
        # Skip if name is literally "nvim" (that's the default config, already represented by "default")
        [[ "$name" == "nvim" ]] && continue
        items+=("$name")
      fi
    done
  fi

  # If we only have default, still allow selection without failing
  config=$(
    printf "%s\n" "${items[@]}" |
      fzf --prompt=" Neovim Config 󰄾 " --height=50% --layout=reverse --border --exit-0
  )

  [[ -z "$config" ]] && { print "Nothing selected"; return 0; }

  if [[ "$config" == "default" ]]; then
    NVIM_APPNAME= command nvim "$@"
  else
    NVIM_APPNAME="$config" command nvim "$@"
  fi
}

# -------------------------
# Search through all man pages (fzf UI)
# -------------------------
fman() {
  emulate -L zsh

  command -v fzf >/dev/null 2>&1 || { print -u2 "fman: fzf not found"; return 127; }

  local query="${1:-}"

  man -k . 2>/dev/null |
    fzf -q "$query" --prompt='man> ' --exit-0 \
      --preview $'\
        entry="{}"; \
        name="${entry%% *}"; \
        name="${name%%,*}"; \
        name="${name%%*( )}"; \
        sec="${entry#* (}"; sec="${sec%%)*}"; \
        [[ "$sec" == "$entry" ]] && sec=""; \
        if [[ -n "$sec" ]]; then man "$sec" "$name" 2>/dev/null | col -bx; else man "$name" 2>/dev/null | col -bx; fi' |
    {
      local entry name sec
      read -r entry || return 0

      name="${entry%% *}"
      name="${name%%,*}"
      name="${name%%*( )}"

      sec="${entry#* (}"; sec="${sec%%)*}"

      if [[ -n "$sec" && "$sec" != "$entry" ]]; then
        man "$sec" "$name"
      else
        man "$name"
      fi
    }
}

md() {
  emulate -L zsh
  [[ $# -eq 1 ]] || { print -u2 "md: usage: md <dir>"; return 2; }
  mkdir -p -- "$1" && cd -- "$1"
}
(( $+functions[compdef] )) && compdef _directories md


# -------------------------
# History filtering
# -------------------------

# Commands to ignore in history (extend freely)
typeset -ga HISTORY_IGNORE_CMDS=(
  ls
  pwd
  exit
  clear
  history
  fg
  bg
  git\ status
  ..
  ...
)

zshaddhistory() {
  emulate -L zsh

  # Full command line as entered
  local cmd="${1%%$'\n'*}"

  # Strip leading whitespace
  cmd="${cmd#"${cmd%%[![:space:]]*}"}"

  # Extract first word (command)
  local first="${cmd%% *}"

  # Ignore empty commands
  [[ -z "$first" ]] && return 2

  # Ignore listed commands
  if (( ${HISTORY_IGNORE_CMDS[(Ie)$first]} )); then
    return 2
  fi

  return 0
}


# -------------------------
# App update helpers (moved from aliases)
# -------------------------
updallapps() {
  command -v caffeinate >/dev/null 2>&1 && caffeinate -dimsu true >/dev/null 2>&1 || true

  echo "starting HOMEBREW update ..." &&
  brew update &&
  brew upgrade --formula &&
  brew cu -aqy --no-brew-update --cleanup &&

  echo "\nstarting NPM update ..." &&
  npm -g update &&

  echo "\nstarting Rust update ..." &&
  rustup update &&

  echo "\nstarting Go update ..." &&
  gup update &&

  echo "\nstarting UV update ..." &&
  uv self update &&
  uv tool upgrade --all &&

  echo "\nstarting Ruby update ..." &&
  gem update --system &&
  gem update
}

cleanup() {
  echo "Cleaning homebrew cache ..." && brew cleanup --prune=all &&
  echo "Cleaning npm cache ..." && npm cache clean --force && npm cache verify &&
  echo "Cleaning go cache ..." && go clean -modcache &&
  gem cleanup
}

bunupd()  { echo "starting Bun update ..." && bun update -g }
miseupd() { echo "starting MISE update ..." && mise self-update && mise upgrade }

brewupd() {
  echo "starting HOMEBREW update ..." &&
  brew update &&
  brew upgrade --formula &&
  brew cu -aqy --no-brew-update --cleanup
}

npmupd() {
  echo "starting NPM update ..." &&
  npm -g update &&
  corepack prepare pnpm@latest --activate &&
  echo "\nstarting PNPM update ..." &&
  pnpm update -g --latest
}

gemupd()  { echo "starting Ruby gem update ..." && gem update --system && gem update }
rustupd() { echo "starting Rust update ..." && rustup update }
gupd()    { echo "starting Go update ..." && gup update }

# -------------------------
# Folder jumpers (moved from aliases)
# -------------------------
work() { cd "$HOME/workspace" || return; }
doc()  { cd "$HOME/Documents" || return; }
dow()  { cd "$HOME/Downloads" || return; }

# -------------------------
# Docker helpers (moved from aliases)
# -------------------------
dockrmall() {
  local ids
  ids=($(docker ps -a -q))
  (( ${#ids} )) || { echo "No containers to remove"; return 0; }
  docker rm $ids
}

docknuke() {
  local cids iids
  cids=($(docker ps -a -q))
  iids=($(docker images -q))
  (( ${#cids} )) && docker rm $cids
  (( ${#iids} )) && docker rmi $iids
}

dockstats() {
  local ids
  ids=($(docker ps -q))
  (( ${#ids} )) || { echo "No running containers"; return 0; }
  docker stats $ids
}

# -------------------------
# zinit update wrapper (interactive)
# -------------------------
zup() {
  zinit update --all || return $?

  # Blow away compdump so compinit -C will rebuild it next start
  rm -f "${XDG_CACHE_HOME:-$HOME/.cache}/zcompdump"* 2>/dev/null

  # Blow away Rust cache (if you use the cached sysroot approach)
  rm -f "${XDG_CACHE_HOME:-$HOME/.cache}/rustc-sysroot.cache" 2>/dev/null

  # If you're using the hybrid compinit stamp, also remove it to force a full rebuild:
  # rm -f "${XDG_CACHE_HOME:-$HOME/.cache}/zcompdump.stamp" 2>/dev/null

  exec zsh
}