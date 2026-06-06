# Only load completion styling in interactive shells
[[ -o interactive ]] || return 0

# Case-insensitive completion
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'

# Completion menu behaviour (use menu selection, but don't auto-cycle)
zstyle ':completion:*' menu no

# fzf-tab previews
# Use a portable directory listing command for previews.
if [[ "$OSTYPE" == darwin* ]]; then
  zstyle ':fzf-tab:complete:cd:*' fzf-preview 'command ls -G -- "$realpath"'
  zstyle ':fzf-tab:complete:__zoxide_z:*' fzf-preview 'command ls -G -- "$realpath"'
else
  zstyle ':fzf-tab:complete:cd:*' fzf-preview 'command ls --color=auto -- "$realpath"'
  zstyle ':fzf-tab:complete:__zoxide_z:*' fzf-preview 'command ls --color=auto -- "$realpath"'
fi

# Better ssh/scp host completion (uses ~/.ssh/config)
zstyle ':completion:*:ssh:argument-1:'       tag-order hosts users
zstyle ':completion:*:scp:argument-rest:'    tag-order hosts files users
zstyle ':completion:*:(ssh|scp|rdp):*:hosts' hosts


# dotnet CLI completions
# .NET 10+ native hybrid completion — significantly faster than the legacy
# dynamic approach. Static CLI grammar is handled directly in the shell;
# dynamic content (e.g. NuGet package IDs) falls back to `dotnet complete`.
# Falls back to the legacy dynamic completion for .NET < 10.
if (( $+commands[dotnet] )); then
  _cache_dir="${XDG_CACHE_HOME:-$HOME/.cache}"
  _dotnet_cache="$_cache_dir/dotnet-completion.zsh"
  _dotnet_ver_cache="$_cache_dir/dotnet-version.cache"

  _dotnet_ver="$(dotnet --version 2>/dev/null)"
  _old_dotnet_ver=""
  [[ -f "$_dotnet_ver_cache" ]] && _old_dotnet_ver="$(<"$_dotnet_ver_cache")"

  if [[ ! -s "$_dotnet_cache" || "$_dotnet_ver" != "$_old_dotnet_ver" ]]; then
    if dotnet completions script zsh &>/dev/null; then
      dotnet completions script zsh >| "$_dotnet_cache"
    else
      # Legacy dynamic completion for .NET < 10
      cat >| "$_dotnet_cache" <<'EOF'
_dotnet_zsh_complete() {
  local completions=("$(dotnet complete "$words")")
  reply=( "${(ps:\n:)completions}" )
}
compctl -K _dotnet_zsh_complete dotnet
EOF
    fi
    print -r -- "$_dotnet_ver" >| "$_dotnet_ver_cache"
  fi

  source "$_dotnet_cache"
fi
