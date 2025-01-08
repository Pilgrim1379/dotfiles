# Environment variables
export TERM='xterm-256color'

# Workspace
export WORKSPACE=$HOME/workspace

# Prefer GB English and use UTF-8
export LC_ALL=en_GB.UTF-8
export LANG=en_GB.UTF-8 # Not set on macOS.

# LS_COLORS
if (( $+commands[vivid] )); then
  export LS_COLORS=$(vivid generate catppuccin-mocha)
fi

# -U ensures each entry in these is Unique (that is, discards duplicates).
export -U PATH path FPATH fpath MANPATH manpath
export -UT INFOPATH infopath  # -T creates a "tied" pair; see below.

# Homebrew
export HOMEBREW_BAT=yes
export HOMEBREW_COLOR=yes
export HOMEBREW_AUTO_UPDATE_SECS=604800
export HOMEBREW_NO_ENV_HINTS=yes
export HOMEBREW_NO_ANALYTICS=yes

# Micro
export "MICRO_TRUECOLOR=1"

# Set default editor
export EDITOR=lazyvim
export VISUAL="code --wait"
[[ -v SSH_CONNECTION ]] && VISUAL=lazyvim

# Fzf
# Setting fd as the default source for fzf
export FZF_DEFAULT_COMMAND='fd --type f --strip-cwd-prefix --hidden'

export PAGER=${PAGER:-less}
# export MANPAGER='bat -l man'
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
# export READNULLCMD=bat # Causees compile issues for z4h
export LESS='-FiMr -j.5 --incsearch'
export LESSHISTFILE=$XDG_DATA_HOME/less/lesshst
mkdir -pm 0700 "$LESSHISTFILE":h

[[ $VENDOR == apple ]] && MANPAGER="col -bpx | $MANPAGER"
export QUOTING_STYLE=escape # Used by GNU ls

# Golang environment variables
export GOROOT=$(brew --prefix go)/libexec
export GOPATH=$HOME/go

# Erlang Exports
ERL_AFLAGS="+pc unicode"
ERL_AFLAGS="$ERL_AFLAGS -kernel shell_history enabled"
ERL_AFLAGS="$ERL_AFLAGS -kernel shell_history_file_bytes 1024000"
# KERL_CONFIGURE_OPTIONS="--disable-debug --without-javac --with-ssl=$(brew --prefix openssl@1.1)" \

export KERL_CONFIGURE_OPTIONS="--disable-debug --without-javac"
export KERL_BUILD_DOCS=yes
export ERL_AFLAGS

# Python
export PNPM_HOME=$XDG_DATA_HOME/pnpm
export PYTHON_CONFIGURE_OPTS="--enable-framework"

# UV
export INSTALLER_NO_MODIFY_PATH=1
export UV_PYTHON_PREFERENCE="only-system" # [only-managed, system and only-system] Whether to prefer only-managed, managed or system Python installations
# export UV_PYTHON_DOWNLOADS="manual" # Automatic downloads of Python

# Bun / Deno
# export BUN_INSTALL=$HOME/.bun # Bun
export DENO_INSTALL_ROOT=$HOME/.deno # Deno



# Don't put anything below this line
################################################################################