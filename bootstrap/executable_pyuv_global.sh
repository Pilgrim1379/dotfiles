#!/usr/bin/env zsh
set -euo pipefail

# -------- helpers --------
log() { print -P "%F{cyan}==>%f $*"; }
err() { print -P "%F{red}ERROR:%f $*" >&2; }

require_cmd() {
  command -v "$1" >/dev/null 2>&1 || { err "Missing command: $1"; exit 127; }
}

# -------- preflight --------
require_cmd uv
require_cmd python

log "Using python: $(command -v python)"
log "Python version: $(python -V)"

# Upgrade pip in the current interpreter's site-packages
log "Upgrading pip (system site-packages for current interpreter)"
uv pip install -U --system pip

# ---------------------------
# SYSTEM PACKAGES (current python only)
# Keep this list small and purposeful.
# ---------------------------
SYSTEM_PKGS=(
  wheel
  pipdeptree
  debugpy
  pynvim
  bpyutils
  "camoufox[geoip]"
)

# NOTE: configparser is stdlib on Python 3.x; omit it.

log "Installing/upgrading system packages: ${SYSTEM_PKGS[*]}"
uv pip install -U --system "${SYSTEM_PKGS[@]}"

# ---------------------------
# UV TOOLS (isolated environments)
# Prefer these for CLI tools: black/ruff/mypy/ipython/jupyterlab/etc.
# ---------------------------

# Optional: reset tools only when requested
RESET_TOOLS="${RESET_TOOLS:-0}"
if [[ "$RESET_TOOLS" == "1" ]]; then
  log "RESET_TOOLS=1 set; uninstalling all uv tools"
  uv tool uninstall --all
fi

# Install/upgrade IPython as a tool (single shared copy).
# Use the extra that provides Pygments styles for Catppuccin.
log "Installing/upgrading tool: ipython (+ catppuccin pygments styles)"
uv tool install --upgrade --with "catppuccin[pygments]" ipython

# JupyterLab tool with your extras
log "Installing/upgrading tool: jupyterlab (+ extensions)"
uv tool install --upgrade \
  --with jupyterlab-lsp \
  --with jupyterlab-git \
  --with jupyterlab_code_formatter \
  --with jupyterlab_templates \
  --with ipywidgets \
  --with catppuccin-jupyterlab \
  --with catppuccin-matplotlib \
  --with python-lsp-server \
  --with python-lsp-ruff \
  --with ruff \
  jupyterlab

# Other tools (no need to reinstall IPython here)
TOOLS=(
  cmakelang
  cmake-language-server
  pyinstaller
  sphinx
  mypy
  ruff
  codespell
  black
  neovim-remote
  pytest
  nose
  jill
  xxh-xxh
  asciinema
)

log "Installing/upgrading tools: ${TOOLS[*]}"
for t in "${TOOLS[@]}"; do
  uv tool install --upgrade "$t"
done

log "Done."
log "Tool list:"
uv tool list