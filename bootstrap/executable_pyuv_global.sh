#!/usr/bin/env zsh

uv pip install -U --system pip
uv tool uninstall --all
if [ $? -eq 0 ]; then
    uv pip install -U --system wheel
    # uv pip install -U --system pip-autoremove
    uv pip install -U --system configparser
    uv pip install -U --system pipdeptree
    uv pip install -U --system debugpy
    uv pip install -U --system pynvim
    uv pip install -U --system bpyutils
    uv pip install -U --system "camoufox[geoip]"

    # uv pip install -U --system jupyterlab && \
    #     uv pip install -U --system jupyterlab jupyterlab-lsp && \
    #     uv pip install -U --system jupyterlab jupyterlab-git && \
    #     uv pip install -U --system jupyterlab jupyterlab_code_formatter && \
    #     uv pip install -U --system jupyterlab jupyterlab_templates && \
    #     uv pip install -U --system jupyterlab ipywidgets && \
    #     uv pip install -U --system jupyterlab catppuccin-jupyterlab && \
    #     uv pip install -U --system jupyterlab catppuccin-matplotlib
    # uv pip install -U --system pipupgrade

    #[##########################################################################
    # pipx install visidata
    # pipx inject visidata psycopg2
    
    # # uv
    # uv tool install --with psycopg2 visidata
    #]##########################################################################
    
    uv tool install --with catppuccin ipython

    uv tool install --with jupyterlab-lsp \
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

    uv tool install cmakelang
    uv tool install pyinstaller
    uv tool install sphinx
    uv tool install mypy
    uv tool install ruff
    uv tool install codespell
    uv tool install black

    uv tool install cmake-language-server
    uv tool install python-lsp-server

    uv tool install neovim-remote
    uv tool install pytest
    uv tool install nose
    
    uv tool install jill
    uv tool install xxh-xxh
    uv tool install asciinema
else
    echo -e "\nUpgrading pip or uninstalling tools failed"
fi
