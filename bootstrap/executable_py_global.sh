#!/usr/bin/env zsh

pip install -U pip
if [ $? -eq 0 ]; then
    pip install -U wheel
    pip install -U pip-autoremove
    pip install -U configparser
    pip install -U pipdeptree
    pip install -U debugpy
    pip install -U pynvim
    pip install -U bpyutils
    pip install -U pipupgrade
    # pip install -U pipx

        # first uninstall previous packages
        if pipx uninstall-all; then
            # pipx install pdm
            # pipx install virtualenv
            # pipx install pipenv
            # pipx install --pip-args='--pre' pipenv
            
            # pipx install ipython # cli for python
            # pipx install bpython # cli for python
            # pipx install ptpython # cli for python
            # pipx install pgcli # cli for psql
            # pipx install litecli # cli for sqlite

            pipx install cmakelang
            pipx install pyinstaller
            pipx install sphinx
            pipx install mypy
            # pipx install isort
            # pipx install black
            # pipx install flake8
            pipx install ruff
            pipx install codespell

            pipx install cmake-language-server
            pipx install ruff-lsp
            # pipx install jedi-language-server
            pipx install python-lsp-server && \
                pipx inject python-lsp-server python-lsp-ruff

            pipx install neovim-remote
            pipx install pytest
            # pipx install vex
            pipx install nose
            pipx install pycodestyle
            pipx install pyflakes

            pipx install jupyterlab --include-deps && \
                pipx inject jupyterlab jupyterlab-lsp && \
                pipx inject jupyterlab jupyterlab-git && \
                pipx inject jupyterlab jupyterlab_code_formatter && \
                pipx inject jupyterlab jupyterlab_templates && \
                pipx inject jupyterlab ipywidgets && \
                pipx inject jupyterlab catppuccin-jupyterlab && \
                pipx inject jupyterlab catppuccin-matplotlib && \
                pipx inject jupyterlab 'catppuccin[pygments]'
            
            pipx install jill
            pipx install xxh-xxh
            pipx install asciinema
            # pipx install streamlit
            pipx install kaggle
        else
  		    echo -e "\npipx uninstall-all failed or didn't excute"
  	    fi
    # mise
    if (( $+commands[mise] )); then
        mise reshim
    fi
else
    echo -e "\nUpgrading pip failed"
fi
