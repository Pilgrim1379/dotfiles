#!/bin/zsh

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
    pip install -U pipx

    # when using asdf, first reshim python to ensure pipx is available for subsequent commands to work
    # if asdf reshim python; then
        # first uninstall previous packages
        if pipx uninstall-all; then
            pipx install pdm
            # pipx install virtualenv
            # pipx install pipenv
            # pipx install --pip-args='--pre' pipenv
            
            pipx install ipython # cli for python
            # pipx install bpython # cli for python
            # pipx install ptpython # cli for python
            # pipx install pgcli # cli for psql
            # pipx install litecli # cli for sqlite

            pipx install pyinstaller
            pipx install sphinx
            pipx install mypy
            # pipx install isort
            # pipx install black
            # pipx install flake8
            pipx install ruff
            pipx install codespell

            pipx install jedi-language-server
            pipx install cmake-language-server
            pipx install ruff-lsp

            pipx install neovim-remote
            pipx install pytest
            # pipx install vex
            pipx install nose
            pipx install pycodestyle
            pipx install pyflakes
            
            # pipx install jupyterlab && \
            # pipx inject jupyterlab catppuccin-jupyterlab && \
            # pipx inject jupyterlab ipywidgets && \
            # pipx inject jupyterlab jupyter-dash && \
            # pipx inject jupyterlab-code-formatter
            
            pipx install jill
            pipx install xxh-xxh
            pipx install asciinema
            # pipx install streamlit
            pipx install kaggle
        else
  		    echo -e "\npipx uninstall-all failed or didn't excute"
  	    fi
    # else
  	#     echo -e "\npipx installation failed therefore default python packages not installed"
    # fi
else
    echo -e "\nUpgrading pip failed"
fi
