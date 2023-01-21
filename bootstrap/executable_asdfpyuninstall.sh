#!/bin/zsh

pipx uninstall-all
if [ $? -eq 0 ]; then
  pip uninstall pipx -y && asdf uninstall $1 $2
else
  echo -e "\npipx could not uninstall all packages as requested. Please check $HOME/.local/bin"
fi
