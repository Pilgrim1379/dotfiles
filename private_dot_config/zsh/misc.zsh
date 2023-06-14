#!/bin/zsh

# Direnv
if (( $+commands[direnv] )); then
  eval "$(direnv hook zsh)"
fi


## Neovim config switcher
function nvims() {
  items=(
    "default"
    "LazyVim"
    )
  config=$(printf "%s\n" "${items[@]}" | fzf --prompt=" Neovim Config 󰄾 " --height=~50% --layout=reverse --border --exit-0)
  if [[ -z $config ]]; then
    echo "Nothing selected"
    return 0
  elif [[ $config == "default" ]]; then
    config=""
  fi
  NVIM_APPNAME=$config nvim $@
}
