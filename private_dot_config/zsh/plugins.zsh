# Only load plugins in interactive shells
[[ -o interactive ]] || return 0

# Powerlevel10k
zinit ice depth=1
zinit light romkatv/powerlevel10k

# zsh plugins
zinit light zsh-users/zsh-syntax-highlighting
zinit light zsh-users/zsh-completions
zinit light zsh-users/zsh-autosuggestions
zinit light Aloxaf/fzf-tab

# snippets
zinit snippet OMZP::command-not-found

# Direnv shell integration (moved here from paths.zsh)
zinit as"program" make'!' atclone'./direnv hook zsh > zhook.zsh' \
  atpull'%atclone' pick"direnv" src"zhook.zsh" for \
  direnv/direnv