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

# Search through all man pages
function fman() {
	man -k . | fzf -q "$1" --prompt='man> ' --preview $'echo {} | tr -d \'()\' | awk \'{printf "%s ", $2} {print $1}\' | xargs -r man' | tr -d '()' | awk '{printf "%s ", $2} {print $1}' | xargs -r man
}

# Improved myip alias. Echoed to avoid strange character at end in ZSH.
function myip() {
	echo "$(curl -s whatismyip.akamai.com)"
}

# Python PDM
# function pdm() {
# 	local command=$1

# 	if [[ "$command" == "shell" ]]; then
# 		eval $(pdm venv activate)
# 	else
# 		command pdm $@
# 	fi
# }

# Make and cd into directory
#  - from: http://alias.sh/make-and-cd-directory
function mkcd() {
	mkdir "$1" && cd "$1" || exit
}

# Activate virtualenvs
function activate(){
	source .venv/bin/activate
}