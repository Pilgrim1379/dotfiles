#!/usr/bin/env zsh

## Install default global package for deno

# Type `git open` to open the GitHub page or website for a repository.
# deno install -g --allow-scripts=npm:fsevents
# deno install -g --allow-scripts=npm:yarn

deno install -g --allow-all npm:git-open

# fancy listing of recent branches
deno install -g --allow-all npm:git-recent

# ghost
# deno install -g --allow-all --allow-scripts npm:ghost-cli@latest

# dev frameworks
# deno install -g --allow-all npm:@quasar/cli
# deno install -g --allow-all npm:cordova
# deno install -g --allow-all npm:framework7-cli

deno install -g --allow-all npm:stylus

# formatter and linters
deno install -g --allow-all npm:eslint
deno install -g --allow-all npm:js-beautify
deno install -g --allow-all npm:stylelint

echo -e "Finished installing DENO packages"

# mise
if (( $+commands[mise] )); then
    mise reshim
fi

# echo -e "\nPackage installation complete - Don't forget to reshim so that new packages can be recognised"
echo -e "\nIf required run 'deno completions zsh > ~/.zfunc/_deno' to get completions working for deno."
echo -e "Installation complete."
