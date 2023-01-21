#!/usr/bin/env bash

# Non Cask

# Install command-line tools using Homebrew.

# Make sure we’re using the latest Homebrew.
brew update

# Upgrade any already-installed formulae.
brew upgrade

# Save Homebrew’s installed location.
BREW_PREFIX=$(brew --prefix)

# Install GNU core utilities (those that come with macOS are outdated).
# Don’t forget to add `$(brew --prefix coreutils)/libexec/gnubin` to `$PATH`.
brew install coreutils
ln -s "${BREW_PREFIX}/bin/gsha256sum" "${BREW_PREFIX}/bin/sha256sum"

# Install some other useful utilities like `sponge`.
brew install moreutils
# Install GNU `find`, `locate`, `updatedb`, and `xargs`, `g`-prefixed.
brew install findutils
# Install GNU `sed`, overwriting the built-in `sed`.
brew install gnu-sed --with-default-names
# Install a modern version of Bash.
brew install bash
brew install bash-completion2

brew install zsh

# Switch to using brew-installed zsh as default shell
if ! fgrep -q "${BREW_PREFIX}/bin/zsh" /etc/shells; then
  echo "${BREW_PREFIX}/bin/zsh" | sudo tee -a /etc/shells;
  chsh -s "${BREW_PREFIX}/bin/zsh";
fi;

# Install `wget` with IRI support.
brew install wget --with-iri

# Install GnuPG to enable PGP-signing commits.
brew install gnupg

# Install more recent versions of some macOS tools.
brew install vim --with-override-system-vi
brew install grep
brew install openssh
brew install screen
brew install php
brew install gmp

# Install font tools.
brew tap bramstein/webfonttools
brew install sfnt2woff
brew install sfnt2woff-zopfli
brew install woff2

# Install other useful binaries.
brew install ack
#brew install exiv2
brew install git
brew install git-lfs
brew install gs
brew install imagemagick
brew install lua
brew install p7zip
brew install pigz
brew install pv
brew install rename
brew install rlwrap
brew install ssh-copy-id
brew install tree
brew install vbindiff

# Remove outdated versions from the cellar.
brew cleanup




# Cask
# Install native apps

brew install caskroom/cask/brew-cask
# brew tap caskroom/versions

brew install --cask karabiner-elements
brew install --cask spectacle
brew install --cask dropbox
brew install --cask alfred
brew install --cask anki
brew install --cask atom
brew install --cask visual-studio-code
brew install --cask osxfuse
brew install --cask appcleaner

brew install --cask libreoffice
brew install --cask libreoffice-language-pack

# dev
brew install --cask iterm2
brew install --cask sublime-text
brew install --cask imagealpha
brew install --cask imageoptim

# fun
brew install --cask limechat
# brew cask install miro-video-converter

# browsers
# brew install --cask google-chrome-canary
# brew install --cask firefoxnightly
# brew install --cask webkit-nightly
# brew install --cask chromium
# brew install --cask torbrowser

# less often
brew install --cask screenflow
brew install --cask vlc
# brew cask install utorrent

brew install --cask spotify

brew tap caskroom/fonts
brew install --cask font-jetbrains-mono
brew install --cask font-jetbrains-mono-nerd-font
