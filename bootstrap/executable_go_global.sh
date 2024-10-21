#!/usr/bin/env zsh

go install honnef.co/go/tools/cmd/staticcheck@latest
go install golang.org/x/tools/cmd/goimports@latest
# go install github.com/fatih/gomodifytags@latest
go install github.com/go-delve/delve/cmd/dlv@latest
brew install nao1215/tap/gup
# Install Fabric directly from the repo
go install github.com/danielmiessler/fabric@latest