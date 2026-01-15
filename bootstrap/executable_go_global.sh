#!/usr/bin/env zsh

go install honnef.co/go/tools/cmd/staticcheck@latest
go install golang.org/x/tools/cmd/goimports@latest
# go install github.com/fatih/gomodifytags@latest
go install github.com/go-delve/delve/cmd/dlv@latest
# go install github.com/nao1215/jose@latest
go install github.com/nao1215/gup@latest
# Install Fabric directly from the repo
# go install github.com/danielmiessler/fabric@latest