#!/bin/zsh

# export GOPATH=~/work/go

go install github.com/x-motemen/gore/cmd/gore@latest
go install github.com/mdempsky/gocode@latest   # for code completion
go install golang.org/x/tools/cmd/godoc@latest
go install golang.org/x/tools/cmd/goimports@latest
go install golang.org/x/tools/cmd/gorename@latest
go install golang.org/x/tools/cmd/guru@latest
go install github.com/fatih/gomodifytags@latest

# golangci-lint (optional: for flycheck to integrate golangci-lint results) it is recommended to not use go get to install this one, check the documentation.
