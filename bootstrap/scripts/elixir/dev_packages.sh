#!/bin/zsh
#
mix local.hex
mix archive.install hex phx_new
mix archive.install hex credo
mix archive.install hex bunt
mix archive.install hex jason