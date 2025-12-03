#!/bin/env sh
cd "$(dirname "$0")"
mise --quiet --raw x -- haskell-language-server-9.8.4 lsp $@
