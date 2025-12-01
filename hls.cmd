@echo off
setlocal
cd /d %~dp0
mise --quiet --raw x -- haskell-language-server-wrapper.exe lsp %*
