# User Example

### Setup

1. Install [ghcup](https://www.haskell.org/ghcup/), a command line tool to manage haskell
2. run `ghcup tui`
3. Install the tools to get it looking like this -- GHC 9.2.5, Cabal 3.6.2, HLS (haskell language server) 1.9.0, though realistically a more recent HLS and GHC should work.  
 ![image](https://user-images.githubusercontent.com/1423526/234316556-d0778114-6c7e-4e4e-8284-838637d163f1.png)

### VSCode
There is a plugin for Haskell, https://github.com/haskell/vscode-haskell, but you'll want `ghcup` installed first.

### Running

1. `cabal build`
2. `cabal exec user-example`

