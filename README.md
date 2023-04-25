# User Example

### Setup

1. Install [ghcup](https://www.haskell.org/ghcup/), a command line tool to manage haskell
2. run `ghcup tui`
3. Install the tools to get it looking like this -- GHC 9.2.5, Cabal 3.6.2, HLS (haskell language server) 1.9.0, though realistically a more recent HLS and GHC should work (note the HLS powered though, definitely choose a GHC version with that).
 ![image](https://user-images.githubusercontent.com/1423526/234316556-d0778114-6c7e-4e4e-8284-838637d163f1.png)

### VSCode
There is a plugin for Haskell, https://github.com/haskell/vscode-haskell, but you'll want `ghcup` installed first.

### Running

1. `cabal build`
2. `cabal exec user-example`

### File walkthrough
* `Main` contains the top level element and interpreter
* `Server` is the finer parts of handling web and websocket requests
* `Router` contains the route reducer, receiving an initial path then able to change it via events
* `Pages` contains the pages (just user creation and listing now)
* `Services` contains the user service

### Automated formatted
Provided by https://hackage.haskell.org/package/fourmolu, configuration is in `fourmolu.yaml`

1. `cabal install fourmolu` (this will take awhile, it's a big boy)
2. `fourmolu -i src` to format all files
