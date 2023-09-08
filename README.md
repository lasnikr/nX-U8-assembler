# nX-U8-assembler

[![Hackage](https://img.shields.io/hackage/v/nX-U8-assembler.svg?logo=haskell)](https://hackage.haskell.org/package/nX-U8-assembler)
[![Stackage Lts](http://stackage.org/package/nX-U8-assembler/badge/lts)](http://stackage.org/lts/package/nX-U8-assembler)
[![Stackage Nightly](http://stackage.org/package/nX-U8-assembler/badge/nightly)](http://stackage.org/nightly/package/nX-U8-assembler)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

An assembler written in Haskell for nX-U8/100 Core.

This project is still in development and in its alpha state with really basic functionality. As soon as I implement labels and all basic functionalities, I'll provide binaries for Linux, Windows and macOS (all x86_64), right now you have to build it yourself which is super easy but requires GHC and Stack.

## Ressources for nX-U8/100 assembly
- Assembly Language -> Core Instruction Manual: https://github.com/fxesdev/nXU8100-resources
- Questions -> Discord: https://discord.com/invite/QjGpH6rSQQ
- Casio Calculator related -> Organisation: https://github.com/fxesdev


## Building (all OSs and architectures)
- Install GHC and Stack using [GHCup](https://www.haskell.org/ghcup/)
- run `git clone https://github.com/lasnikr/nX-U8-assembler && cd nX-U8-assembler`
- run `stack run` to execute or `stack install` to have a `nxasm` binary copied to your path