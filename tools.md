# Haskell tools

## Editor(ish) tooling

Most of these tools have editor support, or are most pleasantly used through
editor automation, although a bunch are actually CLI programs.

> A simple Haskell code prettifier. The goal is not to format all of the code in
> a file, since I find those kind of tools often "get in the way". However,
> manually cleaning up import statements etc. gets tedious very quickly.
-- [*stylish-haskell*](https://github.com/jaspervdj/stylish-haskell)
([vim-stylishask](https://github.com/alx741/vim-stylishask),
[vim-stylish-haskell](https://github.com/nbouscal/vim-stylish-haskell))

> Haskell pretty printer
-- [*hindent*](https://github.com/chrisdone/hindent)
([vim-hindent](https://github.com/octol/vim-hindent))

> Persistent GHC powered background server for FAST Haskell development tools
-- [*hdevtools*](https://github.com/hdevtools/hdevtools)
([vim-hdevtools](https://github.com/bitc/vim-hdevtools))

> HLint is a tool for suggesting possible improvements to Haskell code. These
> suggestions include ideas such as using alternative functions, simplifying
> code and spotting redundancies
-- [*hlint*](https://github.com/ndmitchell/hlint)
([ale](https://github.com/w0rp/ale/blob/master/ale_linters/haskell/hlint.vim), [vim-scripts/hlint](https://github.com/vim-scripts/hlint))

> Complete interactive development program for Haskell
-- [*intero*](https://github.com/chrisdone/intero) ([intero-vim](https://github.com/parsonsmatt/intero-neovim), [intero-vim (vim8.1)](https://github.com/Fyrbll/intero-vim)) 

> ghc-mod provides editors/IDEs with support for Haskell compiler features. It
> supports both Cabal and Stack based projects and integrations exist for Emacs,
> Vim, Atom, IntelliJ and VSCode.
-- [*ghc-mod*](https://github.com/DanielG/ghc-mod)
([ghcmod-vim](https://github.com/eagletmt/ghcmod-vim),
[neco-ghc](https://github.com/eagletmt/neco-ghc))

## Resources

- [Writing Haskell with Vim (Nov 2017)](https://monicalent.com/blog/2017/11/19/haskell-in-vim/)
- [Haskell Development with Neovim (July 2017)](https://blog.jez.io/haskell-development-with-neovim/)
- [Vim and Haskell in 2017 (reddit)](https://www.reddit.com/r/haskell/comments/6nvgla/vim_and_haskell_in_2017/)
- [Stephen Diehl: Vim 2016](http://www.stephendiehl.com/posts/vim_2016.html)
