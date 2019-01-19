# Haskell tools

## Editor(ish) tooling

Most of these tools have editor support, or are most pleasantly used through
editor automation, although a bunch are actually CLI programs.

| Tool | Purpose | Description |
|------|---------|-------------|
| [hfmt](https://github.com/danstiner/hfmt) | Code formatter | Integrates hlint, hindent, and stylish-haskell together |
| [stylish-haskell](https://github.com/jaspervdj/stylish-haskell) | Code formatter | Align and sort import statements, remove trailing whitespace, Align case statements, align fields, replace tabs |
| [hindent](https://github.com/chrisdone/hindent) | Code formatter | Complete Haskell code formatter (stylish-haskell isn't as aggressive in changes) |
| [hlint](https://github.com/ndmitchell/hlint) | Linter | Code suggestions and simplification, redundancy detection. |
| [hdevtools](https://github.com/chrisdone/hindent) | Language server | Backend for editor plugins: enables realtime syntax and type checking | 
| [ghc-mod](https://github.com/DanielG/ghc-mod) | Language server | Backend for editor plugins: enables autocompletion (through neocomplete or deoplete), identify type under cursor, linting, syntax checking, basic code refactoring |
| [intero](https://github.com/chrisdone/intero) | Language server | Backend for editor plugins: enables realtime syntax and type checking, identify type under cursor, jump to definition, built in REPL, automatic type annotation | 

### Code formatters

Automatically format your Haskell code.

> hfmt is a tool for formatting Haskell programs. Currently it is simply a gofmt
> style wrapper of the excellent tools hlint, hindent, and stylish-haskell.
-- [*hfmt*](https://github.com/danstiner/hfmt)

> A simple Haskell code prettifier. The goal is not to format all of the code in
> a file, since I find those kind of tools often "get in the way". However,
> manually cleaning up import statements etc. gets tedious very quickly.
-- [*stylish-haskell*](https://github.com/jaspervdj/stylish-haskell)
([vim-stylishask](https://github.com/alx741/vim-stylishask),
[vim-stylish-haskell](https://github.com/nbouscal/vim-stylish-haskell))

> Haskell pretty printer
-- [*hindent*](https://github.com/chrisdone/hindent)
([vim-hindent](https://github.com/octol/vim-hindent))

### Linting

These tools suggest improvements or highlight anti-patterns in your code.

> HLint is a tool for suggesting possible improvements to Haskell code. These
> suggestions include ideas such as using alternative functions, simplifying
> code and spotting redundancies
-- [*hlint*](https://github.com/ndmitchell/hlint)
([ale](https://github.com/w0rp/ale/blob/master/ale_linters/haskell/hlint.vim), [vim-scripts/hlint](https://github.com/vim-scripts/hlint))

Both Intero and ghc-mod also perform linting.

### Language servers

These projects semantically understand Haskell and allow things like
refactoring, automatic type annotation, syntax checking etc.

> Persistent GHC powered background server for FAST Haskell development tools
-- [*hdevtools*](https://github.com/hdevtools/hdevtools)
([ale](https://github.com/w0rp/ale), [syntastic](https://github.com/vim-syntastic/syntastic), [vim-hdevtools](https://github.com/bitc/vim-hdevtools))

> ghc-mod provides editors/IDEs with support for Haskell compiler features. It
> supports both Cabal and Stack based projects and integrations exist for Emacs,
> Vim, Atom, IntelliJ and VSCode.
-- [*ghc-mod*](https://github.com/DanielG/ghc-mod)
([ghcmod-vim](https://github.com/eagletmt/ghcmod-vim),
[neco-ghc](https://github.com/eagletmt/neco-ghc))


> Complete interactive development program for Haskell
-- [*intero*](https://github.com/chrisdone/intero) ([intero-vim](https://github.com/parsonsmatt/intero-neovim), [intero-vim (vim8.1)](https://github.com/Fyrbll/intero-vim)) 

> This project aims to be the universal interface to a growing number of Haskell
> tools, providing a full-featured and easy to query backend for editors and
> IDEs that require Haskell-specific functionality.
-- [*HIE - Haskell IDE Engine*](https://github.com/haskell/haskell-ide-engine) ([LanguageClient-neovim](https://github.com/autozimu/LanguageClient-neovim))

## IDE like features

> HaRe can rename symbols, lift definitions, convert equivalent Haskell
> constructs like ifs and cases and more while preserving program semantics,
> types and correctly handling indentation. Only HaRe can execute identity
> transformation!
-- [HaRe](https://github.com/RefactoringTools/HaRe)
([vim-hare](https://github.com/vmchale/vim-hare))

## Resources

- [Tools in the Haskell ecosystem](https://github.com/haskell/haskell-ide-engine/blob/master/docs/Tools.md)
- [Writing Haskell with Vim (Nov 2017)](https://monicalent.com/blog/2017/11/19/haskell-in-vim/)
- [Haskell Development with Neovim (July 2017)](https://blog.jez.io/haskell-development-with-neovim/)
- [Vim and Haskell in 2017 (reddit)](https://www.reddit.com/r/haskell/comments/6nvgla/vim_and_haskell_in_2017/)
- [Stephen Diehl: Vim 2016](http://www.stephendiehl.com/posts/vim_2016.html)
