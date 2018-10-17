---
title: Haskell Development Workflows
author: Roman Gonzalez @ FP Complete
patat:
  incrementalLists: true

  theme:
    codeBlock: [onRgb#393939, dullWhite]
    code: [onVividBlack, vividMagenta, bold]
    emph: [underline]
    strong: [bold, rgb#f08000]

  pandocExtensions:
    - patat_extensions
    - autolink_bare_uris
    - emoji
---

# Intro

## Goals

- Understanding the importance of fast feedback loops

- Enumerate differences between compiled and interpreted workflows

- Learn about different tools (editor agnostic) for effective workflows

- Learn GHCi basics, useful settings, and capabilities

- Follow through different examples that showcase different workflows

## Disclaimer

- We are assuming you are familiar with the ~stack~ tool

- There are other tools that offer similar capabilities to the ones exposed
  here, these tools are the ones FP Complete developers use regularly

## Feedback loops ↻

- Repetitive work is not challenging, but it is taxing

- The tiny bits of repetition add up as time progresses

- If feedback is fast and automatic, you iterate through many effective changes

- Fast feedback loops must have relevant importance when delevoping software

# Workflows via Compilation

## Stack - Compile your whole project on file change

Stack comes with an out-of-the-box command to compile, test or even generate
documentation for your project on file changes:

```
$ stack build --fast --file-watch # Build binaries
$ stack build --fast --file-watch --tests # Build and Run tests
$ stack build --fast --file-watch --haddock --no-haddock-deps # Build docs
```

## Stack - Run extra commands after compilation

If you want to execute a command after a successful compilation, you may want use
the `--exec` parameter.

For example, if running a web server, I may want to automatically refresh my
Firefox browser everytime a compilation is successful:

```
$ stack build --fast --file-watch --exec './script.sh'
```

## Summary of Compilation

- No chance to experiment with on-the-fly code
- Becomes rather slow in big to huge projects (linking time)
- Saving a file automatically re-compiles code (automated feedback loop)

# Workflows via REPL (GHCi)

## GHCi - Running a REPL with your project sources

To run a REPL:

```
$ stack repl
```

This command will prompt you to pick a target (a library, executable or
test-suite) from all cabal projects inside a stack project.

## GHCi - Navigate inside your project

When on the REPL you can query where you are located at:

```
> :show paths
```

And you can move around using `:cd`

```
> :load src/Main.hs
> :cd src
> :load Main.hs
```

## GHCi - Running compiled Haskell

When you use the `-fobject-code` in your stack repl invocation, GHCi will compile
your sources to a `.o` instead of interpreting the code's byte-code.

```
$ stack repl --ghc-options '-fobject-code'
```

## GHCi - Dealing with undeclared dependencies on the fly

If you try to import the module from a package that is currently installed, but
not part of your cabal target dependency list (e.g. a dependency of a
dependency) you can use the `:set -package` instruction

```
> :m + Data.Map.Strict
<no location info>: error:
    Could not find module ‘Data.Map.Strict’
    It is not a module in the current program, or in any known package.

> :set -package containers
package flags have changed, resetting and loading new packages...

> :m + Data.Map.Strict
```

## GHCi - Dealing with compiler extensions on the fly

If you are using a GHC extension in the code you load, those extensions won't be
automatically included in the GHCi REPL environment, you'll need to specify the
extension in a `:seti` instruction

```
> import qualified Data.Text as Text
> Text.toUpper "hello"

<interactive>:2:14: error:
    • Couldn't match expected type ‘Text.Text’
                  with actual type ‘[Char]’
    • In the first argument of ‘Text.toUpper’, namely ‘"hello"’
      In the expression: Text.toUpper "hello"
      In an equation for ‘it’: it = Text.toUpper "hello"

> :seti -XOverloadedStrings
> Text.toUpper "hello"
"HELLO"
```

## GHCi - Extending output with relevant info

You may use GHCi only options that provide information about how much time GHC
took to interpret a statement and how much memory consumption the statement
produced; using `:set +s +t`

```
> :set +s +t
> 5 + 5
10
it :: Num a => a
(0.01 secs, 386,416 bytes)
```

## GHCi - Customizing your REPL setup

You can have script files located at different paths in your system

* Root of the project (e.g. `project/.ghci`)
* Home directory (e.g. `~/.ghci`)
* Write statements as if you were inside the GHCi REPL

## GHCi - Development Workflow - Reloading from Main

Regularly, you want to load the main module of your application and then invoke
`:reload` for every change in the project, this workflow doesn't scale on large
projects (e.g. `cardano-sl`), especially when changes occur in modules that are
re-usable among many other modules.

## GHCi - Development Workflow - Only loading module being changed

The best course of action in large projects is to load the file in question, (or
the test module) and reload that instead. Consider this situation to Unit vs
Integration reloading.

## GHCi - Development Workflow - Execute test-suite in the REPL

Whenever you are using `stack repl` by default it will load the library target
of your cabal project, you may specify multiple targets at once so that you can
load different files at the same time

```
$ stack ide targets # query what targets are available
$ stack repl <package-name>:lib <package-name>:test:<testsuite-name>
```

## Summary of GHCi workflow

- Can experiment with on-the-fly code
- Can run test-suites in the REPL
- No automatic reload of changes on file save
- Can be pretty slow on large projects (:reload from Main module)
- Large project slowness can be addressed (:reload from targeted module only)

# Workflows via GHCid

## GHCid - a combination of the two approaches

To run ghcid

```
$ ghcid --command 'stack repl'
```

## GHCid - Run tests on file change

```
$ ghcid \
  --command 'stack repl project:lib project:test:project-tests' \
  --test ':main'
```

That's it :man-shrugging:

## Summary of GHCid workflow

- Faster than compiled approach
- No chance to experiment with on-the-fly code
- Becomes rather slow in large projects
- Saving a file automatically re-compiles code (automated feedback loop)

## Thank you :bow:

Questions.

twitter: @romanandreg / @fpcomplete
blog: https://blog.roman-gonzalez.ca
