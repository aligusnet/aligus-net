---
title: The Haskell Cabal
date: 2016-06-02
author: Alexander Ignatyev
tags: [haskell]
summary: Cabal is a build and package manager for Haskell. It is considered as slightly outdated in modern Haskell development. Mostly because the new system called Stack has been introduced. But Stack uses Cabal under the hood and understanding of how Cabal works might be useful even if you use Stack for building and packaging.
aliases: [2016-06-02-haskell-cabal.html]
---

Cabal is a build and package manager for Haskell. It is considered as slightly outdated in modern Haskell development. Mostly because the new system called Stack has been introduced. But Stack uses Cabal under the hood and understanding of how Cabal works might be useful even if you use Stack for building and packaging.

## Quick reference

### Sandboxes

```bash
cabal sandbox init  # to create new sandbox
```

### Projects

```bash
cabal init # to create new project
cabal install --enable-tests -j # to install dependencies and enable unit testing
cabal build # to build the project
cabal run  # to run executable
cabal test  # to run unit tests
```

### Project config
```
name:                cabal-project
version:             0.1.0.0
-- synopsis:            
-- description:         
license:             MIT
license-file:        LICENSE
author:              Alexander Ignatyev
maintainer:          ignatyev.alexander@gmail.com
-- copyright:           
-- category:            
build-type:          Simple
-- extra-source-files:  
cabal-version:       >=1.10

library
  exposed-modules:     Fib
  -- other-modules:       
  -- other-extensions:    
  build-depends:       base >=4.8 && <4.9
  hs-source-dirs:      src
  default-language:    Haskell2010

executable fib
  main-is:                 Main.hs
  build-depends:       base >=4.8 && <4.9
                        , cabal-project
  hs-source-dirs:       app
  default-language:   Haskell2010

test-suite tests
  type:                exitcode-stdio-1.0
  main-is:             HUnitTests.hs
  hs-source-dirs:      tests, src
  build-depends:       base >=4.8 && <4.9
                        , HUnit
  default-language:    Haskell2010
```

## Cabal Hell and sandboxes.

Cabal is considered as difficult to use due to the issue called "dependency hell", in which cabal is not able to install desired package because of some dependency conflicts. Resolving these conflict might require a large amount of work or may be even impossible. In most cases conflicts can be resolved by easing version bounds. The probability of version conflicts is much higher if you use global storage for Haskell libraries. Resolving version conflicts can require packages removals and reinstalls. These operation much easier to perform using private storage for libraries.

This is exactly why cabal sandbox was introduced. Usually I create a new sandbox as a start for new Haskell experiment or project.

So to create a new sandbox you just need to run:

```bash
mkdir my_sanbox && cd my_sanbox
cabal sandbox init
```

All following cabal operations in `my_sandbox` directory will be performed in the sandbox.

One remark regarding executables from cabal packages. If you install a cabal package into the sandbox, executables are placed in the dir .cabal_sandbox/bin. You can run an executable right from this directory or use:

    cabal exec <executable-name>

## Using Cabal to setup a new Haskell project

The command

    cabal init

starts an interactive session or a wizard to create new Haskell project.

Defaults answers in the interactive session should be good enough but with 3 remarks:

1. you need to choose project license otherwise cabal will refuse to build the project;

1. specify source directory as `src`;

1. It is better to select project type as library even if you going to create executable because it is always good practice to put logic of the application in the library and leave for the application only UI code that calls library functions.

As a result of running `cabal init` a few files will be created, the most important of them is a project file:`<project_name>.cabal`. There is an example of a project file:

    name:                cabal-project
    version:             0.1.0.0
    -- synopsis:            
    -- description:         
    license:             MIT
    license-file:        LICENSE
    author:              Alexander Ignatyev
    maintainer:          my-email@my-email.com
    -- copyright:           
    -- category:            
    build-type:          Simple
    -- extra-source-files:  
    cabal-version:       >=1.10

    library
      -- exposed-modules:     
      -- other-modules:       
      -- other-extensions:    
      build-depends:       base >=4.8 && <4.9
      hs-source-dirs:      src

Let's quickly go through the file. It contains 2 sections, the unnamed section with common information regarding the package: name, version, license, author etc. The following section library contains:

* exposed-modules with the list of exported modules of the library, every module that library client is intended to use must be explicitly listed here
* build-depends with the list of external packages with optimal versions bounds
* hs-source-dirs defines list of firs contain Haskell source code.

E.g. if we would like to expose module Data.BinaryTree from the library we should create at least src/Data/BinaryTree.hs file.

## Adding Executables

Every cabal project can contains only one library but as many executable as you wish.

First of all, let us add some library code. For example, naive implementation of Fibonacci numbers:

```haskell
-- src/Fib.hs
module Fib
(
  fib
)

where

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
```

and add the module to the list of exposed modules:

    exposed-modules:    Fib

Not create a simple application that takes a list of numbers and calculate Fibonacci numbers for them. Put the code into `app/Main.hs` file:

```haskell
-- app/Main.hs
module Main where

import Fib (fib)
import System.Environment (getArgs)
import Control.Monad (forM_)

main = do
  args <- getArgs
  forM_ args $ print . fib . read
```

and add new `executable` section to the project file

After all changes out project file should looks like:

    name:                cabal-project
    version:             0.1.0.0
    -- synopsis:            
    -- description:         
    license:             MIT
    license-file:        LICENSE
    author:              Alexander Ignatyev
    maintainer:          ignatyev.alexander@gmail.com
    -- copyright:           
    -- category:            
    build-type:          Simple
    -- extra-source-files:  
    cabal-version:       >=1.10

    library
      exposed-modules:     Fib
      -- other-modules:       
      -- other-extensions:    
      build-depends:       base >=4.8 && <4.9
      hs-source-dirs:      src
      default-language:    Haskell2010

    executable fib
      main-is:             Main.hs
      build-depends:       base >=4.8 && <4.9
                         , cabal-project
      hs-source-dirs:      app
      default-language:    Haskell2010

Take into account:
*  how we specify a source file with `main` function using `main-is` property;
* a source dir using `hs-source-dirs` and
* build dependencies, one of them is our `cabal-project` library

## Adding Unit Tests

Let's quickly go through the process of adding uni tests to Haskell project. There are a few of unit testing libraries available in Haskell. I chose HUnit just because it looks modern and sounds familiar. The simple unit tests of our Fibonacci function may look something like:

```haskell
-- tests/HUnitTests.hs
module Main where

import System.Exit (exitSuccess, exitFailure)
import Test.HUnit
import Fib (fib)

fibTests = TestList [
    "0" ~: 0 ~=? fib 0
  , "1" ~: 1 ~=? fib 1
  , "10" ~: 55 ~=? fib 10
  ]

tests = TestList [
  TestLabel "Fib" fibTests
  ]

main = do
  stats <- runTestTT tests
  if (errors stats + failures stats == 0)
    then exitSuccess
    else exitFailure
```

Updated project file:

    name:                cabal-project
    version:             0.1.0.0
    -- synopsis:            
    -- description:         
    license:             MIT
    license-file:        LICENSE
    author:              Alexander Ignatyev
    maintainer:          ignatyev.alexander@gmail.com
    -- copyright:           
    -- category:            
    build-type:          Simple
    -- extra-source-files:  
    cabal-version:       >=1.10

    library
      exposed-modules:     Fib
      -- other-modules:       
      -- other-extensions:    
      build-depends:       base >=4.8 && <4.9
      hs-source-dirs:      src
      default-language:    Haskell2010

    executable fib
      main-is:                 Main.hs
      build-depends:       base >=4.8 && <4.9
                            , cabal-project
      hs-source-dirs:       app
      default-language:   Haskell2010

    test-suite tests
      type:                exitcode-stdio-1.0
      main-is:             HUnitTests.hs
      hs-source-dirs:      tests, src
      build-depends:       base >=4.8 && <4.9
                            , HUnit
      default-language:    Haskell2010

New section `test-suits` has the same structure as `executable` section with some differences.

I personally prefer to specify `src` folder in `hs-source-dirs` parameter in contract to add tested library in `build-depends` because it allows to test not only exposed modules, but all modules of the library.

Run the following command to install new packages (in our case HUnit) and enable unit testing:

    cabal install --enable-tests -j

You do not really need to specify `--enable-tests` if you omit it cabal will enable tests for you after your first attempt to build and run tests.

To run unit tests just execute the command:

    cabal tests
