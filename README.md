# do-notation

[![Build Status](https://travis-ci.org/isovector/do-notation.svg?branch=master)](https://travis-ci.org/isovector/do-notation) | [Hackage][hackage]

[hackage]: https://hackage.haskell.org/package/do-notation


## Dedication

> I've just locked an open door. Strange, yet symbolically compelling.
>
> Manny Calavera, Grim Fandango


## Overview

Have you ever wanted to manage siiiick invariants with indexed monads without
giving up your regular monads in the process? `do-notation` lets you do this
with a bunch of type jiggery-pokery behind the scenes.

It also provides the `Ix m` indexed monad which is a free construction over a
regular monad `m`. Cool.


## Usage

```haskell
{-# LANGUAGE RebindableSyntax #-}

import Language.Haskell.DoNotation
import Prelude hiding (Monad (..), pure)
```


## Limitations

The implementation doesn't play very nicely with `do`-blocks bound via `let`.


## Thanks

Huge shout-outs to [Csongor Kiss][kcsongor] for very patiently walking me
through the incoherent instance machinery necessary to make this all work.

[kcsongor]: http://kcsongor.github.io/

