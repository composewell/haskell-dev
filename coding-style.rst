Haskell Coding Style
====================

The goals of a coding style are as follows:

* to have a uniform coding style across a project
* to make the code easy to read

In some cases there are reasons for choosing a particular style over
the other, in other cases we just choose one particular style without
any strong reasons just for the sake of uniformity. The choices
could be subjective, not all may agree but we need to have one style
nevertheless. As far as possible we try to keep style conventions
consistent across different language constructs as well.

Match the local style
---------------------

As long as possible please try to match the style of the file or the
surrounding code. Sometimes we may have copied a file from somewhere
else and we do not want to change the style, so its important to adhere
to the local style rather than mixing styles.

Define before use
-----------------

Define top level declarations before their first use in the file. In
other words, use the `bottom up style` to organize top level declarations.

There are two rationales for this rule, (1) choose an order instead of
a random ordering, (2) facilitate easier type error debugging on new
changes.

When refactoring, e.g. changing a fundamental type, comment out all the
exports and all the top level declarations in the file using a block
comment. Then uncomment the first top level declaration, change it, fix
the errors, then uncomment the next one, change it and fix the errors
and so on, this way we can incrementally expose the declarations and fix
them. This works because we have defined each declaration before its
use.  Reduced scope of type inferencing makes the type errors localized.

If you use type signatures on all top level declarations commenting out
code may not be necessary for localized inferencing. However, define
before use results in a better order of type errors.

Also, you can test the code in a similar incremental fashion. You can
make a lower level piece of code working and tested first and then move
on to the code using it by moving further down in the file.

To summarize, this is useful when refactoring or changing code in bulk.

List modules in dependency order
--------------------------------

Use the same principle as described in the previous section for
listing modules in the cabal file, list them in dependency order. When
refactoring, start with the module higher most in the list and comment
out the rest, the whole library still keeps compiling.

Line Length
-----------

Maximum 80 columns. This is very helpful when seeing diffs side by side.

Indentation
-----------

Indent each level by 4 spaces. 2 spaces makes the code look cluttered, I
would prefer 8 spaces but it often makes lines too long in Haskell with
multiple indentation levels. As a reference, Linux kernel uses 8 spaces
and it looks very neat.

If expressions or lines become too long or there are too many
indentation levels then it means you need to one of the following:

* Use a `let` clause to name parts of your expression
* Use a `where` clause to factor out some definitions
* Use top level definitions

Module level pragmas
--------------------

Keep the lines sorted by the pragma name, do not align the ends of lines::

  {-# LANGUAGE BangPatterns #-}
  {-# LANGUAGE CPP #-}
  {-# LANGUAGE ConstraintKinds #-}

Aligning end of lines becomes a headache when new entries are added, it
requires changing all the lines and produces unnecessary diffs.

Module Declaration
------------------

Single line ::

  module Streamly.Internal.Data.Stream (Step (..)) where

Multi-line ::

  module Streamly.Internal.Data.Stream
      (
      -- * The stream type
        Step (..)

      -- * Construction
      , nil
      , nilM
      , cons
      )

  where

Imports
-------

* Import symbols explicitly by names as long as possible. It helps readers in
  finding out where the symbol is coming from.
* Import qualified if there are too many symbols to be imported

Single line::

    import Control.Concurrent (killThread, myThreadId, takeMVar, threadDelay)

Multi line::

    import Control.Exception
        (assert, Exception, SomeException, AsyncException, fromException, mask_)

    import Prelude hiding 
        (map, mapM, mapM_, repeat, foldr, last, take, filter, takeWhile, drop
        , concatMap, replicate, enumFromTo, concat, reverse, iterate, splitAt
        )

Delineating Sections
--------------------

In the file use comments as follows to delineate different logical sections,
dashes are up to 79 columns::

    -------------------------------------------------------------------------------
    -- Type
    -------------------------------------------------------------------------------

Data Declarations
-----------------

Separate data declarations by a blank line.

Single line ::

  {-# ANN type Step Fuse #-}
  data Step s a = Yield a s | Skip s | Stop

  data Person = Person String String Int

  -- Single field records
  data Person = Person {firstName :: String}

Multi line ::

  -- | Sum types
  data Step s a =
        Yield a s -- ^ Yield
      | Skip s    -- ^ Skip
      | Stop      -- ^ Stop

  -- | Product types (prefer records when there are too many fields)
  data Person = Person
      String String Int

  data Person = Person
      String -- ^ First name
      String -- ^ Last name
      Int    -- ^ Age

  -- | Records
  data Person = Person
      { firstName :: String  -- ^ First name
      , lastName  :: String  -- ^ Last name
      , age       :: Int     -- ^ Age
      } deriving (Eq, Show)

  -- | Records, with long comments for fields
  data Person = Person
      { 
        -- | First name
        firstName :: String

        -- | Last name
      , lastName  :: String

        -- | Age
      , age       :: Int
      } deriving (Eq, Show)

Sequence Types
--------------

Single line::

    list = [One, Two, Three]

    tuple = (One, Two, Three)

Multi line::

    list =
        [ One
        , Two
        , Three
        ]

    tuple =
        ( One
        , Two
        , Three
        )

Expressions
-----------

Use single whitespace to separate operators and terms. Do not use
whitespace after opening and before closing parentheses. Do not use
whitespace between lambda and the first argument.

::
    
    a + b
    (a + b)
    \x -> return x

Avoid creating long expressions, name parts of a long expression using `let`,
`where` or top level binding and use those names to make the expression
shorter.

`case` statements
-----------------

DO NOT USE THIS ::

    foobar = case x of
        Just j -> foo
        Nothing -> bar

Use this instead ::

    foobar =
        case x of
            Just y -> foo
            Nothing -> bar

Nested/multi line case alternatives::

    foobar =
        case x of
            Just y ->
                case y of
                    Just z -> ...
                    Nothing -> ...
            Nothing -> bar

`do` block ::

    foobar =
        case x of
            Just y -> do
                case y of
                    Just z -> ...
                    Nothing -> ...
                putStrLn "hello"
            Nothing -> bar

Do not align the arrows.

Lambdas
-------

Single line::

  f x = g $ h $ \y -> putStrLn y

Multi line::

  f x =
      g $ h $ \y -> do
          putStrLn "hello "
          return y

  f x =
      ( g
      $ h
      $ \y -> do
          putStrLn "hello "
          return y
      )

`if`-`then`-`else`
------------------

Single line ::

    if x then y else z

    if x
    then y
    else z

Multi line ::

    if x
    then
        case y of
            True -> ...
            Flase ...
    else z

Cascading ::

    if x
    then y
    else if z
    then u
    else v

Its preferable to not mix single line and multi-line formats, but sometimes you
can, use your judgement.

Variable Naming
---------------

Use camelCase.

Do not capitalize all letters of an abbreviation, it may become
problematic if capitals are next to each other e.g. `decodeHTTPUTF8` vs
`decodeHttpUtf8`.

In general, avoid using a prime on the variable names, e.g. use `step1`
instead of `step'`. Numbered indexing is better because it is easier
on the eyes and we can represent multiple generations of the variables
without adding more characters e.g. we can write `step2` instead of
`step''`.

Use shorter variable names for shorter scopes, and longer variable names for
bigger scopes.

Top Level Definitions
---------------------

* Declarations should be separated by a blank line.
* Each declaration must have a type signature
* `INLINE` and `SPECIALIZE` pragmas must be placed before the signature
* haddock comments should come before the pragmas
* There should be no blank lines between haddock, pragmas, signature and
  declaration.
* Do not use a blank line between multiple equations of the same function.

::

  -- | An empty 'Stream'.
  {-# INLINE nil #-}
  nil :: Monad m => Stream m a
  nil = Stream (\_ _ -> return Stop) ()

  -- | An empty 'Stream' with a side effect.
  {-# INLINE nilM #-}
  nilM :: Monad m => m b -> Stream m a
  nilM m = Stream (\_ _ -> m >> return Stop) ()

Pragmas are important for performance, they are placed before the
signature so that they are clearly visible (compared to placement after
the function definition).

Single line::

  nil = Stream (\_ _ -> return Stop) ()

Multi line::

  -- fit in two lines when one line is too long
  nil =
      Stream (\_ _ -> return Stop) ()

  f x =
      case x of
          1 -> ...
          2 -> ...
          _ -> ...

Signatures
----------

To keep signatures consistent with function definition formatting style,
we keep the `::` on the same line as the function name as we keep `=` on
the same line in definitions.

Single line ::
    
    f :: (Monad m, IsStream m, Num a) => a -> t m a

Multi line ::
    
    -- fit in two lines when one line is too long
    -- without constraints
    f ::
        a -> t m a

    -- fit in two lines when one line is too long
    -- with constraints
    f :: (Monad m, IsStream m, Num a)
        => a -> t m a

    f ::
        (Monad m, IsStream m, Num a)
        => a -> t m a

    f ::
        ( Monad m    -- ^ Monad
        , IsStream m -- ^ Stream
        , Num a      -- ^ Num
        )
        => a         -- ^ a
        -> t m a     -- ^ t m a

Let Clause
----------

Single line ::
    
    let x = f x in x

Multi line, align the end of `let` with end of `in`, this alignment is
compatible with `do` blocks which require `in` to be indented beyond the start
of `let`::

    let x = f x
     in x

Multi line with single line definitions::

    let f x = x
        g x = x
     in f y + g y

Multi line, indent the body within the definition, separate the
multi line definitions with a blank line::

    let f x y =
            case x of
                True -> x
                False -> y
            ...

        g x y =
            case x of
                True -> x
                False -> y
            ...
     in f a b || g c d

Its preferable to not mix single line and multi-line formats, but sometimes you
can, use your judgement.

Where Clause
------------

Use a blank line before and after the `where` clause.

Single line::

  f x = 
      ...

      where f1 = ...

Multi line, do not indent the body of `where` clause::

  f x = 
      ...

      where

      f1 = ...

      f2 y = do
          putStrLn x
          ...

Single line definitions within `where` may omit blank lines between them::

  f x = 
      ...

      where

      f1 = ...
      f2 y = ...

`do` Blocks
-----------

Usually the `do` keyword can be combined with the previous line::

    parselMx' pstep initial extract (Stream step state) = do
        initial >>= go SPEC state []
        ...

    if x == y
    then do
        ...
        ...
    else do
        ...
        ...

    let f x y = do
            putStrLn x
            putStrLn y
            ...
     in f y

If not, start a `do` like this::

    do
        putStrLn "hello"
        putStrLn "hello"

Guards
------

Single line ::

  f (One x)
      | x < y = True
      | otherwise = False

Multi line ::

  f (One x)
      | x < y =
          case x of
            1 -> ...
            2 -> ...
            _ -> ...
      | otherwise = False

In case ::

  case x of
      One y
          | y < z1 ->
              f z1
          | y < z2 -> do
              ...
              ...
          | otherwise ->
              f y
      Two y ->
          ...

Its preferable to not mix single line and multi-line formats, but sometimes you
can, use your judgement.

Function Application & Composition
----------------------------------

Single line::

    y = f $ g $ h x
    y = h x & g & f
    k = f . g . h

Multi line::

    func =
        ( S.drain
        $ encodeLatin1Lax
        $ S.concatUnfold A.read
        $ S.concatMapWith parallel use
        $ S.unfold TCP.acceptOnPort 8090
        )

Multi line in `do` block::

    func = do
        putStrLn "do block"
        ( S.unfold TCP.acceptOnPort 8090
        & S.concatMapWith parallel use
        & S.concatUnfold A.read
        & encodeLatin1Lax
        & S.drain
        )

Haddock
-------

* Exported declarations must have haddock documentation.
* Add examples, annotations like `See also`, `Unsafe`, `Time
  complexity`, `Space complexity`, `since` where applicable.

::

  -- | Create an @Array Word8@ of the given length from a machine address
  -- 'Addr#'.
  --
  -- >>> fromAddr# 5 "hello world!"#
  -- > [104,101,108,108,111]
  --
  -- /See also: 'fromString#'/
  --
  -- /Unsafe/
  --
  -- /Time complexity: O(1)/
  --
  -- /Space complexity: O(1)/
  --
  -- @since 0.8.0
  --
  {-# INLINE fromAddr# #-}
  fromAddr# :: Int -> Addr# -> IO (Array Word8)
  fromAddr# n addr# = do

