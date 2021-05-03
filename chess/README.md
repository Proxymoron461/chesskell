# Compiling Chesskell Games

To run Chesskell, please run `stack build` from your terminal of choice when in the project directory. Make sure that you have [stack](https://docs.haskellstack.org/en/stable/README/) installed.

The EDSL can be used via GHCi, the interactive GHC interpreter, which can be booted using `stack repl`. An example command is below, where `:t` queries the type of the expression:

```haskell
$> :t chess pawn e2 to e4 end
```

Or, alternatively, you can enter a Chess game into the empty file `src/DemoFile.hs`, which already has all necessary imports, and run `stack build`. Please see the dissertation for relevant examples of longer games.

**NOTE**: You may wish to use `--ghc-options -freduction-depth=0` when running `stack build`.

# Testing

To run the test suite, simply enter `stack test` from your terminal of choice when in the project directory.

**NOTE**: Many of the expensive tests have been commented out, as they can cause GHC to crash. However, if you wish to run them, simply remove the `-- ` prefix from the start of the line. For example, to run a test that has been commented out like so:

```haskell
-- castleTest1 :: '(True, True) :~: '(Eval (IsKingAt White WhiteCastleLeftDec (At C Nat1)), Eval (IsRookAt White WhiteCastleLeftDec (At D Nat1)))
-- castleTest1 = Refl

-- castleTestSuite = describe "Castle Tests" $ do
--     describe "Castling tests" $ do
--         it "1: If White castles to the left, the White King should be at C1, with the Rook at D1." $
--             shouldTypecheck castleTest1
```

Simply modify it like this:

```haskell
castleTest1 :: '(True, True) :~: '(Eval (IsKingAt White WhiteCastleLeftDec (At C Nat1)), Eval (IsRookAt White WhiteCastleLeftDec (At D Nat1)))
castleTest1 = Refl

castleTestSuite = describe "Castle Tests" $ do
    describe "Castling tests" $ do
        it "1: If White castles to the left, the White King should be at C1, with the Rook at D1." $
            shouldTypecheck castleTest1
```

Although some type errors may occur during compilation of the test suite, these are intentional; some of these tests are intended not to type check, and the test will succeed when type-checking fails.

# Chesskell Syntax

All relevant Chesskell continuations can be found in `src/FlatBuilders.hs`, but please look in `src/FamousGames.hs` for examples of Chesskell games.
