# Instructions

To run Chesskell, please run `stack build` from your terminal of choice when in the project directory.

The EDSL can be used via GHCi, like so:

```haskell
$> :t chess pawn e2 to e4 end
```

Or, alternatively, you can enter a Chess game into the empty file `src/DemoFile.hs`, which already has all necessary imports. Please see the paper for relevant examples of longer games.

**NOTE**: You may wish to use `--ghc-options -freduction-depth=0` when running `stack build`.
