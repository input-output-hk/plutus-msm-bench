# To run test do:
```bash
nix develop
cabal build msm-testing
cabal exec msm-testing
```

# To format code do
```bash
find . -name "*.hs" -exec fourmolu -i {} \;
```