# To run test do:
```bash
nix develop --no-eval-cache
cabal build msm-testing
cabal exec msm-testing
```

# To format code do
```bash
find . -name "*.hs" -exec fourmolu -i {} \;
```

# To process profiling data do
```bash
nix run github:IntersectMBO/plutus/1.30.0.0#uplc -- evaluate -t -i MSM.flat --if flat-namedDeBruijn --trace-mode LogsWithBudgets -o logs
cat logs | nix run github:IntersectMBO/plutus/1.30.0.0#traceToStacks | nix run nixpkgs#flamegraph > cpu.svg
cat logs | nix run github:IntersectMBO/plutus/1.30.0.0#traceToStacks -- --column 2 | nix run nixpkgs#flamegraph > mem.svg
```