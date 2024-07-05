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

# Results

| No. of points | CPU addition costs Bls12_381_G1_add | CPU multiplication costs Bls12_381_G1_scalarMul | total as % of CPU budget (10 000 000 000) |
|---------------|-------------------------------------|-------------------------------------------------|-------------------------------------------|
| 10            | 9 623 350                           | 764 684 780                                     | **7,74%** <br/> (774 308 130)             |
| 30            | 28 870 050                          | 2 294 054 340                                   | **23.23%** <br/> (2 322 924 390)          |
| 50            | 48 116 750                          | 3 823 423 900                                   | **38.72%** <br/> (3 871 540 650)          |
| 80            | 76 986 800                          | 6 117 478 240                                   | **61,94%** <br/> (6 194 465 040)          |
| 100           | 96 233 500                          | 7 646 847 800                                   | **77.43%** <br/> (7 743 081 300)          |
| 300           | 288 700 500                         | 22 940 543 400                                  | **232.29%** <br/> (23 229 243 900)        |
| 500           | 481 167 500                         | 38 234 239 000                                  | **387.15%** <br/> (38 715 406 500)        |
| 800           | 769 868 000                         | 61 174 782 400                                  | **619.45%** <br/> (61 944 650 400)        |
| 1000          | 962 335 000                         | 76 468 478 000                                  | **774.31%** <br/> (77 430 813 000)        |
