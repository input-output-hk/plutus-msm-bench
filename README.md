# THis repo is based on Plutus Tx Template
https://github.com/intersectMBO/plutus-tx-template

# What is in the repository?

Code that does performance testing of MSM (multi scalar multiplication) implemented with `Bls12_381_G1_add` and `Bls12_381_G1_scalarMul` builtin functions. 
The point of this experiment was to check how much CPU budget is used for naive implementation of MSM and what is the max number of points that can be processed in single transaction.

# What you need to run the tests?
Cabal and Nix

# Cabal in Nix
If you do not want to install cabal, you can run nix shell with this command
```bash
nix develop --no-eval-cache
```

# How to run the tests?
```bash
cabal build msm-testing
cabal exec msm-testing
```
This will create files with `*.flat` extension that can be used with uplc  

# To process profiling data do
```bash
nix run github:IntersectMBO/plutus/1.30.0.0#uplc -- evaluate -t -i MSM.flat --if flat-namedDeBruijn --trace-mode LogsWithBudgets -o logs
cat logs | nix run github:IntersectMBO/plutus/1.30.0.0#traceToStacks | nix run nixpkgs#flamegraph > cpu.svg
cat logs | nix run github:IntersectMBO/plutus/1.30.0.0#traceToStacks -- --column 2 | nix run nixpkgs#flamegraph > mem.svg
```
In first command replace `MSM.flat` with specific test case you want to process, i.e. `20MSM.flat`, which is test case for MSM with 20 scalars and 20 points.
First command will output cost breakdown of the contract. 
It has 3 columns first is the name of the operation, second is CPU cost, third is MEM cost.

Second command takes results of the first command and produces flame graph of CPU usage in form of an SVG.
Third command takes results of the first command and produces flame graph of MEM usage in form of an SVG.
It is best to open SVGs with a web browser.

# Results
This table includes only budget spent on `Bls12_381_G1_add` and `Bls12_381_G1_scalarMul` and represents it as percentage of total transaction CPU budget
But for those 2 operations to work call to `bls12_381_G1_uncompress` is needed which also takes a significant portion of CPU units in transaction. 

| No. of points | CPU addition costs Bls12_381_G1_add | CPU multiplication costs Bls12_381_G1_scalarMul | (add + mul) as % of CPU budget (10 000 000 000) |
|--------------:|------------------------------------:|------------------------------------------------:|------------------------------------------------:|
|            10 |                           9 623 350 |                                     764 684 780 |                   **7,74%** <br/> (774 308 130) |
|            30 |                          28 870 050 |                                   2 294 054 340 |                **23.23%** <br/> (2 322 924 390) |
|            50 |                          48 116 750 |                                   3 823 423 900 |                **38.72%** <br/> (3 871 540 650) |
|            80 |                          76 986 800 |                                   6 117 478 240 |                **61,94%** <br/> (6 194 465 040) |
|           100 |                          96 233 500 |                                   7 646 847 800 |                **77.43%** <br/> (7 743 081 300) |
|           129 |                         124 141 215 |                                   9 864 433 662 |                **99.89%** <br/> (9 988 574 877) |
|           130 |                         125 103 550 |                                   9 940 902 140 |               **100.66** <br/> (10 066 005 690) |
|           300 |                         288 700 500 |                                  22 940 543 400 |              **232.29%** <br/> (23 229 243 900) |
|           500 |                         481 167 500 |                                  38 234 239 000 |              **387.15%** <br/> (38 715 406 500) |
|           800 |                         769 868 000 |                                  61 174 782 400 |              **619.45%** <br/> (61 944 650 400) |
|          1000 |                         962 335 000 |                                  76 468 478 000 |              **774.31%** <br/> (77 430 813 000) |

## Additional table with point uncompress CPU cost
Costs of uncompress that is required to uncompress specific number of points as percentage of total transaction CPU budget.

| No. of points | CPU uncompress costs bls12_381_G1_uncompress | total as % of CPU budget (10 000 000 000) |
|--------------:|---------------------------------------------:|------------------------------------------:|
|            10 |                                  582 429 342 |                                 **5.82%** |
|            30 |                                1 641 391 782 |                                **16.41%** |
|            50 |                                2 700 354 222 |                                **27.00%** |
|            80 |                                4 288 797 882 |                                **42.89%** |
|           100 |                                5 347 760 322 |                                **53.48%** |
|           129 |                                6 883 255 860 |                                **68.83%** |
|           130 |                                6 936 203 982 |                                **69.36%** |
|           300 |                               15 937 384 722 |                               **159.37%** |
|           500 |                               26 527 009 122 |                               **265.27%** |
|           800 |                               42 411 445 722 |                               **424.11%** |
|          1000 |                               53 001 070 122 |                               **530.01%** |