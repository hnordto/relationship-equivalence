# Relationship equivalence

Code for analysis and visualization of topics concerning genetically indistinguishable simple unilineal relationships. 

The repository contains scripts to reproduce the results of the following paper: Nordtorp, H. & Vigeland, M.D. A classification of genetically indistinguishable unilineal relationships. (Manuscript submitted for publication). 

## Overview

All analyses are located in the [scripts/](scripts/) folder. Simulations are performed with the R package [`ibdsim2`](https://github.com/magnusdv/ibdsim2) (Vigeland, 2021).

- [scripts/peds_unilineal.R](scripts/peds_unilineal.R): Visualization of the four types of simple unilineal relationships with fixed, free, and anchor individuals highlighted. (Figure 1).
- [scripts/peds_examples.R](scripts/peds_examples.R): Examples of equivalent and non-equivalent relationships with their simulated IBD distributions in terms of mean segment lengths and counts. (Figure 2).
- [scripts/length_count_ranges.R](scripts/length_count_ranges.R): Large-scale simulations to investigate first order summary statistics of IBD segments for various equivalence classes. (Figure 3 and Supplementary Figure S2).
- The remaining scripts were mainly used for development and testing purposes.

Note that some of the scripts require a working version of the R package [`ibdrel`](https://github.com/hnordto/ibdrel), not yet available on CRAN. It can be installed with the following command:

```r
pak::pak("hnordto/ibdrel")
```
