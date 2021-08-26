# Model-based Analysis of Protein Degradability

Targeted protein degradation (TPD) has rapidly emerged as a therapeutic modality to eliminate previously undruggable proteins through hijacking the cell’s endogenous protein degradation machinery. However, development of TPD compounds is largely driven by trial-and-error. Recent systematic TPD studies of the kinome have shown dramatic differences in degradation between kinases with otherwise similar drug-target engagement, suggesting unexplained factors influencing degradability. We therefore developed a machine learning model, MAPD (Model-based Analysis of Protein Degradability), to predict degradability from protein features that encompass post-translational modifications, protein stability, protein expression and protein-protein interactions. We found 5 features to achieve optimal performance of MAPD, with ubiquitination rate being the most highly predictive. Here, we implement the R package MAPD, which enable users to re-train the MAPD model, compare the new model with old model based on cross-validation, and predict proteome-wide degradability. Furthermore, we include ligand availability data and disease association data of proteins for prioritizing TPD targets.

# Documentation
Please visit https://wubingzhang.github.io/MAPD/ or http://cistrome.org/~wubing/MAPD.html.

# Installation
Install this package via github using the `devtools` package.

```R
> devtools::install_github('WubingZhang/MAPD')
```

## Citation

Wubing Zhang, 

## Contacts
* Wubing Zhang (wzhang@ds.dfci.harvard.edu)
* X. Shirley Liu (xsliu@ds.dfci.harvard.edu)