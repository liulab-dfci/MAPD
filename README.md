# Model-based Analysis of Protein Degradability

Targeted protein degradation (TPD) has rapidly emerged as a therapeutic modality to eliminate previously undruggable proteins through repurposing the cell’s endogenous protein degradation machinery. However, development of TPD compounds is largely driven by trial-and-error. We developed a machine learning model, MAPD (Model-based Analysis of Protein Degradability), to predict degradability from protein-intrinsic features that encompass post-translational modifications, protein stability, protein expression and protein-protein interactions. MAPD shows promising performance in predicting kinases that are degradable by TPD compounds and is likely generalizable to independent non-kinase proteins.

Here, we designed R package MAPD to make it quick and easy to：

* Reproduce our MAPD model for benchmarking
* Investigate protein features predictive of protein degradability
* Extend the MAPD model by incorporating new protein degradability data and/or protein feature data.

For more detail, please visit https://wubingzhang.github.io/MAPD/articles/MAPD.html

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
