# Model-based Analysis of Protein Degradability (MAPD) 

## Background

Targeted protein degradation (TPD) has rapidly emerged as a therapeutic modality to eliminate previously undruggable proteins because of a unique mechanism-of-action that hijacks the cell’s endogenous protein degradation machinery. However, development of TPD compounds, such as Proteolysis Targeting Chimeras (PROTACs) and “molecular glues”, is largely driven by trial-and-error. Recent systematic studies of the kinome have shown dramatic differences in protein degradation between kinases that have similar drug-target engagement, suggesting unexplained factors influence efficacy. In our study, we investigated how 42 protein features spanning post-translational modifications, protein stability, protein expression and protein-protein interaction influence the degradability of the kinome. By developing a machine learning model, named MAPD (Model-based Analysis of Protein Degradability), we found that the degradability of the kinome can be well predicted by five particular features, including the fraction of ubiquitination sites, acetylation sites, and phosphorylation sites in the protein, protein half life and protein length. Benchmarking revealed the good performance of MAPD in predicting degradability of both kinase and non-kinase proteins. Thus, we extended our predictions of degradability to the entire proteome. Furthermore, by modeling the ubiquitination machinery, we found that protein degradability is particularly dependent on E2-accessible ubiquitination sites. Finally, we developed a user-friend web platform that incorporates protein intrinsic features, protein ligandability, predicted protein degradability, and accessibility of ubiquitination sites / lysine residues to the E2 enzyme. This valuable resource will enable rapid prioritization of targets amenable to TPD and provide insights into rational design of TPD drugs.  


## Notes
This repo includes source codes for repeating the analysis in our study, as well as source codes of the shiny app of MAPD.

To repeat our analysis, please download the Repeatable_analysis folder and run the R scripts in the jupyter notebooks. All required data and scripts are included in the folder. 

The source codes of the shiny app (http://mapd.cistrome.org/) are deposited in the App folder.

For more detail, please read our paper titled ["Protein intrinsic features predict tractability of targeted protein degradation"](https://cancerdiscovery.aacrjournals.org/content/early/2021/02/15/2159-8290.CD-20-0812) or visit the shiny app at http://mapd.cistrome.org/.

## Citation

Wubing Zhang, 

## Contacts
* Wubing Zhang (wzhang@ds.dfci.harvard.edu)
* X. Shirley Liu (xsliu@ds.dfci.harvard.edu)
