# Modeling range-wide environmental variation and connectivity for the northern bobwhite

Repository for analysis of Bobwhite quail (*Colinus virginianus*) habitat suitability and connectivity

# Bobwhite data acquisition

The primary bobwhite location data used for fitting and evaluating habitat suitability models came from [eBird](https://ebird.org/data/download/ebd?showSuccessMsg=true). The eBird datasets (bobwhite observations and checklist data) that was used for this project were from the 'relDec-2022' data release.

We downloaded the full EBD checklist dataset and used the custom download option to download only observations of northern bobwhite (*Colinus virginianus*) in the United States [here](https://ebird.org/data/download/ebd?showSuccessMsg=true). The bobwhite data is stored here[insert link] and the effort data is stored here[insert link].

Data were further pre-processed using the following steps.

1.  We downloaded the northern bobwhite rangemap from the [IUCN Red List database](https://www.iucnredlist.org/species/22728956/178045540). Then using [this script](https://github.com/csp-inc/pf-bobwhite/blob/main/code/utils/prep-norbo-range-maps.R) we unioned the range geometries and buffered them by X km to provide a coarse range filter for exclusion of sampling points and checklists used for evaluating presence and absence of bobwhite.
2.  Then, we used [this script](https://github.com/csp-inc/pf-bobwhite/blob/main/code/utils/prep-ebird-data-ptf.R) to further process eBird data through a number of filters and construct zero-filled datasets in preparation for species distribution modeling. Additional details related to pre-processing steps can be explored in the script.
