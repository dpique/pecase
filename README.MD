# pecase
A Dataframe Containing Winners of Presidential Early Career Awards for Scientists and Engineers (PECASE) from 1996-2017.

I reformated the [pecase data](https://en.wikipedia.org/wiki/Presidential_Early_Career_Award_for_Scientists_and_Engineers) from wikipedia into an R dataframe. A description of how this was done is available in the `data-raw` directory.

Here are a few thoughts on how this data could be used: 

* Track the number of grants and papers published by each PECASE award winner, both before and after the award (combine with [NIH RePORTER](https://projectreporter.nih.gov/reporter.cfm) and [Pubmed](https://pubmed.ncbi.nlm.nih.gov/) data for grants and papers, respectively). 

* Are PECASE awardees successful in obtaining funding and publishing (measures of academic success) after the award?

* Look for gender, race, or institutional bias amongst awardees

* Add additional variables (eg doctoral institution, years since first publication, number of publications, age) for each awardee to look for commonalities among awardees

`pecase` is a dataframe with 1708 rows (one for each awardee) and 4 variables, described below:

* `year`: the year the award was granted. Note that the awards for 2015-17 were announced together in 2019 and are all labeled as 2015 in the dataset, as there was no distinction made between the awardee years for these 3 years.

* `dept`: the governmental department granting the award

* `name`: the name of the award recipient

* `instit`: the awardee's affiliated institution

  
## Installation

```R
## Install the development version of knudson from github
devtools::install_github("dpique/pecase",
    build_opts=c("--no-resave-data", "--no-manual"),
    build_vignettes=TRUE)

library(pecase)

## open the vignette
vignette("pecase")
```
