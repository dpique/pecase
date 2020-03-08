# pecase
Winners of Presidential Early Career Awards for Scientists and Engineers (PECASE)

This package sources the raw [pecase data]() from wikipedia.
A description of how this was done is available in the `data-raw` directory.

These data may be used to assess the success of PECASE award winners. Here are a few thoughts on how this data could be used: (for example, by combining with NIH Reporter data to assess number of grants awarded)
Track the number of papers published by each investigator (by )
Expanding to look for trends in # of awardees by gender.
Add additional variables for each investigator to look for commonalities among awardees (What type of investigator gets the award?)

* `pecase` is a dataframe with 1708 rows (one for each awardee) and 4 variables, described below:

* `year`: the year the award was granted

* `dept`: the governmental department granting the award

* `name`: the name of the award recipient

* `instit`: the awardee's affiliated institution

  
## Installation

```R
## Install the development version of knudson from github
devtools::install_github("dpique/pecase",
    build_opts=c("--no-resave-data", "--no-manual"),
    build_vignettes=TRUE)

## open the vignette
vignette("pecase")
```
