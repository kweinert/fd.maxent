# fd.maxent: Solve Maximum Entropy Problems in R

This R-Package implements several algorithms to solve constrained maximum entropy problems (currently discrete problems only) and provides several applications and use cases to 
explore the pros and cons of the algorithms.

## Installation

You need [R](https://cran.r-project.org/) or [RStudio](https://posit.co/download/rstudio-desktop/) to use the package. 

To install `fd.maxent`, you need the packages `remotes`, `rmarkdown` and `knitr`. These get installed with 

```r
install.packages("remotes")
install.packages("rmarkdown")
install.packages("knitr")
```

Then you can execute

```r
remotes::install_github('kweinert/fd.maxent', build_vignettes=TRUE)
```

Typically, you will be asked during the installation to install or update additional packages. It is recommended to choose the option "CRAN packages only" in this case.

You can test whether the installation was successfull by running the unit tests. This requires the package `tinytest`:

```r
install.packages("tinytest")
library(fd.maxent)
mep_test()
```

## Usage

The main functions in the package start with `mep_`. Two simple problems aim to explain the idea and can be viewed with:

```r
vignette("simple", package="fd.maxent")
```

When the problems become more complex, some help from mapping the problem into a numerical formulation is desireable. The functions `set`, `variab`, `index` and `vec2df` are the approach taken in this package and introduced in the `mapping` vignette.

## Examples

This repository digests some papers and books I read on maximum entropy. You can find the reading list including reviews on [wyrms.de](https://www.wyrms.de/list/123/s/maximum-entropy-principle) and in the `literature.bib` file in the `vignettes` folder.

Here is a list of worked examples:

* `vignette("europawahl19")` is an example which combines several surveys to estimate voting behaviour with respect to socialdemographic variables.
* `vignette("balancing")` is an example which uses the maximum entropy principle to mitigate the survey reponse bias.

## Support

Please reports bugs and ideas via the [Issue-Board](https://github.com/kweinert/fd.maxent/issues). You can contact me also directly via [Mastodon](https://berlin.social/@karstengweinert#)

## License

This project is free software as in GPL-3. See LICENSE for details.

## Roadmap

Next steps are 

- [x] synthetic `balancing` example which reweighs a survey due to known population parameters to remedy nonresponse-bias
- [ ] inequality constraints
- [ ] logistic regression example (Titanic dataset)
- [ ] Poisson regression example 
- [ ] `lotto` example that estimates pick probabilities from observed quotas
- [ ] document the LP approach via a vignette
- [ ] Implement interior point algorithm
- [ ] Implement solver using the `highs` package
- [ ] Comparison of algorithms

Please note that this is a one-man side-project, so don't expect anything soon.
