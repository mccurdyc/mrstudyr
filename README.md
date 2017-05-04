# mrstudyr
---

<img src="https://raw.githubusercontent.com/mccurdyc/mrstudyr/master/graphics/logo/mrstudyr-logo.png" height="250" alt="mrstudyr - retrospective mutant analysis.">

mrstudyr is a tool for retrospectively analysing the effectiveness of
mutation testing reduction techniques. Instead of integrating each
approach into a system, mrstudyr performs the various reduction
techniques on the data collected from a single execution of the
mutation testing phase in the mutation analysis process.

## Dependencies
+ [R](https://www.r-project.org/) (> 3.3.3)

To check which version of R you have installed at the command-line:
```
R --version
```

or in R (check the **version.string** value)

```
version
```

## Imports
+ [readr](https://github.com/hadley/readr)
+ [dplyr](https://github.com/hadley/dplyr)
+ [magrittr](https://github.com/smbache/magrittr)
+ [ggplot2](https://github.com/hadley/ggplot2)
+ [broom](https://github.com/dgrtwo/broom)
+ [parallel](https://stat.ethz.ch/R-manual/R-devel/library/parallel/doc/parallel.pdf)

## Installing

The following commands need to be run in an R console in the `mrstudyr/` directory (type `[sudo] R` in terminal).

To install mrstudyr:
```
install.packages("devtools")
library(devtools)
devtools::install_github("mccurdyc/mrstudyr")
library(mrstudyr)
```

## Usage

[![asciicast](https://asciinema.org/a/86989.png)](https://asciinema.org/a/86989)

## Building and Execution Environment
All of the previous instructions for building, installing, and using mrstudyr have been tested on Mac OS X 10.11 "El Capitan" and
Arch Linux 4.9.11. All of the development and testing was done using R version 3.3.3 "Bug in Your Hair".
While mrstudyr is very likely to work on other Unix-based development environments, we cannot guarantee correct results for systems
different than the ones mentioned previously. Currently, mrstudyr does not provide support for the building, installation,
or use on Windows unless using RStudio --- which also has not been tested.

However, if you have questions or comments, please let me know and I will try to assist you.

## License
[GNU General Public License v3.0](./LICENSE)
