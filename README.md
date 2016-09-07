# mrstudyr

mrstudyr is a tool for retrospectively analysing the effectiveness of
mutation testing reduction techniques. Instead of integrating each
approach into a system, mrstudyr performs the various reduction
techniques on the data collected from a single execution of the
mutation testing phase in the mutation analysis process.

## Dependencies
+ [R](https://www.r-project.org/) (> 3.1.2)

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
+ [Cairo](https://cran.r-project.org/web/packages/Cairo/index.html)

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

[![asciicast](https://asciinema.org/a/2xnxuapeku9nljfnrx522d8x1.png)](https://asciinema.org/a/2xnxuapeku9nljfnrx522d8x1)

### Analysing Reduction Techniques

| Function Name  | Output |
| :-------------: | :-------------: |
| `create_random_sampling_graphs`  | Creates all graphs for the random sampling reduction technique |
| `create_operator_sampling_graphs`  | Creates all graphs for the operator sampling reduction technique |

Both of these functions create a single visualization in the current state of the project but, can easily be extended
to create additional informative visualizations.

#### Creating the Visualization in the Accompanying Paper

To create the visualization in the accompanying paper, a single function call is necessary: `create_random_sampling_graphs`.
In the paper, the visualization is focused on the random sampling mutant reduction technique but, creating the same visualization
for the operator sampling technique is just as easy, `create_operator_sampling_graphs`.

Which produces the following visualization from the paper:

<p align="center">
<img src ="https://raw.githubusercontent.com/mccurdyc/mrstudyr/master/imgs/schema_v_ms.png">
</p>

## Building and Execution Environment
All of the previous instructions for building, installing, and using mrstudyr have been tested on Mac OS X 10.11 "El Capitan".
All of the development and testing was done using R version 3.2.3 "Wooden Christmas-Tree".
While mrstudyr is very likely to work on other Unix-based development environments, we cannot guarantee correct results for systems
different than the ones mentioned previously. Currently, mrstudyr does not provide support for the building, installation,
or use on Windows unless using RStudio.

## License
[GNU General Public License v3.0](./LICENSE)
