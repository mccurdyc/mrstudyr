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
+ [dplyr](https://github.com/hadley/dplyr)
+ [magrittr](https://github.com/smbache/magrittr)
+ [ggplot2](https://github.com/hadley/ggplot2)
+ [readr](https://github.com/hadley/readr)
+ [Metrics](https://cran.r-project.org/web/packages/Metrics/Metrics.pdf)
+ [broom](https://github.com/dgrtwo/broom)

## Installing
The following commands need to be run in the R console (type `R` in terminal).

To install mrstudyr:
```
install.packages("devtools")
library(devtools)
devtools::install_github("mccurdyc/mrstudyr")
library(mrstudyr)
```

## Usage
### Reading in sample data:
```
data <- read_data("sqlite-avmdefaults.dat")
```

In the accompanying paper, we empirically analyse nine schemas:
CoffeeOrders, Employee, Inventory, Iso3166, JWhoisServer, MozillaPermissions, NistWeather, Person, Products.

To filter the data to only include these schemas, use the following command:

```
data <- select_empirical_study_schemas(data)
```

Since we are only interested in the **NORMAL** type mutants:
```
data <- select_normal_data(data)
```

To test that the sample data was read in and filtered correctly:
```
head(data)
```

Which should produce the following output:

```
Source: local data frame [6 x 7]

            identifier   dbms       schema       operator   type killed  time
                 <chr>  <chr>        <chr>          <chr>  <chr>  <chr> <int>
1 fpbpyn2cnbiinwfyy2mi SQLite CoffeeOrders FKCColumnPairR NORMAL   true    59
2 fpbpyn2cnbiinwfyy2mi SQLite CoffeeOrders FKCColumnPairR NORMAL   true    59
3 fpbpyn2cnbiinwfyy2mi SQLite CoffeeOrders FKCColumnPairR NORMAL   true    88
4 fpbpyn2cnbiinwfyy2mi SQLite CoffeeOrders FKCColumnPairR NORMAL   true    54
5 fpbpyn2cnbiinwfyy2mi SQLite CoffeeOrders FKCColumnPairE NORMAL   true    56
6 fpbpyn2cnbiinwfyy2mi SQLite CoffeeOrders FKCColumnPairE NORMAL   true    49
```

### Analysing Reduction Techniques

| Function Name  | Parameters |
| :-------------: | :-------------: |
| `analyse`  | data from mutation testing on all mutants  |

The `analyse` function performs all of the mutant reduction techniques supported
by mrstudyr. This allows the tool to be extended to include additional reduction
techniques while still only requiring a single command to perform them. Additionally,
the `analyse` function returns a single [tidy](http://vita.had.co.nz/papers/tidy-data.pdf)
data frame consisting of the data collected from performing every reduction technique.

To perform all supported reduction techniques:

```
t <- analyse(data)
```

Which should produce the following output:
```
head(t)

           method       schema trial percentage reduced_numerator reduced_denominator original_numerator original_denominator reduced_time original_time cost_reduction reduced_mutation_score original_mutation_score
1 random_sampling CoffeeOrders     1          1                16                  17               1590                 1680          644         63716      0.9898926              0.9411765               0.9464286
2 random_sampling CoffeeOrders     2          1                15                  17               1590                 1680          613         63716      0.9903792              0.8823529               0.9464286
3 random_sampling CoffeeOrders     3          1                16                  17               1590                 1680          663         63716      0.9895945              0.9411765               0.9464286
4 random_sampling CoffeeOrders     4          1                17                  17               1590                 1680          609         63716      0.9904420              1.0000000               0.9464286
5 random_sampling CoffeeOrders     5          1                17                  17               1590                 1680          580         63716      0.9908971              1.0000000               0.9464286
6 random_sampling CoffeeOrders     6          1                16                  17               1590                 1680          672         63716      0.9894532              0.9411765               0.9464286

...

               method   schema trial percentage reduced_numerator reduced_denominator original_numerator original_denominator reduced_time original_time cost_reduction reduced_mutation_score original_mutation_score
5935 across_operators Products    25        100              1280                1470               1280                 1470        25857         25857              0              0.8707483               0.8707483
5936 across_operators Products    26        100              1280                1470               1280                 1470        25857         25857              0              0.8707483               0.8707483
5937 across_operators Products    27        100              1280                1470               1280                 1470        25857         25857              0              0.8707483               0.8707483
5938 across_operators Products    28        100              1280                1470               1280                 1470        25857         25857              0              0.8707483               0.8707483
5939 across_operators Products    29        100              1280                1470               1280                 1470        25857         25857              0              0.8707483               0.8707483
5940 across_operators Products    30        100              1280                1470               1280                 1470        25857         25857              0              0.8707483               0.8707483

```

### Calculating Efficiency and Effectiveness

There are two functions for calculating the supported efficiency and effectiveness
metrics. These functions are `analyse_calculations` and `analyse_correlation` which
both take as input the data returned from the `analyse` function and return a new tidy
dataframe with the included calculations. Below is the list of calculation functions and the
calculated metric(s) of each one.

| Function Name  | Parameter  | Calculation(s)  |
| :-------------: | :-------------: | :------------- |
| `analyse_calculations` | reduction experiments data | <ul><li>Mutation Score</li><li>Cost Reduction</li><li>MAE</li><li>RMSE</li</ul> |
| `analyse_correlation` | reduction experiments data | Kendall's &tau;<sub>b</sub> Correlation Coefficient  |

### Visualising Performance of Reduction Techniques

| Function Name  | Parameter(s) | Description of Visualisation |
| :-------------: | :-------------: | :------------- |
| `visualise_mutation_score_across_schemas` | reduction experiments data filter to contain only data from four percentages | This is the visualisation present in the accompanying tool paper. A visualisation facetted by percentage with scheams on the horizontal-axis and mutation score on the vertical-axis. (Creation of this visualisation is displayed below) |

#### Creating the `visualise_mutation_score_across_schemas` Visualisation

In the `R` console type the following:

```
d <- read_data("sqlite-avmdefaults.dat")
...
t <- analyse(d)
pers <- c(1, 10, 20, 40)
t_filt <- dplyr::filter(t, method == "random_sampling", percentage %in% pers)
visualise_mutation_score_across_schemas(t_filt)
```

Which produces the following visualisation:
![Sample Visualisation](https://raw.githubusercontent.com/mccurdyc/mrstudyr/master/imgs/schema_v_ms.png)

## Building and Execution Environment
All of the previous instructions for building, installing, and using mrstudyr have been tested on Mac OS X 10.11 "El Capitan".
All of the development and testing was done using R version 3.2.3 "Wooden Christmas-Tree".
While mrstudyr is very likely to work on other Unix-based development environments, we cannot guarantee correct results for systems
different than the ones mentioned previously. Currently, mrstudyr does not provide support for the building, installation,
or use on Windows unless using RStudio.

## License
[GNU General Public License v3.0](./LICENSE)

## FAQ
+ `Error in FUNCTION_NAME_HERE() : could not find function "%>%"`
    + `library(magrittr)`
