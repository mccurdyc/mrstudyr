# mrstudyr

mrstudyr is a tool for retrospectively analysing the effectiveness of
mutation testing reduction techniques. Instead of integrating each
approach into a system, mrstudyr performs the various reduction
techniques on the data collected from a single execution of the
mutation testing phase in the mutation analysis process.

## Dependencies
+ R (> 3.0.2)

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
data <- read_data()
```

Since we are only interested in the **NORMAL** type mutants:
```
data <- select_normal_data(data)
```

In the accompanying paper, we empirically analyse nine schemas:
CoffeeOrders, Employee, Inventory, Iso3166, JWhoisServer, MozillaPermissions, NistWeather, Person, Products.

To filter the data to only include data about these schemas use the following command:

```
data <- select_empirical_study_schemas(data)
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

| Reduction Technique  | Function Name (parameters) |
| ------------- | ------------- |
| Uniform Random Sampling  | analyse_random_sampling (data)  |
| Uniform Random Sampling Over Operators  | analyse_across_operators (data)  |

One mutant reduction technique supported by mrstudyr is uniform random sampling. In uniform
random sampling, a maximum threshold percentage, _x_, is set for the percentage of the total
mutants to be analysed. The mrstudyr tools evaluates _x_ from 1% to 90% in increments of 10%.

To perform **uniform random sampling**:
```
rs_data <- analyse_random_sampling(data)
```

Which should produce the following output:
```
head(rs_data)

         schema         trial   percentage   reduced_numerator reduced_denominator original_numerator original_denominator reduced_time original_time reduced_mutation_score original_mutation_score
         CoffeeOrders     1          1                17                  17               1590                 1680          690         63716              1.0000000               0.9464286
         CoffeeOrders     2          1                17                  17               1590                 1680          630         63716              1.0000000               0.9464286
         CoffeeOrders     3          1                17                  17               1590                 1680          718         63716              1.0000000               0.9464286
         CoffeeOrders     4          1                17                  17               1590                 1680          619         63716              1.0000000               0.9464286
         CoffeeOrders     5          1                16                  17               1590                 1680          720         63716              0.9411765               0.9464286
         CoffeeOrders     6          1                15                  17               1590                 1680          573         63716              0.8823529               0.9464286

```

The second mutant reduction technique supported by the mrstudyr tool
is uniform random sampling across operators. Similar to plain old
uniform random sampling, a maximum threshold percentage, _x_, is set
for the percentage of the total mutants to be analysed.
The mrstudyr tools evaluates _x_ from 1% to 90% in increments of 10%.
But, instead of randomly selecting the _x%_ from all of the mutants, the _x%_ is
selected from each operator. The following is the list of operators: FKCColumnPairR, FKCColumnPairE,
PKCColumnA, PKCColumnR, PKCColumnE, NNCA, NNCR, UCColumnA, CCNullifier, CCRelationalExpressionOperatorE,
UCColumnE, CCInExpressionRHSListExpressionElementR.

To perform **uniform random sampling across operators**:

```
os_data <- analyse_across_operators(data)
```

Which should produce the following output:

```
head(os_data)

            schema    trial   percentage    reduced_numerator reduced_denominator original_numerator original_denominator reduced_time original_time reduced_mutation_score original_mutation_score
       CoffeeOrders     1          1                15                  16               1590                 1680          603         63716                 0.9375               0.9464286
       CoffeeOrders     2          1                15                  16               1590                 1680          590         63716                 0.9375               0.9464286
       CoffeeOrders     3          1                15                  16               1590                 1680          573         63716                 0.9375               0.9464286
       CoffeeOrders     4          1                15                  16               1590                 1680          600         63716                 0.9375               0.9464286
       CoffeeOrders     5          1                15                  16               1590                 1680          686         63716                 0.9375               0.9464286
       CoffeeOrders     6          1                15                  16               1590                 1680          580         63716                 0.9375               0.9464286
```

### Calculating Effectiveness

| Calculation  | Function Name (parameters) |
| ------------- | ------------- |
| Mutation Score  | analyse_mutation_score (data)  |
| Kendall's &tau;<sub>b</sub> Correlation Coefficient  | analyse_correlation (data)  |
| Error (MAE and RMSE)  | analyse_percents_error (data)  |

### Visualising Performance of Reduction Techniques

| Visualisation  | Function Name (parameters) |
| ------------- | ------------- |
|   |   |

## Building and Execution Environment
All of the previous instructions for building, installing, and using mrstudyr have been tested on Mac OS X 10.11 "El Capitan".
All of the development and testing was done using R version 3.2.3 "Wooden Christmas-Tree".
While mrstudyr is very likely to work on other Unix-based development environments, we cannot guarantee correct results for systems
different than the ones mentioned previously. Currently, mrstudyr does not provide support for the building, installation,
or use on Windows unless using RStudio.

## License
[GNU General Public License v3.0](./LICENSE)
