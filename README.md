# NotchedBoxplot Package

## NotchedBoxplot

**NotchedBoxplot** is an R package designed to create the dual-notched boxplot proposed by Kong et al. (2026). This visualization tool enables the simultaneous comparison of both **group means** and **group medians**. The mean and median notches are displayed independently on opposite sides of the box, which avoids the notch protrusion and improves the visual interpretation. 


## Features

### `notched_boxplot()`

Generates a customizable dual-notched boxplot using ggplot2. It provides the geometric visual representations of the mean and median notches and allows users to configure the display sides, colors, widths, and notch depths to suit their data visualization needs.

- **mean notch** for comparing the group means.
- **median notch** for comparing the group medians.

## Installation

To install the **NotchedBoxplot** package from GitHub, please use the following commands in R:

```r
install.packages("remotes")
remotes::install_github("kongdr/NotchedBoxplot")
```

## Documentation
For detailed documentation, parameter descriptions, and the latest updates, please visit the package's GitHub repository at: https://github.com/kongdr/NotchedBoxplot

## Usage
Below are quick examples demonstrating how to create dual-notched boxplots using standard built-in R datasets.

### Example 1: Comparing the means only
Using the built-in ToothGrowth dataset to display only the mean notch.

```r
library(NotchedBoxplot)

# Create a mean-notched boxplot
notched_boxplot(data = ToothGrowth,
              group_col = "supp",
              value_col = "len",
              show_mean_ci = TRUE,
              show_med_ci = FALSE)
```
### Example 2: Comparing simultaneously the group means and medians 
Using the built-in iris dataset to compare both the group means and the group medians.

```r
library(NotchedBoxplot)

# Create a dual-notched boxplot
notched_boxplot(data = iris,
              group_col = "Species",
              value_col = "Sepal.Width")
```


## References
Kong, D., He, X., Wang, W., and Tong, T. (2026).  Dual-notched Boxplot: A New Visualization for Simultaneous Comparison of Means and Medians.


