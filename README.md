# ConnectednessApproach

## Step 1: Install the devtools package

To install a R package, start by installing the devtools package. The best way to do this is from CRAN, by typing:

```r
install.packages("devtools")
```

## Step 2: Install the package of interest from GitHub

Install the package of interest from GitHub using the following code, where you need to remember to list both the author and the name of the package (in GitHub jargon, the package is the repo, which is short for repository). In this example, we are installing the ConnectednessApproach package created by GabauerDavid.

```r
library(devtools)
install_github("GabauerDavid/ConnectednessApproach")
```
