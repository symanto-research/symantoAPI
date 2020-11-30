<br/>

<p align="center">
<img src="man/figures/symanto.png" alt="drawing" width="200"/>
</p>

Symanto is a company specializing in discovering consumer insights through text analytics. The combination of psychology, statistics and artificial intelligence makes Symanto a pioneer in the field of customer analytics. 

# symantoAPI

**symantoAPI** is an R package to analyze text data by calling the Symanto API (Application Programming Interface). Methods available include the extraction of personality traits, communication style, emotions, Ekman's emotions, sentiments, topic sentiments and language. This new technology allows us to identify the global thoughts and insights that the customer is conveying. 

### Getting started 

In order to get acquianted with this library, we recommend starting with the reference functions and the articles from the top menu. Further information about the API is available at [its home website.](https://developers.symanto.net/) 

### Installation

Please install the latest development version from GitHub:

```r
# The following option may be needed. Try first without it.
# options(download.file.method = "wininet") 
# In addition, some packages such as kableExtra should be installed first in order to build the vignettes.
library(devtools)
install_github("symanto-research/symantoAPI", build_vignettes = TRUE)
```
### Loading

```r
library(symantoAPI) 
```

<p align="center">
<img src="man/figures/symanto_background.png" alt="drawing" width="200"/>
</p>
