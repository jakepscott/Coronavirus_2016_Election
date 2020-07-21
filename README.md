# Coronavirus, the 2016 Election, and Differential Views of the Virus
I wrote the code found in this repo to generate figures for a [Medium article](https://medium.com/@jakepscott16/what-2016-can-tell-us-about-coronavirus-6dd395c7b64) called *What 2016 Can Tell Us About Coronavirus*. Specifically, I made figures to demonstrate that impact of the Coronavirus has been non-randomly distributed across the United States based on the political leanings of counties/states. Up to the point in which I wrote the article, cases had been concentrated in blue states and counties, though I note that that pattern had begun to change.

![image](https://user-images.githubusercontent.com/56490913/88094007-d475a480-cb60-11ea-8374-33b30e23ade5.png)


## Getting Started

To replicate the figures in the article, simply download the folders and code in this repo. The order in which you run the files should not matter. One wrinkle to note is that the set up code for the county-level analysis is in the County_Setup_Code.R file, which is sourced in the analysis files and thus does not need to be run seperately. 

For specific figures, in the order in which they appear in the article:
* *Figure 1, Full County Gap*: County_Level_Cases_Gap.R
* *Figure 2, Zoomed in County Gap*: County_Level_Cases_Gap.R
* *Figure 3, Trend by Margin of Victory (with Suffolk)*: Trend_by_Victory_Margin.R
* *Figure 4, Trend by Margin of Victory (without Suffolk)*: Victory_Margin_Without_Suffolk.R
* *Figure 5, Geo_Facet Map by State*: County_Level_Cases_Gap.R
* *Figure 6, State Gap in Cases*: State_Level_Cases.R
* *Figure 7, County Gap in Deaths*: County_Level_Cases_Gap.R

### Prerequisites

To run the code on your computer you just need R and the following packages installed and loaded:

```
library(tidyverse)
library(readr)
library(lubridate)
library(zoo)
library(ggtext)
library(scales)
library(geofacet)
library(patchwork)
```

### Datasets
The election data came from the [MIT Election lab](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ). The population data came from the [Census Bureau](https://www.census.gov/data/tables/time-series/demo/popest/2010s-counties-total.html). 

## Author

* **Jake Scott** - [Twitter](https://twitter.com/jakepscott2020), [Medium](https://medium.com/@jakepscott16)
