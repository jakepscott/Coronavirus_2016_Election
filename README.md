# Coronavirus_2016_Election
This repo contains the code for a [Medium article](https://medium.com/@jakepscott16/what-2016-can-tell-us-about-coronavirus-6dd395c7b64) I wrote on how the impact of the Coronavirus was non-randomly distributed across the United States in a way that related to the 2016 election.

## Getting Started

To replicate the figures in the article, simply download the folders and code in this repo. The order in which you run the code should not matter. The set up code for the county-level analysis is in the County_Setup_Code.R file, which is sourced in the analysis files. 

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

## Author

* **Jake Scott** - [Twitter](https://twitter.com/jakepscott2020), [Medium](https://medium.com/@jakepscott16)
