# San Diego RIPA Report


<li><a href="https://www.catalystcalifornia.org/campaign-tools/maps-and-data/racial-bias-in-policing-an-in-depth-analysis-of-stopping">Link to online report</a></li>

<br>

<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a></li>
    <li>
      <a href="#acknowledgements">Acknowledgements</a></li>
    <li>
      <a href="#built-with">Built With</a></li>
    <li><a href="#getting-started">Getting Started</a>
      <ul>
        <li><a href="#prerequisites">Prerequisites</a></li>
      </ul>
    </li>
    <li><a href="#data-methodology">Data Methodology</a>
      <ul>
        <li><a href="#data-sources">Data Sources</a></li>
        <li><a href="#data-limitations">Data Limitations</a></li>
      </ul>
    <li><a href="#contact-us">Contact Us</a></li>
    <li><a href="#about-catalyst-california">About Catalyst California</a>
      <ul>
        <li><a href="#our-vision">Our Vision</a></li>
        <li><a href="#our-mission">Our Mission</a></li>
      </ul>
    </li>
    <li><a href="#partners">Partners</a></li>
    <li><a href="#license">License</a></li>
  </ol>
</details>

## About The Project

This report combines data analysis of San Diego Police Department (“SDPD”) patrol activities, video interviews with community members in Southeast San Diego, and public policy research to show how SDPD criminalizes community members in Southeast San Diego through gang profiling. Gang profiling is a practice through which SDPD–and law enforcement agencies throughout California–document people of color as gang members and place their identities in gang databases. The report unpacks how gang profiling in Southeast San Diego inflicts devastating racially biased harms, undermines community safety, and wastes tremendous public dollars. This GitHub repository includes access to our methodology and scripts to analyze the data and test for racial bias in SDPD's gang profiling activity. The repository does not include access to the data tables used for analysis. We pull tables from our private PostgreSQL database. The database is accessible only by our Research & Data Analysis team. The original Racial and Identity Profiling Act (“RIPA”) data used for this project can be accessed via San Diego's Open Data portal. For access to the community stories gathered as a part of this project, please visit the report page.

<p align="right">(<a href="#top">back to top</a>)</p>

## Acknowledgements

We would like to express our deepest appreciation to Pillars of the Community for their tremendous partnership in this project. Pillars of the Community is an organization that advocates and organizes for people harmed by the criminal legal system in Southeast San Diego. As a part of this project, they collected stories from Southeast San Diego community members who have been impacted by SDPD's practices. Thank you to the Southeast community members who courageously shared their stories and perspectives on how SDPD patrol activities undermine community safety. 

This project was completed with support from the Microsoft Justice Reform Initiative.

The following individuals contributed to the data analysis and visualizations that show in up in the report:

* [Elycia Mulholland Graves, Catalyst California](https://github.com/elyciamg)
* [Jennifer Zhang, Catalyst California](https://github.com/jzhang514)
* [David Segovia, Catalyst California](https://github.com/davidseg1997)
* [Hillary Khan, Catalyst California](https://github.com/hillaryk-ap)

The following individuals contributed to the framing and writing of the report:

* [Chauncee Smith, Catalyst California](https://www.catalystcalifornia.org/who-we-are/staff/chauncee-smith)
* [Mitchelle Woodson, Pillars of the Community](https://www.potcsd.org/about-us)

<p align="right">(<a href="#top">back to top</a>)</p>

## Built With

<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/1b/R_logo.svg/1086px-R_logo.svg.png?20160212050515" alt="R" height="32px"/> &nbsp; <img  src="https://upload.wikimedia.org/wikipedia/commons/d/d0/RStudio_logo_flat.svg" alt="RStudio" height="32px"/> &nbsp; <img  src="https://upload.wikimedia.org/wikipedia/commons/thumb/e/e0/Git-logo.svg/768px-Git-logo.svg.png?20160811101906" alt="RStudio" height="32px"/>

<p align="right">(<a href="#top">back to top</a>)</p>

## Getting Started

To get a local copy up and running follow these simple example steps.

### Prerequisites

We completed the data cleaning, analysis, and visualization using the following software. 
* [R](https://cran.rstudio.com/)
* [RStudio](https://posit.co/download/rstudio-desktop)

We used several R packages to analyze data and perform different functions, including the following.
* data.table     
* dplyr     
* extrafont     
* flextable     
* gt     
* gtExtras     
* highcharter     
* htmltools     
* htmlwidgets     
* knitr     
* leaflet     
* magrittr     
* RPostgreSQL     
* sf     
* showtext     
* stringr     
* sp     
* tidyr     
* tidyverse     
* usethis     
```
list.of.packages <- c("data.table", "dplyr", "extrafont", "flextable", "gt", "gtExtras", "highcharter", "htmltools", "htmlwidgets", "knitr", "leaflet", "magrittr", "RPostgreSQL", "sf", "showtext", "stringr", "sp", "tidyr", "tidyverse", "usethis")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

devtools::install_github("r-lib/usethis")

library(data.table)
library(dplyr)
library(extrafont)
library(flextable)
library(gt)
library(gtExtras)
library(highcharter)
library(htmltools)
library(htmlwidgets)
library(knitr)
library(leaflet)
library(magrittr)
library(RPostgreSQL)
library(sf)
library(showtext)
library(stringr)
library(sp)
library(tidyr)
library(tidyverse)
library(usethis)
```

<p align="right">(<a href="#top">back to top</a>)</p>

## Data Methodology

This report evaluates SDPD gang profiling by analyzing 2022 data collected and reported by SDPD pursuant to the Racial and Identity Profiling Act (“RIPA”) of 2015. For each stop made by law enforcement, RIPA data includes information that can be analyzed for evidence of profiling—including characteristics about the person stopped (e.g., race, gender, and age), the stop location, the length of the stop, the reason for the stop, and the result of the stop. This report uses RIPA data to evaluate SDPD’s gang profiling activities. Key RIPA indicators include the officer assignment field (under “gang enforcement”), and the stop result field (under “field interview card”). You can access our full methodology [here](https://github.com/catalystcalifornia/lbripa/blob/main/Methodology_Racial_Bias_LBPD_2023.pdf).

### Data Sources

Police Stop Data	 

* City of San Diego, San Diego Police Department, 2022, Police Stop Data (RIPA). Retrieved from https://data.sandiego.gov/datasets/police-ripa-stops/. 

Population Estimates by Age and Race 

* U.S. Census Bureau, 2017-2021, American Community Survey, 5-Year Estimates. Tables DP05, B04006, B02018. Retrieved from https://data.census.gov/cedsci/.

* U.S. Census Bureau, 2020 Decennial Census, Demographic and Housing Characteristics. Table P12. Retrieved from https://data.census.gov/cedsci/.

* U.S. Census Bureau, 2021, TIGER/Line Shapefiles, Census Tracts. Retrieved from https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2021&layergroup=Census+Tracts.  

Geographic Data 

* City of San Diego, 2023, San Diego Police Department Beats. Retrieved from https://data.sandiego.gov/datasets/police-beats/.  

* City of San Diego, 2023, San Diego Police Department and Divisions. Retrieved from https://data.sandiego.gov/datasets/pd-divisions/.  

### Data Limitations

As with all data, the findings seen in this analysis are dependent on the quality of the data collected. We strongly encourage readers and data analysts to consider the limitations of RIPA data when interpreting findings or using RIPA data. For instance, RIPA data are collected under state regulations for all law enforcement agencies, but this at times limits the applicability of data elements at the local level. For this project, this translated to officer assignment types collected under RIPA poorly correlating with SDPD units that conduct gang profiling. RIPA data are also based on officers’ reports. The information attached to each stop is solely based on officer disclosure and perceptions. For example, officers report what they perceive as the race(s) of the people they stopped, rather than having the people they stopped self-report their race(s). We encourage researchers using RIPA data to ground truth trends seen in the data with community to identify discrepancies between the data collected and everyday community experiences. For a full discussion of limitations, please see our [Methodology document](https://github.com/catalystcalifornia/lbripa/blob/main/Methodology_Racial_Bias_LBPD_2023.pdf)

<p align="right">(<a href="#top">back to top</a>)</p>


## Contact Us

For policy-related inquiries: 

* [Chauncee Smith](https://www.catalystcalifornia.org/who-we-are/staff/chauncee-smith) - csmith[at]catalystcalifornia.org

* [Mitchelle Woodson, Esq.](https://www.linkedin.com/in/mitchellie-woodson-esq-41064793/) - mitchelle[at]potcsd.org

For inquiries about videos collected or advocacy in Southeast San Diego:

* [Mitchelle Woodson, Esq.](https://www.linkedin.com/in/mitchellie-woodson-esq-41064793/) - mitchelle[at]potcsd.org

For data-related inquiries: 

* [Elycia Mulholland Graves](https://www.linkedin.com/in/elycia-mulholland-graves-54578258/) - egraves[at]catalystcalifornia.org 

* [Jennifer Zhang](www.linkedin.com/in/jenniferzhang3) - jzhang[at]catalystcalifornia.org

<p align="right">(<a href="#top">back to top</a>)</p>

## About Catalyst California

### Our Vision
A world where systems are designed for justice and support equitable access to resources and opportunities for all Californians to thrive.

### Our Mission
[Catalyst California](https://www.catalystcalifornia.org/) advocates for racial justice by building power and transforming public systems. We partner with communities of color, conduct innovative research, develop policies for actionable change, and shift money and power back into our communities. 

[Click here to view Catalyst California's Projects on GitHub](https://github.com/catalystcalifornia)

![Catalyst California logo](W:\RDA Team\Communications\CC Logos\Logos Catalyst California-14.png)

<p align="right">(<a href="#top">back to top</a>)</p>

## Partners

![Pillars of the Community logo](W:\Project\RJS\Pillars\Documentation\Pillars-of-the-Community_Logo.png)

[Website](https://www.potcsd.org/)

<p align="right">(<a href="#top">back to top</a>)</p>

## License

Distributed under the MIT License. See `LICENSE.txt` for more information.

<p align="right">(<a href="#top">back to top</a>)</p>
