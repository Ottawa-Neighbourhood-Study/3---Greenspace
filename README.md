Preface
=======

This is a working document and will be updated and improved as time goes
on. If you have questions or comments please contact Christopher
Belanger at
<a href="mailto:cbela092@uottawa.ca" class="email">cbela092@uottawa.ca</a>.

Summary
=======

This Ottawa Neighbourhood Study (ONS) GitHub repository includes the
code and methodology for creating an area crosswalks between postal
codes and ONS neighbourhoods, and also includes the resulting crosswalk
files. The two main crosswalk files are:

-   **LDUS\_all\_ONS\_long.csv**: A “long” weighted postal-code/ONS
    Neighbourhood crosswalk file. Each row identifies a postal code, an
    ONS Neighbourhood ID, and the proportion of the postal code within
    that ONS Neighbourhood. Each postal code can be mapped to more than
    one ONS Neighbourhood.
-   **LDUS\_all\_ONS\_SLI.csv**: A “long” single-link indicator
    postal-code/ONS neighbourhood crosswalk file. Each row identifies a
    postal code and a single ONS Neighbourhood ID. Each postal code is
    mapped to only one ONS Neighbourhood.

In addition, two other “augmented” files include everything from the two
files above plus new postal codes collected by ICES. These “augmented”
files are datestamped since they may need to be updated. These files
are:

-   **LDUS\_ONS\_augmented\_long\_(2020.12.02).csv**: Same format as the
    long file above.
-   **LDUS\_ONS\_augmented\_SLI\_(2020.12.02).csv**: Same format as the
    SLI file above.

Methodology
===========

The code and methodology are presented and described in the two .Rmd
files. This section may be updated to give an overview of the
methodology if time permits.

Workflow
========

This project is written in
[RMarkdown](https://bookdown.org/yihui/rmarkdown/) using
[RStudio](https://rstudio.com/).

Note on Proprietary Data
========================

**Please note** that the code in this repository requires non-public
proprietary data files in order to work. Specifically, we’re not able to
share the postal code shapefile due to copyright.
