# Preface

This is a working document and will be updated and improved as time goes
on. If you have questions or comments please contact Christopher
Belanger at <cbela092@uottawa.ca>.

# Summary

This Ottawa Neighbourhood Study (ONS) GitHub repository includes the
code and methodology for creating an area crosswalks between postal
codes and ONS neighbourhoods, and also includes the resulting crosswalk
files.

Crosswalk files come in two flavours: weighted files, where each postal
code can be assigned to more than one neighbourhood, and single-link
indicator (SLI) files, where each postal code is linked to one single
neighbourhood.

In addition, crosswalk files have been generated for older “Gen 2” ONS
neighbourhood boundaries and new “Gen 3” boundaries.

Crosswalk files are saved in the `results/` folder, are named according
to their generation and sli/weighted status, and are datestamped.

# Methodology

The process is as follows:

1.  We start with a proprietary dataset of postal codes (local delivery
    units, or LDUs), and ONS neighbourhoods (gen2 or gen3).
2.  We get residential zones according to OpenStreepMaps (OSM).
3.  We “trim” our LDU and ONS datasets to their intersection with the
    OSM residential areas.
4.  For each trimmed LDU, we find its intersection with the trimmed ONS
    neighbourhoods and calculate its area.
5.  Each LDU/ONS intersection area is converted to a weight, by dividing
    that LDU/ONS intersection area by that LDU’s total intersected area.
    In other words, we normalize to the postal code’s residential area
    that intersects any neighbourhood’s residential area, not the postal
    code’s total area.
6.  Some LDUs will not intersect any ONS residential areas, so next we
    account for these. We take the LDUs not in our weighted list, and
    repeat the previous two steps but using *untrimmed* regions, in
    other words using total land area instead of residential land area.
    The results are added to our list of weights.
7.  To create an SLI, we map each LDU to the one neighbourhood it
    overlaps the most.

Then we manually add some postal codes that were in stakeholders’
datasets but are not in our LDU shapefile. These may be new postal codes
not yet represented in the LDU data, or they may be older decommissioned
postal codes that are still in address data sets.

Results are saved as .csv files in the folder `results/`, and several
diagnostic images are saved in the folder `results/images/`.

Two considerations:

1.  By using residential area instead of total land area, we hope to
    distribute populations more accurately between neighbourhoods.
2.  By normalizing our intersection percentages to intersecting area,
    rather than total area, we will ensure that all Ottawa residents are
    counted completely. (In other words, if a postal code is only
    partially inside Ottawa, any Ottawa resident in that postal code
    will be completely attributed to one or more ONS neighbourhoods.)

# Code

This project is written in R using the package `targets` for a
reproducible workflow. All steps described above are outlined in the
file `_targets.R`.

# Note on Proprietary Data

**Please note** that the code in this repository requires non-public
proprietary data files in order to work. Specifically, we’re not able to
share the postal code shapefile due to copyright.
