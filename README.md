
<!-- README.md is generated from README.Rmd. Please edit that file -->
nhdR
====

The goal of nhdR is to provide R tools for interacting with the National Hydrography Dataset.

Installation
------------

You can install nhdR from github with:

``` r
# install.packages("devtools")
devtools::install_github("jsta/nhdR")
```

`nhdR` expects the NHD Plus `.gdb` file to be located at the location returned by `gdb_plus_path()`. This is ultimately a function of `rappdirs::user_data_dir`. On a Linux machine this will be something like:

`~/.local/share/nhdR/NHDPlusV21_NationalData_National_Seamless_Geodatabase_02/NHDPlusNationalData/NHDPlusV21_National_Seamless.gdb`

Getting the data to this location is a manual process for now in part because of the sizes of the files involved.

``` r
# Copy NHD Plus gdb file to:
nhdR:::gdb_plus_path()
```

Load package
------------

``` r
library(nhdR)
```

List layers
-----------

``` r
nhd_plus_list()
#>  [1] "Gage"                     "BurnAddLine"             
#>  [3] "BurnAddWaterbody"         "LandSea"                 
#>  [5] "Sink"                     "Wall"                    
#>  [7] "CatchmentSP"              "NHDArea"                 
#>  [9] "NHDWaterbody"             "NHDPlusComponentVersions"
#> [11] "PlusARPointEvent"         "PlusFlow"                
#> [13] "PlusFlowAR"               "NHDFCode"                
#> [15] "DivFracMP"                "BurnLineEvent"           
#> [17] "NHDFlowline_Network"      "NHDFlowline_NonNetwork"  
#> [19] "HUC12"                    "GeoNetwork_Junctions"    
#> attr(,"driver")
#> [1] "OpenFileGDB"
#> attr(,"nlayers")
#> [1] 20
```

Get layer info
--------------

``` r
nhd_plus_info("NHDWaterbody")
```

    #>  [1] "Driver: OpenFileGDB; number of rows: 448512 "                     
    #>  [2] "Feature type: wkbPolygon with 2 dimensions"                       
    #>  [3] "Extent: (-124.6899 24.52142) - (-66.99787 49.38433)"              
    #>  [4] "CRS: +proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs  "
    #>  [5] "Number of fields: 15 "                                            
    #>  [6] "           name type length typeName"                             
    #>  [7] "1         COMID    0      0  Integer"                             
    #>  [8] "2         FDATE   11      0 DateTime"                             
    #>  [9] "3    RESOLUTION    4      7   String"                             
    #> [10] "4       GNIS_ID    4     10   String"                             
    #> [11] "5     GNIS_NAME    4     65   String"                             
    #> [12] "6      AREASQKM    2      0     Real"                             
    #> [13] "7     ELEVATION    2      0     Real"                             
    #> [14] "8     REACHCODE    4     14   String"                             
    #> [15] "9         FTYPE    4     24   String"                             
    #> [16] "10        FCODE    0      0  Integer"                             
    #> [17] "11 Shape_Length    2      0     Real"                             
    #> [18] "12   Shape_Area    2      0     Real"                             
    #> [19] "13     ONOFFNET    0      0  Integer"                             
    #> [20] "14     PurpCode    4      2   String"                             
    #> [21] "15     PurpDesc    4    254   String"

Load layer
----------

``` r
dt <- nhd_plus_load("NHDWaterbody")
#> Reading layer `NHDWaterbody' from data source `/home/jose/.local/share/nhdR/NHDPlusV21_NationalData_National_Seamless_Geodatabase_02/NHDPlusNationalData/NHDPlusV21_National_Seamless.gdb' using driver `OpenFileGDB'
#> Simple feature collection with 448512 features and 15 fields
#> geometry type:  MULTIPOLYGON
#> dimension:      XY
#> bbox:           xmin: -124.6899 ymin: 24.52142 xmax: -66.99787 ymax: 49.38433
#> epsg (SRID):    4269
#> proj4string:    +proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs
```
