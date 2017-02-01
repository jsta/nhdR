
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

Usage
-----

### Load package

``` r
library(nhdR)
#> Loading required package: maps
```

### NHD

``` r
nhd_get(state = c("DC", "HI"))
```

``` r
nhd_list(state = "DC")
#>  [1] "ExternalCrosswalk"           "NHDFCode"                   
#>  [3] "NHDFeatureToMetadata"        "NHDFlow"                    
#>  [5] "NHDFlowlineVAA"              "NHDMetadata"                
#>  [7] "NHDProcessingParameters"     "NHDReachCodeMaintenance"    
#>  [9] "NHDReachCrossReference"      "NHDSourceCitation"          
#> [11] "NHDStatus"                   "NHDVerticalRelationship"    
#> [13] "NHDPoint"                    "NHDFlowline"                
#> [15] "NHDLine"                     "NHDArea"                    
#> [17] "NHDWaterbody"                "NHDAreaEventFC"             
#> [19] "NHDLineEventFC"              "NHDPointEventFC"            
#> [21] "WBDLine"                     "NonContributingDrainageArea"
#> [23] "NWISBoundary"                "NWISDrainageArea"           
#> [25] "WBDHU14"                     "WBDHU8"                     
#> [27] "WBDHU2"                      "WBDHU4"                     
#> [29] "WBDHU6"                      "WBDHU10"                    
#> [31] "WBDHU12"                     "WBDHU16"                    
#> [33] "HYDRO_NET_Junctions"        
#> attr(,"driver")
#> [1] "OpenFileGDB"
#> attr(,"nlayers")
#> [1] 33
```

``` r
nhd_info(state = "DC", layer_name = "NHDWaterbody")
#> Source: "/home/jose/.local/share/nhdR/NHDH_DC.gdb", layer: "NHDWaterbody"
#> Driver: OpenFileGDB; number of rows: 8025 
#> Feature type: wkbPolygon with 3 dimensions
#> Extent: (-78.07095 38.52142) - (-76.82219 39.64683)
#> CRS: +proj=longlat +datum=NAD83 +no_defs  
#> Number of fields: 12 
#>                    name type length typeName
#> 1  Permanent_Identifier    4     40   String
#> 2                 FDate   11      0 DateTime
#> 3            Resolution    0      0  Integer
#> 4               GNIS_ID    4     10   String
#> 5             GNIS_Name    4     65   String
#> 6              AreaSqKm    2      0     Real
#> 7             Elevation    2      0     Real
#> 8             ReachCode    4     14   String
#> 9                 FType    0      0  Integer
#> 10                FCode    0      0  Integer
#> 11         Shape_Length    2      0     Real
#> 12           Shape_Area    2      0     Real
```

``` r
nhd_load(state = "DC", layer_name = "NHDWaterbody")
#> Reading layer `NHDWaterbody' from data source `/home/jose/.local/share/nhdR/NHDH_DC.gdb' using driver `OpenFileGDB'
#> Simple feature collection with 8025 features and 12 fields
#> geometry type:  MULTIPOLYGON
#> dimension:      XYZ
#> bbox:           xmin: -78.07095 ymin: 38.52142 xmax: -76.82219 ymax: 39.64683
#> epsg (SRID):    4269
#> proj4string:    +proj=longlat +datum=NAD83 +no_defs
#> Simple feature collection with 8025 features and 12 fields
#> geometry type:  MULTIPOLYGON
#> dimension:      XYZ
#> bbox:           xmin: -78.07095 ymin: 38.52142 xmax: -76.82219 ymax: 39.64683
#> epsg (SRID):    4269
#> proj4string:    +proj=longlat +datum=NAD83 +no_defs
#> First 20 features:
#>    Permanent_Identifier               FDate Resolution  GNIS_ID
#> 1              51772167 2002-09-15 01:00:00          2         
#> 2              46565431 2002-08-22 01:00:00          2         
#> 3              51767181 2002-09-15 01:00:00          2         
#> 4              51767223 2002-09-15 01:00:00          2         
#> 5              51767287 2002-09-15 01:00:00          2         
#> 6              51767709 2002-09-15 01:00:00          2         
#> 7              51768273 2002-09-15 01:00:00          2         
#> 8              51768749 2002-09-15 01:00:00          2         
#> 9              51768883 2002-09-15 01:00:00          2         
#> 10             51768891 2002-09-15 01:00:00          2         
#> 11            120022347 2004-04-16 14:37:51          2 01712592
#> 12             51768975 2002-09-15 01:00:00          2         
#> 13             51769061 2002-09-15 01:00:00          2         
#> 14             51769153 2002-09-15 01:00:00          2         
#> 15             51769369 2002-09-15 01:00:00          2         
#> 16             51769411 2002-09-15 01:00:00          2         
#> 17             51769545 2002-09-15 01:00:00          2         
#> 18             51769861 2002-09-15 01:00:00          2         
#> 19             51769871 2002-09-15 01:00:00          2         
#> 20             51770521 2002-09-15 01:00:00          2         
#>       GNIS_Name AreaSqKm Elevation      ReachCode FType FCode Shape_Length
#> 1                  0.005       0.0 02070008004787   436 43624 0.0032275838
#> 2                  0.000       0.0 02070010004605   436 43624 0.0005402029
#> 3                  0.002       0.0 02070008004808   390 39004 0.0017289109
#> 4                  0.001       0.0 02070008004829   390 39004 0.0013369633
#> 5                  0.001       0.0 02070008004860   390 39004 0.0011083831
#> 6                  0.002       0.0 02070008005063   390 39004 0.0016429957
#> 7                  0.001       0.0 02070008005335   390 39004 0.0012442057
#> 8                  0.001       0.0 02070008005570   390 39004 0.0013918440
#> 9                  0.001       0.0 02070008005638   390 39004 0.0021229169
#> 10                 0.002       0.0 02070008005641   390 39004 0.0018972528
#> 11 Clopper Lake    0.218       0.0 02070008005635   390 39004 0.0365330031
#> 12                 0.001       0.0 02070008005681   390 39004 0.0017444391
#> 13                 0.002       0.0 02070008005722   390 39004 0.0018785786
#> 14                 0.005       0.0 02070008005764   390 39004 0.0029191491
#> 15                 0.006       0.0 02070008005870   390 39004 0.0035214462
#> 16                 0.000       0.0 02070008005892   390 39004 0.0009224733
#> 17                 0.005       0.0 02070008005954   390 39004 0.0028753323
#> 18                 0.002       0.0 02070008006103   390 39004 0.0019580771
#> 19                 0.001       0.0 02070008006108   390 39004 0.0013311586
#> 20                 0.023      80.8 02070008006400   390 39009 0.0072444361
#>      Shape_Area                          Shape
#> 1  5.164066e-07 MULTIPOLYGONZ(((-77.1138173...
#> 2  1.879174e-08 MULTIPOLYGONZ(((-76.9963107...
#> 3  1.954519e-07 MULTIPOLYGONZ(((-77.5694207...
#> 4  1.239613e-07 MULTIPOLYGONZ(((-77.5694721...
#> 5  8.130533e-08 MULTIPOLYGONZ(((-77.5764423...
#> 6  1.745505e-07 MULTIPOLYGONZ(((-77.4693297...
#> 7  8.126193e-08 MULTIPOLYGONZ(((-77.2087439...
#> 8  1.013940e-07 MULTIPOLYGONZ(((-77.2877509...
#> 9  1.504919e-07 MULTIPOLYGONZ(((-77.6757487...
#> 10 1.882746e-07 MULTIPOLYGONZ(((-77.7425967...
#> 11 2.269156e-05 MULTIPOLYGONZ(((-77.2433261...
#> 12 8.442752e-08 MULTIPOLYGONZ(((-77.3166851...
#> 13 2.154168e-07 MULTIPOLYGONZ(((-77.3468047...
#> 14 5.356300e-07 MULTIPOLYGONZ(((-77.1942657...
#> 15 6.044582e-07 MULTIPOLYGONZ(((-77.2561119...
#> 16 3.859390e-08 MULTIPOLYGONZ(((-77.1896931...
#> 17 5.001452e-07 MULTIPOLYGONZ(((-77.3589241...
#> 18 2.320902e-07 MULTIPOLYGONZ(((-77.2026457...
#> 19 1.289019e-07 MULTIPOLYGONZ(((-77.1975565...
#> 20 2.362901e-06 MULTIPOLYGONZ(((-77.1530661...
```

### NHD Plus

By default, `nhdR` expects the NHD Plus `.gdb` file to be located at the location returned by `nhdR:::gdb_plus_path()`. Getting the data to this location is a manual process for now in part because of the sizes of the files involved. You can also manually specify a path to the `.gdb` file with the `fpath` argument.

``` r
# Copy the NHD Plus national coverage gdb file to:
nhdR:::gdb_plus_path()
```

``` r
# list layers
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

``` r
# get layer info
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

``` r
# load layer
dt <- nhd_plus_load("NHDWaterbody")
#> Reading layer `NHDWaterbody' from data source `/home/jose/.local/share/nhdR/NHDPlusV21_NationalData_National_Seamless_Geodatabase_02/NHDPlusNationalData/NHDPlusV21_National_Seamless.gdb' using driver `OpenFileGDB'
#> Simple feature collection with 448512 features and 15 fields
#> geometry type:  MULTIPOLYGON
#> dimension:      XY
#> bbox:           xmin: -124.6899 ymin: 24.52142 xmax: -66.99787 ymax: 49.38433
#> epsg (SRID):    4269
#> proj4string:    +proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs
```
