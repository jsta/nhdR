
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
#> 1              51772167 2002-09-15 01:00:00          2     <NA>
#> 2              46565431 2002-08-22 01:00:00          2     <NA>
#> 3              51767181 2002-09-15 01:00:00          2     <NA>
#> 4              51767223 2002-09-15 01:00:00          2     <NA>
#> 5              51767287 2002-09-15 01:00:00          2     <NA>
#> 6              51767709 2002-09-15 01:00:00          2     <NA>
#> 7              51768273 2002-09-15 01:00:00          2     <NA>
#> 8              51768749 2002-09-15 01:00:00          2     <NA>
#> 9              51768883 2002-09-15 01:00:00          2     <NA>
#> 10             51768891 2002-09-15 01:00:00          2     <NA>
#> 11            120022347 2004-04-16 14:37:51          2 01712592
#> 12             51768975 2002-09-15 01:00:00          2     <NA>
#> 13             51769061 2002-09-15 01:00:00          2     <NA>
#> 14             51769153 2002-09-15 01:00:00          2     <NA>
#> 15             51769369 2002-09-15 01:00:00          2     <NA>
#> 16             51769411 2002-09-15 01:00:00          2     <NA>
#> 17             51769545 2002-09-15 01:00:00          2     <NA>
#> 18             51769861 2002-09-15 01:00:00          2     <NA>
#> 19             51769871 2002-09-15 01:00:00          2     <NA>
#> 20             51770521 2002-09-15 01:00:00          2     <NA>
#>       GNIS_Name AreaSqKm Elevation      ReachCode FType FCode Shape_Length
#> 1          <NA>    0.005        NA 02070008004787   436 43624 0.0032275838
#> 2          <NA>    0.000        NA 02070010004605   436 43624 0.0005402029
#> 3          <NA>    0.002        NA 02070008004808   390 39004 0.0017289109
#> 4          <NA>    0.001        NA 02070008004829   390 39004 0.0013369633
#> 5          <NA>    0.001        NA 02070008004860   390 39004 0.0011083831
#> 6          <NA>    0.002        NA 02070008005063   390 39004 0.0016429957
#> 7          <NA>    0.001        NA 02070008005335   390 39004 0.0012442057
#> 8          <NA>    0.001        NA 02070008005570   390 39004 0.0013918440
#> 9          <NA>    0.001        NA 02070008005638   390 39004 0.0021229169
#> 10         <NA>    0.002        NA 02070008005641   390 39004 0.0018972528
#> 11 Clopper Lake    0.218        NA 02070008005635   390 39004 0.0365330031
#> 12         <NA>    0.001        NA 02070008005681   390 39004 0.0017444391
#> 13         <NA>    0.002        NA 02070008005722   390 39004 0.0018785786
#> 14         <NA>    0.005        NA 02070008005764   390 39004 0.0029191491
#> 15         <NA>    0.006        NA 02070008005870   390 39004 0.0035214462
#> 16         <NA>    0.000        NA 02070008005892   390 39004 0.0009224733
#> 17         <NA>    0.005        NA 02070008005954   390 39004 0.0028753323
#> 18         <NA>    0.002        NA 02070008006103   390 39004 0.0019580771
#> 19         <NA>    0.001        NA 02070008006108   390 39004 0.0013311586
#> 20         <NA>    0.023      80.8 02070008006400   390 39009 0.0072444361
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

Unlike the standard NHD, the NHD-Plus exports are organized by vector processing unit (vpu). A vpu map can be found [here](http://www.horizon-systems.com/NHDPlus/NHDPlusV2_data.php).

``` r
# get a vpu export
nhd_plus_get(vpu = 4)
```

``` r
# list layers
nhd_plus_list(vpu = 4)
#> [1] "NHDAreaEventFC.shp"  "NHDArea.shp"         "NHDFlowline.shp"    
#> [4] "NHDLineEventFC.shp"  "NHDLine.shp"         "NHDPointEventFC.shp"
#> [7] "NHDPoint.shp"        "NHDWaterbody.shp"
```

``` r
# get layer info
nhd_plus_info(vpu = 4, "NHDWaterbody")
```

    #>  [1] "Driver: ESRI Shapefile; number of rows: 31830 "     
    #>  [2] "Feature type: wkbPolygon with 3 dimensions"         
    #>  [3] "Extent: (-93.24332 40.43575) - (-73.61814 48.11344)"
    #>  [4] "CRS: +proj=longlat +datum=NAD83 +no_defs  "         
    #>  [5] "LDID: 87 "                                          
    #>  [6] "Number of fields: 12 "                              
    #>  [7] "         name type length typeName"                 
    #>  [8] "1       COMID    0      9  Integer"                 
    #>  [9] "2       FDATE    9     10     Date"                 
    #> [10] "3  RESOLUTION    4      7   String"                 
    #> [11] "4     GNIS_ID    4     10   String"                 
    #> [12] "5   GNIS_NAME    4     65   String"                 
    #> [13] "6    AREASQKM    2     19     Real"                 
    #> [14] "7   ELEVATION    2     19     Real"                 
    #> [15] "8   REACHCODE    4     14   String"                 
    #> [16] "9       FTYPE    4     24   String"                 
    #> [17] "10      FCODE    0      9  Integer"                 
    #> [18] "11 SHAPE_LENG    2     19     Real"                 
    #> [19] "12 SHAPE_AREA    2     19     Real"

``` r
# load layer
dt <- nhd_plus_load(vpu = 4, "NHDWaterbody")
#> Reading layer `NHDWaterbody' from data source `/home/jose/.local/share/nhdR/NHDPlus/GL_04_NHDSnapshot/NHDWaterbody.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 31830 features and 12 fields
#> geometry type:  POLYGON
#> dimension:      XYZ
#> bbox:           xmin: -93.24332 ymin: 40.43575 xmax: -73.61814 ymax: 48.11344
#> epsg (SRID):    4269
#> proj4string:    +proj=longlat +datum=NAD83 +no_defs
```
