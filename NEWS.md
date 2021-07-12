# nhdR 0.5.6

* CRAN maintenance release
* Added support for recent `sf` package versions depending on `s2` geometry

# nhdR 0.5.5

* Drop automatic conversion to Geopackage for NHD (non-plus) products
* Massive (~30x) speed improvement for `nhd_plus` query operations (#63)
* Bug fix for loading `NHDReachCrossReference` layers (#72)
* Bug fix to make `vpu_shp` geometries "valid" (#70)

# nhdR 0.5.4

* Update URL endpoint for NHDPlus (#64)
* Bug fix for `nhd_plus_get` unzipping to enable the `force_unzip` argument

# nhdR 0.5.3

* Update URL endpoint for NHDPlus
* Fix crs tests for upcoming sf release

# nhdR 0.5.2

* Bug fix for multi-vpu data loading with mismatched columns (#57)
* Bug fix for character VPUs that match multiple remote data sources (#58)
* Added package logo

# nhdR 0.5.1

* Critical bug fix for `nhd_get` recursive folder creation (# 56)
* Bug fixes for `extract_network` enabling propagation of `approve_all_dl` flag
* Improved documentation of non-geographic buffer extraction

# nhdR 0.5.0

* Improve `extract_network` to handle headwater lakes 
* Add function to return Great Lakes polygons
* Enable non-geographic query buffers
* Fix broken NHD download endpoint
* Bug fixes for `nhd_plus_query` on multi vpu queries

# nhdR 0.4

* Confirmed working operation on MacOS
* Results of load functions are now cached (memoized) for massive increase in package speed

# nhdR 0.3

* Added a `NEWS.md` file to track changes to the package.



