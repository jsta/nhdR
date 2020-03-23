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



