# osbng 0.2.0

* New `geom_to_bng_intersection_explode()` simplifies exploding and flattening features to create a spatial data frame. `sf` package is required for this functionality.
* New `bng_grid_<resolution>()` family added to create spatial data frames of `BNGReference` features. `sf` package is required for this functionality.
* New vignette with overview on basic BNG interaction added.
* Updated vignette on indexing examples includes `geom_to_bng_intersection_explode()`. 
* `bbox_to_bng()` now optionally accepts a `data.frame` input with named columns.
* `geom_to_bng()` and `geom_to_bng_intersection()` accept `sfc` objects as inputs.
* Updated website and package description.
* Adds a Citation File Format (CFF) file. 

# osbng 0.1.0

* Initial release of `osbng` establishing custom `BNGRference`-type objects with support for conversion, indexing, and traversal.
* Establish MIT license.
* Website and vignettes set up.
