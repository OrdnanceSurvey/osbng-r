
<!-- README.md is generated from README.Rmd. Please edit that file -->

# osbng: British National Grid References in R

An R library for Ordnance Survey’s British National Grid (BNG) index
system. This library provides tools for working with the BNG, a
rectangular Cartesian grid system used to identify and index locations
across Great Britain into grid squares at various resolutions.

## Overview

The osbng R package provides a programmatic interface to the British
National Grid (BNG), supporting efficient grid-based indexing and
spatial analysis. This enables applications such as statistical
aggregation, data visualisation, and data interoperability using BNG
grid references. Designed for developers working with geospatial data in
the context of Great Britain, the library offers tools to simplify
working with the BNG, facilitating both technical integration into
geospatial workflows and practical exploration of the index system’s
structure.

The package supports the ‘standard’ BNG metre-based resolutions, which
represent powers of ten from 1m to 100km (1m, 10m, 100m, 1km, 10km,
100km). It also supports the ‘intermediate’ quadtree resolutions (5m,
50m, 500m, 5km, 50km), identified by an ordinal (NE, SE, SW, NW) BNG
reference direction suffix.

## Installation

Install `osbng` in R directly from CRAN:

``` r
install.packages("osbng")
```

Alternatively, you can install the latest version from
[GitHub](https://github.com/OrdnanceSurvey/osbng-r) with:

``` r
remotes::install_github("OrdnanceSurvey/osbng-r")
```

Once installed, load the package to use it:

``` r
library(osbng)
```

### Complimetary tools

- See the Python version:
  [`osbng-py`](https://github.com/OrdnanceSurvey/osbng-py)
- BNG grid data in GeoParquet and GeoPackage formats:
  [`osbng-grids`](https://github.com/OrdnanceSurvey/osbng-grids)

## Usage

The `osbng` package supports different interactions with the BNG index
system (e.g. indexing, hierarchy, traversal). A high-level summary of
each task is provided below:

### BNG Reference

`osbng` implements a custom BNG reference object, `BNGReference`. The
`BNGReference` object validates the reference and provides access to
functions to work with the reference.

``` r
library(osbng)

bng_ref <- as_bng_reference("ST57SE")

print(bng_ref)
#> <BNGReference[1] with Resolution=5km>
#> [1] <ST 5 7 SE>

get_bng_resolution(bng_ref)
#> [1] 5000

get_bng_resolution_string(bng_ref)
#> [1] "5km"
```

### Indexing

Provides the ability to index and work with coordinates and geometries
against the BNG index system. This includes:

- Encoding easting and northing coordinates into `BNGReference` objects
  at a specified resolution.
- Decoding `BNGReference` objects back into coordinates, bounding boxes
  and grid squares as [`geos`](https://cran.r-project.org/package=geos)
  geometries.
- Indexing bounding boxes and `geos` geometries into grid squares at a
  specified resolution for spatial analysis.

The following example demonstrates a round trip of constructing a
`BNGReference` object from easting and northing coordinates, and then
decoding back into coordinates and a bounding box:

``` r
bng_ref <- xy_to_bng(easting = 356976, northing = 171421, resolution = "5km")

bng_to_xy(bng_ref, position = "lower-left")
#>        [,1]   [,2]
#> [1,] 355000 170000

bng_to_bbox(bng_ref)
#>        [,1]   [,2]   [,3]   [,4]
#> [1,] 355000 170000 360000 175000
```

### Hierarchy

Provides functionality to navigate the hierarchical structure of the BNG
index system. This includes:

- Returning parents and children of `BNGReference` objects at specified
  resolutions.

The following example demonstrates the construction of a `BNGReference`
object from a reference string and returns a parent of the
`BNGReference`:

``` r
bng_ref <- as_bng_reference("ST5671SE")

bng_to_parent(bng_ref, resolution = "10km")
#> <BNGReference[1] with Resolution=10km>
#> [1] <ST 5 7>
```

## Contributing

Please raise an issue to discuss features, bugs, or general questions.

## License

`osbng-r` is licensed under the terms of the MIT License.
