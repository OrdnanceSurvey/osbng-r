#' Region Boundaries for London
#'
#' This file (London_Regions_December_2024_Boundaries_EN_BFC.gpkg) contains the
#' digital vector boundaries for the London Region in England as at December
#' 2024. The boundaries available are: (BFC) Full resolution - clipped to the
#' coastline (Mean High Water mark). The coordinate reference system is British
#' National Grid (OSGB36; EPSG:27700). Contains both Ordnance Survey and ONS
#' Intellectual Property Rights. Licenced under the Open Government Licence
#' (OGLv3).
#' 
#' @name Regions
#' @docType data
#' @format GeoPackage with one layer, 1 rows and 8 columns:
#' \describe{
#'  \item{fid}{Row index.}
#'  \item{RGN24CD}{Character. Region GSS code.}
#'  \item{RGN24NM}{Character. Standardised Region name.}
#'  \item{BNG_E}{Centroid eastings in British National Grid.}
#'  \item{BNG_N}{Centroid northings in British National Grid.}
#'  \item{LONG}{Centroid longitude coordinate.}
#'  \item{LAT}{Centroid latitude coordinate.}
#'  \item{GlobalID}{Database management.}
#' }
#' @keywords data
#' @source Office for National Statistics (ONS), released 16 February 2025,
#'   Regions (December 2024) Boundaries EN BFC, [ONS Open Geography
#'   Portal](https://geoportal.statistics.gov.uk/).
NULL
