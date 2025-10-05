#' Fill Holes in Habitat Polygons
#'
#' This function removes interior holes from polygon features so that
#' they can be processed by SimStream-Web, which do not handle
#' polygons with holes correctly. The function also writes the resulting
#' polygon layer to a shapefile.
#'
#' @param poly_mesohabitats An \code{sf} object containing polygon geometries
#'   representing mesohabitats.
#' @param file_name Character string giving the output shapefile path.
#' @param threshold Numeric value giving the maximum area (in map units) of holes
#'   to fill. Defaults to 10,000 (map units)
#'
#' @return Invisibly returns the modified \code{sf} object (without holes).
#'
#' @details
#' Internally, this function uses \code{smoothr::fill_holes()} to remove holes
#' smaller than the specified threshold. The modified polygon is then written
#' to disk using \code{sf::st_write()} with \code{delete_layer = TRUE}.
#'
#' @examples
#' \dontrun{
#'   filled <- fill_holes(poly_mesohabitats, "mesohabitats_noholes.shp", threshold = 5000)
#' }
#'
#' @export

# save simstream ready shp
fill_holes <- function(poly_mesohabitats, file_name, threshold = 10000) {

  poly_mesohabitats <- smoothr::fill_holes(poly_mesohabitats,threshold) # holes in polygons are deteled, otherwise simstream cannot handle polygons

  sf::st_write(poly_mesohabitats,file_name,delete_layer=TRUE)

  invisible(poly_mesohabitats)

}
