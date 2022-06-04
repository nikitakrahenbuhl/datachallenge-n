



zoom2n <- function(zoom){2.0 ^ zoom}

# Turns WGS84 lat/long coordinates into raster tile ids
# https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#R
lon2xtile <- function(lon_deg, zoom){
  n <- zoom2n(zoom)
  floor((lon_deg + 180.0) / 360.0 * n)
}

lat2ytile <- function(lat_deg, zoom){
  n <- zoom2n(zoom)
  lat_rad <- lat_deg * pi /180
  floor((1.0 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2.0 * n)
}

# Turns raster tile ids into WGS84 lat/long coordinates
# https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#R
xtile2lon <- function(xtile, zoom){
  n <- zoom2n(zoom)
  xtile / n * 360.0 - 180.0
}

# Turns raster tile ids into WGS84 lat/long coordinates
# https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#R
ytile2lat <- function(ytile, zoom){
  n <- zoom2n(zoom)
  lat_rad = atan(sinh(pi * (1 - 2 * ytile / n)))
  lat_rad * 180.0 / pi
}

# A wrapper around a function which checks whether a specific file exists, and
#  if so if it should be overwritten before running another function.
doif <- function(filename, overwrite, fun){
  if(!file.exists(filename)){
    fun
  } else{
    if(overwrite){
      fun
      message(paste(filename),"already exists. It will be overwritten since overwrite = ",overwrite)
    } else{
      message(paste(filename, "already exists. It will NOT be overwritten since overwrite =",overwrite))
    }
  }
}



#' A function to download strava tiles and combine the tiles into a single image
#'
#' "https://heatmap-external-b.strava.com/tiles/run/hot/10/536/356@2x.png?v=19" # Schaffhausen
#' "https://heatmap-external-b.strava.com/tiles/run/hot/10/529/363@2x.png?v=19" # Genf
#' "https://heatmap-external-b.strava.com/tiles/run/hot/10/539/362@2x.png?v=19" # Graubünden

#' https://heatmap-external-b.strava.com/tiles/run/hot/10/539/362@2x.png?v=19
#'                                              |   |   |  |   |-> north < south
#'                                              |   |   |  |-> west < east
#'                                              |   |   |-> zoom?
#'                                              |   |-> color (hot/blue/purple/gray/bluered)
#'                                              |-> activity (all/ride/water/run)
#'
#' @param zoom the zoom level (1 to 14, implemented only till 11)
#' @param north, south, west, east tiles
#' @param colour colorband to use (hot, blue, urple, grey or bluered)
#' @param activity activity to use (all ride, water or run)
#' @param download should the tile be downloaded (FALSE is a dry run)
#' @param resolution resolution of the tile ("" or "@2x")
#' @param sleep duration of sleep between downloads
#' @param tempdir temporary directory to store the downloads
#' @param overwrite should existing files be overwritten (ignored for downloads)
#' @param output_dir where should the finished tiles be stored?
strava_download_combine <- function(zoom,
                                    north,
                                    south,
                                    west,
                                    east,
                                    colour,
                                    activity = "all",
                                    download = TRUE,
                                    resolution = "",
                                    sleep = 0.5,
                                    tempdir = getwd(),
                                    overwrite = FALSE,
                                    output_dir = getwd()){
  require(purrr)
  require(glue)
  require(magick)

  total_tiles <- length(north:south)*length(west:east)

  basedir <- glue(file.path(tempdir,"strava_{activity}_{zoom}_{colour}"))
  tiles_dir <- file.path(basedir,"tiles")
  stripes_dir <- file.path(basedir,"stripes")
  combined_dir <- file.path(basedir,"combined")


  if(!dir.exists(basedir)){
    dir.create(basedir)
    dir.create(tiles_dir)
    dir.create(stripes_dir)
    dir.create(combined_dir)

  }

  stripes_all <- imap_chr(west:east,function(easting,e_nr){
    stripe_names <- imap_chr(north:south, function(northing,n_nr){
      resname <- ifelse(resolution == "","", glue("_{resolution}"))

      fi <- file.path(tiles_dir,glue("{easting}_{northing}{resname}.png"))

      if(!file.exists(fi) & download){
        ur <- glue("https://heatmap-external-b.strava.com/tiles/{activity}/{colour}/{zoom}/{easting}/{northing}{resolution}.png?v=19")
        download.file(ur,fi,mode = "wb", quiet = TRUE)
        Sys.sleep(sleep)
      }
      print(((e_nr-1)*length(north:south)+n_nr)/total_tiles)
      fi
    })

    stripe_name <- file.path(stripes_dir,glue("{easting}.png"))

    doif(stripe_name, overwrite, stripe_names %>%
           image_read() %>%
           image_append(stack = TRUE) %>%
           image_write(stripe_name))

    return(stripe_name)
  })




  combined_filename <- file.path(combined_dir,"01_combined.png")

  doif(combined_filename, overwrite, stripes_all %>%
         image_read() %>%
         image_append(stack = FALSE) %>%
         image_write(combined_filename))


  return(combined_filename)
}


#' A function to reclassify the values in a raster file from
#' strava_download_combine() based on the "L" value
#' @param filename_in, filename_out the name of the input / output filename
#' @param viz should the colorpalette be visualized (debuggin)
#' @param overwrite should existing files be overwritten?
#'
#' @return character
#' @export
#'
strava_reclassify <- function(filename_in, filename_out, viz = FALSE, overwrite = FALSE){

  require(dplyr)

  strava_terra <- terra::rast(filename_in)
  my_colours_rgba <- terra::coltab(strava_terra)[[1]]
  lab <- convertColor(my_colours_rgba[,1:3], 'sRGB', 'Lab')
  neworder <- order(lab[, "L"])

  my_colours_hex <- rgb(my_colours_rgba,maxColor = 255)

  if(viz){
    require(scales)

    show_col(my_colours_hex,FALSE,NA)
    show_col(my_colours_hex[neworder],FALSE,NA)
  }


  col_df <- tibble(x = 0:255, col = my_colours_hex)
  col_df2 <- tibble(x_new = 0:255, col = my_colours_hex[neworder])

  # ggplot(col_df,aes(x = x, y = 1)) +
  #     geom_col(aes(fill = col),width = 1) +
  #     scale_fill_identity() +
  #     theme(legend.position = "none",axis.text.y = element_blank())


  recl_df <- full_join(col_df,col_df2, by = "col")

  rec <- as.matrix(recl_df[,c("x","x_new")])

  strava_terra_recl <- terra::classify(strava_terra,rec)

  strava_terra_recl[is.na(strava_terra_recl)] <- 0

  doif(filename_out,overwrite, terra::writeRaster(strava_terra_recl, filename_out,overwrite = overwrite))


  print(glue("{filename_out} done"))
  return(filename_out)
}


# Combine all steps to get strava raster data. In addition to downloading and
# reclassifying the data, this function
# 1. georeferences the raster data
# 2. reprojects it
# 3. crops it to our default extent and resamples it to our default res (100x100)
#' Title
#'
#' @param zoom zoom level (1-14)
#'
#' @return
#'
strava_get <- function(
  zoom,
  colour = "hot",
  activity = "all",
  download = TRUE,
  resolution = "",
  tempdir = getwd(),
  lat_min = 45.817959,
  lat_max = 47.808454,
  lon_min = 5.955902,
  lon_max = 10.492172,
  overwrite = FALSE,
  output_dir = getwd()
){
  require(sf)
  require(dplyr)
  require(terra)
  west <- lon2xtile(lon_min,zoom)
  east <- lon2xtile(lon_max,zoom)
  south <- lat2ytile(lat_min,zoom)
  north <- lat2ytile(lat_max,zoom)

  combined_file <- strava_download_combine(zoom = zoom, north = north, south = south, west = west, east = east,colour = colour, activity = activity, download = download, resolution = resolution,tempdir = tempdir, overwrite = overwrite,output_dir = output_dir)

  combined_path <- dirname(combined_file)

  reclassified_file <- file.path(combined_path, "02_reclassified.tif")
  strava_reclassify(filename_in = combined_file,filename_out = reclassified_file, overwrite = overwrite)
  lon_tile_min_wgs84 <- xtile2lon(west, zoom)
  lon_tile_max_wgs84 <- xtile2lon(east+1, zoom)

  lat_tile_max_wgs84 <- ytile2lat(north, zoom)
  lat_tile_min_wgs84 <- ytile2lat(south+1, zoom)


  extent_wgs84 <- expand.grid(lon = c(lon_tile_min_wgs84, lon_tile_max_wgs84), lat = c(lat_tile_min_wgs84, lat_tile_max_wgs84))


  extent_3857 <- sf::sf_project(st_crs(st_sfc(crs = 4326)),st_crs(st_sfc(crs = 3857)),extent_wgs84)

  reclassified_rast <- rast(reclassified_file)

  crs(reclassified_rast) <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"

  ext(reclassified_rast) <- c(extent_3857[1,1],extent_3857[2,1],extent_3857[1,2],extent_3857[3,2])


  georeferenced_file <- file.path(combined_path, "03_georeferenced.tif")

  doif(georeferenced_file, overwrite, terra::writeRaster(reclassified_rast, georeferenced_file,overwrite = overwrite))

  # if(!file.exists(georeferenced_file) | overwrite){
  #   terra::writeRaster(reclassified_rast, georeferenced_file)
  # }

  georeferenced_rast <- rast(georeferenced_file)

  epsg2056 <- "+proj=somerc +lat_0=46.95240555555556 +lon_0=7.439583333333333 +k_0=1 +x_0=2600000 +y_0=1200000 +ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs"

  projected_file <- file.path(combined_path, "04_projected.tif")
  projected_rast <- terra::project(georeferenced_rast,epsg2056)

  doif(projected_file, overwrite, writeRaster(projected_rast, projected_file, overwrite = overwrite))

  template <- terra::rast(resolution = 100, extent = c(2485500, 2833800, 1075300, 1295900),crs = epsg2056)

  cropped_rast <- terra::crop(projected_rast, ext(template))
  cropped_file <- file.path(combined_path, "05_cropped.tif")
  # if(!file.exists(cropped_file) | overwrite){
  #   terra::writeRaster(cropped_rast, cropped_file)
  # }

  doif(cropped_file, overwrite, terra::writeRaster(cropped_rast, cropped_file, overwrite = overwrite))


  resampled_rast <- terra::resample(cropped_rast,template)
  resampled_file <- file.path(output_dir, glue("strava_{activity}_zoom_{str_pad(zoom,2,pad = '0')}.tif"))

  # if(!file.exists(resampled_file) | overwrite){
  #   terra::writeRaster(resampled_rast, resampled_file)
  # }

  doif(resampled_file, overwrite, terra::writeRaster(resampled_rast, resampled_file,overwrite = overwrite))

}



