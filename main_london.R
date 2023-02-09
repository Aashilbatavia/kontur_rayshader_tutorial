library(sf)
library(rnaturalearth)
library(stars)
library(rayshader)
library(tidyverse)

# library(tigris) # for us states

# library(MetBrewer)
# library(colorspace)

# load kontur data
data <- st_read("data/kontur_population_GB_20220630.gpkg")

# load country/city/regions of interest
# uk <- st_as_sf(countries110) %>%  
#   filter(ADMIN == "United Kingdom") # can use this to get country but below has more control
london <- ne_states(country = "United Kingdom", geounit = "england", returnclass = "sf") %>% 
  filter(region == "Greater London") %>% 
  st_transform(crs = st_crs(data)) # make coordinate reference system the same as data (needed for intersection with data to filter polygons)

london %>% 
  ggplot()+
  geom_sf()

# intersect data with regions on interest
st_london <- st_intersection(data, london)

# define aspect ratio based on bounding box needed for render
bb <- st_bbox(st_london)

bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]]))  %>%  
  st_sfc(crs = st_crs(data))                                # get points for wdith and height calculation and make it a spacial objecr with st_sfc
bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) %>%  
  st_sfc(crs = st_crs(data))
top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) %>% 
  st_sfc(crs = st_crs(data))

# check by plotting points
london %>% 
  ggplot() +
  geom_sf() +
  geom_sf(data = bottom_left) +
  geom_sf(data = bottom_right, color = "red") +
  geom_sf(data = top_left, color = "blue")

width <- st_distance(bottom_left, bottom_right) # diff between black and red point
height <- st_distance(bottom_left, top_left) # diff between black and blue point

# handle conditions of width or height being the longer side
if (width > height) {
  w_ratio <- 1
  h_ratio <- height / width
} else {
  h_ration <- 1
  w_ratio <- width / height
}


# convert to raster so we can then convert to matrix
size <- 1000

london_rast <- st_rasterize(st_london, 
                             nx = floor(size * w_ratio),
                             ny = floor(size * h_ratio))

mat <- matrix(london_rast$population, 
              nrow = floor(size * w_ratio),
              ncol = floor(size * h_ratio))

# create color palette

c1 <- met.brewer("OKeeffe2")
swatchplot(c1)

texture <- grDevices::colorRampPalette(c1, bias = 2)(256)
swatchplot(texture)

# plot that 3d thing!

rgl::close3d()

mat %>% 
  height_shade() %>% 
  plot_3d(heightmap = mat,
          zscale = 100 / 5,
          solid = FALSE,
          shadowdepth = 0)

render_camera(theta = -20, phi = 45, zoom = .8)

outfile <- "images/final_plot.png"

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if (!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  render_highquality(
    filename = outfile,
    interactive = FALSE,
    lightdirection = 280,
    lightaltitude = c(20, 80),
    lightcolor = c(c1[2], "white"),
    lightintensity = c(600, 100),
    samples = 450,
    width = 6000,
    height = 6000
  )
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}


a