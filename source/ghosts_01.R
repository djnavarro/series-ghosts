library(here)
library(tidyverse)
library(ambient)

seed <- 170:199

ghosts <- function(seed) {

  prefix <- "ghosts"
  version <- "01"
  output <- "png"
  cat(seed, "\n")

  set.seed(seed)

  cells_wide <- 2000
  cells_high <- 2000
  n_colours <- 1000
  rescale <- 1

  mesh <- long_grid(
    x = seq(0, 1, length.out = cells_wide),
    y = seq(0, 1, length.out = cells_high)
  )


  # generating noise --------------------------------------------------------

  cat("making noise")

  mesh$a <- fracture(
    noise = gen_worley,
    fractal = ridged,
    octaves = 4,
    frequency = 3,
    value = "distance2",
    seed = sample(100, 1),
    x = mesh$x,
    y = mesh$y
  )

  cat(".")

  mesh$a <- normalise(mesh$a)

  curled <- fracture(
    noise = curl_noise,
    generator = gen_worley,
    fractal = fbm,
    octaves = 10,
    frequency = 2,
    seed = sample(100, 1),
    value = "distance2",
    x = mesh$a + mesh$x,
    y = mesh$a + mesh$y
  )

  cat(".")

  mesh$c <- fracture(
    noise = gen_simplex,
    fractal = billow,
    octaves = 10,
    frequency = .1,
    seed = sample(100, 1),
    x = normalize(curled$x),
    y = normalize(curled$y)
  )

  cat(".\n")


  # construct raster --------------------------------------------------------

  cat("making raster...\n")


  mesh$i <- round(1 + (n_colours - 1) * normalise(mesh$c))
  # pal_fn <- jasmines::palette_adjust(
  # 	"grayC", NULL, red.f = .7, green.f = .7)
  pal_fn <- colorRampPalette(sample(colours(distinct = TRUE), 4))
  palette <- pal_fn(n = n_colours)
  mesh$color <- palette[mesh$i]

  rast <- as.raster(mesh, value = color)



  # write image -------------------------------------------------------------

  cat("writing image...\n")

  pixels_wide <- cells_wide * rescale
  pixels_high <- cells_high * rescale

  png(
    filename = here("image", paste0(prefix, "_", version, "_", seed, ".", output)),
    width = pixels_wide,
    height = pixels_high,
  )
  op <- par(mar = c(0,0,0,0))
  plot(rast)
  dev.off()
  par(op)

}

for(s in seed) ghosts(s)
