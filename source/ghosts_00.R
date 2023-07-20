library(here)
library(tidyverse)
library(ambient)

prefix <- "ghosts"
version <- "00"
output <- "png"

grain <- 5000
g <- long_grid(
  x = seq(0, 1, length.out = grain),
  y = seq(0, 1, length.out = grain)
)

g$a <- fracture(
  noise = gen_worley,
  fractal = ridged,
  octaves = 2,
  frequency = 3,
  value = "distance2",
  seed = 10,
  x = g$x,
  y = g$y
)

g$a <- normalise(g$a)

curled <- fracture(
  noise = curl_noise,
  generator = gen_worley,
  fractal = fbm,
  octaves = 10,
  frequency = 2,
  seed = 6,
  value = "distance2",
  x = g$a + g$x,
  y = g$a + g$y
)

#g$b <- normalise(-g$b)

g$c <- fracture(
  noise = gen_simplex,
  fractal = billow,
  octaves = 10,
  frequency = .1,
  seed = 4,
  x = normalize(curled$x),
  y = normalize(curled$y)
)




g$i <- round(1 + (grain - 1) * normalise(g$c))
#palette <- scico::scico(n = grain, palette = "oslo")
pal_fn <- jasmines::palette_adjust(
	"grayC", NULL, red.f = .7, green.f = .7)
palette <- pal_fn(n = grain)
g$color <- palette[g$i]

rast <- as.raster(g, value = color)

png(
  filename = here("image", paste0(prefix, "_", version, ".", output)),
  width = grain,
  height = grain,
)
op <- par(mar = c(0,0,0,0))
plot(rast)
dev.off()
par(op)
