data(GNI2014, package = "treemap")

hcmap(
  "custom/world-robinson-lowres",
  data = data,
  name = "Number of institutions",
  value = "num_universidades",
  borderWidth = 0,
  nullColor = "#d3d3d3"
) %>% 
  hc_colorAxis(
    stops = color_stops(colors = viridisLite::inferno(10, begin = 0.1)),
    type = "logarithmic"
  )
