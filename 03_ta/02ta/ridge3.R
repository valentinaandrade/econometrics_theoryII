# Identificar los nombres de países únicos
nombres_unicos <- unique(data$country)

# Crear una nueva variable en data que contenga los nombres de países únicos
data$country_unico <- data$country

# Asignar los nombres de países únicos a las filas correspondientes
data$country_unico[duplicated(data$country)] <- NA

data <- data %>% 
  group_by(country) %>%
  mutate(num_universidades = n_distinct(university))


df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv') %>% 
  mutate(country = COUNTRY)

df <- left_join(data, df) %>% 
  mutate(num_uni = if_else(is.na(country_unico), NA_real_, num_universidades))

# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

fig <- plot_geo(df)

fig <- fig %>% add_trace(
  z = ~num_uni, color = ~num_uni, colors = 'Blues',
  text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
)
fig <- fig %>% colorbar(title = 'GDP Billions US$', tickprefix = '$')
fig <- fig %>% layout(
  title = '2014 Global GDP<br>Source:<a href="https://www.cia.gov/library/publications/the-world-factbook/fields/2195.html">CIA World Factbook</a>',
  geo = g
)

fig
