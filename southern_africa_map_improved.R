# Southern Africa Map with Improved Urban Insets and Full Partner Names
# Load required libraries
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dplyr)
library(readr)
library(ggspatial)      # For scale bar and north arrow
library(cowplot)        # For publication-quality plots
library(ggrepel)        # For non-overlapping labels

# Read the data
base_data <- read.csv('base.csv', sep = ';', stringsAsFactors = FALSE)

# Define Southern African countries
southern_africa_countries <- c(
  'South Africa', 'Namibia', 'Botswana', 'Zimbabwe', 'Mozambique', 
  'Lesotho', 'Eswatini', 'Zambia', 'Malawi', 'Angola'
)

# Filter data for Southern Africa
southern_africa_data <- base_data %>%
  filter(Country %in% southern_africa_countries)

# Create a new column for focus area type (for coloring)
# Priority: Research > Policy > Engagement > Finance
southern_africa_data$FocusType <- NA
southern_africa_data$FocusType[southern_africa_data$Research == 1] <- 'Research'
southern_africa_data$FocusType[southern_africa_data$Policy == 1 & is.na(southern_africa_data$FocusType)] <- 'Policy'
southern_africa_data$FocusType[southern_africa_data$Engagement..Advocacy..and.Capacity.Building == 1 & is.na(southern_africa_data$FocusType)] <- 'Engagement, Advocacy and Capacity Building'
southern_africa_data$FocusType[southern_africa_data$Finance_programmes == 1 & is.na(southern_africa_data$FocusType)] <- 'Finance and Programmes'

# Remove rows with NA focus type
southern_africa_data <- southern_africa_data %>%
  filter(!is.na(FocusType))

# Create a shape column for Data Providers
southern_africa_data$Shape <- ifelse(southern_africa_data$Data_Providers == 1, 17, 16)

# Identify major partners using full partner names (Institution column)
southern_africa_data <- southern_africa_data %>%
  mutate(
    focus_count = Research + Policy + Engagement..Advocacy..and.Capacity.Building + Finance_programmes,
    is_major_partner = ifelse(Official.Partners == 1 & (focus_count > 1 | Data_Providers == 1), TRUE, FALSE)
  )

# Get world map data and cities data (for urban insets)
world <- ne_countries(scale = 'medium', returnclass = 'sf')
afrika <- world %>% filter(continent == 'Africa')
cities <- ne_download(scale = 'medium', type = 'populated_places', category = 'cultural', returnclass = 'sf')

# Define color palette
color_palette <- c(
  'Research' = '#0F1F2C',
  'Finance and Programmes' = '#90876E',
  'Engagement, Advocacy and Capacity Building' = '#1E4611',
  'Policy' = '#CD1A1B'
)

# Create the main map
main_map <- ggplot() +
  geom_sf(data = afrika, fill = 'white', color = 'gray80', size = 0.3) +
  geom_point(data = southern_africa_data, 
             aes(x = lon, y = lat, color = FocusType, shape = factor(Shape)), 
             size = 3, alpha = 0.9) +
  ggrepel::geom_text_repel(data = southern_africa_data %>% filter(is_major_partner),
                           aes(x = lon, y = lat, label = Institution),
                           size = 2.5, fontface = 'bold', color = 'black',
                           bg.color = 'white', bg.r = 0.15,
                           segment.color = 'gray50', segment.size = 0.2,
                           min.segment.length = 0, max.overlaps = 20,
                           box.padding = 0.5, point.padding = 0.2) +
  scale_color_manual(values = color_palette, name = 'Focus Area') +
  scale_shape_manual(values = c('16' = 16, '17' = 17), 
                     name = 'Type',
                     labels = c('Regular Institution', 'Data Provider')) +
  coord_sf(xlim = c(10, 40), ylim = c(-35, 0), expand = FALSE) +
  annotation_scale(location = 'bl', width_hint = 0.25, style = 'ticks') +
  annotation_north_arrow(location = 'tr', which_north = 'true', 
                         style = north_arrow_minimal(), height = unit(1, 'cm'), width = unit(1, 'cm')) +
  labs(title = 'Climate & Health Initiatives in Southern Africa',
       subtitle = 'Distribution of institutions by focus area and data provider status',
       caption = 'Source: Wellcome Climate Center') +
  theme_minimal() +
  theme(text = element_text(family = 'sans', color = 'black'),
        plot.title = element_text(size = 14, face = 'bold'),
        plot.subtitle = element_text(size = 11),
        plot.caption = element_text(size = 9, hjust = 0),
        legend.position = 'right',
        legend.title = element_text(size = 10, face = 'bold'),
        legend.text = element_text(size = 9),
        panel.grid.major = element_line(color = 'gray90', linewidth = 0.2),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 8))

# Save the main map
ggsave('southern_africa_map_with_labels.png', main_map, width = 8, height = 6, dpi = 300)

# Define bounding boxes for urban insets
jhb_tshwane_bbox <- c(xmin = 27.8, xmax = 28.4, ymin = -26.3, ymax = -25.6)
cape_town_bbox <- c(xmin = 18.3, xmax = 18.7, ymin = -34.2, ymax = -33.7)

# Crop cities data for each inset (adding slight margins)
jhb_cities <- st_crop(cities, xmin = jhb_tshwane_bbox['xmin'] - 0.5, xmax = jhb_tshwane_bbox['xmax'] + 0.5,
                      ymin = jhb_tshwane_bbox['ymin'] - 0.5, ymax = jhb_tshwane_bbox['ymax'] + 0.5)
cape_cities <- st_crop(cities, xmin = cape_town_bbox['xmin'] - 0.5, xmax = cape_town_bbox['xmax'] + 0.5,
                       ymin = cape_town_bbox['ymin'] - 0.5, ymax = cape_town_bbox['ymax'] + 0.5)

# Create Johannesburg/Tshwane inset map with city details
jhb_tshwane_map <- ggplot() +
  geom_sf(data = afrika, fill = 'white', color = 'gray80', size = 0.3) +
  # Underlying city details (populated places): small transparent points
  geom_sf(data = jhb_cities, color = 'gray70', size = 1, shape = 1, alpha = 0.7) +
  geom_point(data = southern_africa_data, 
             aes(x = lon, y = lat, color = FocusType, shape = factor(Shape)), 
             size = 4, alpha = 0.9) +
  ggrepel::geom_text_repel(data = southern_africa_data %>% 
                              filter(lon >= jhb_tshwane_bbox['xmin'] & lon <= jhb_tshwane_bbox['xmax'] &
                                     lat >= jhb_tshwane_bbox['ymin'] & lat <= jhb_tshwane_bbox['ymax']),
                           aes(x = lon, y = lat, label = Institution),
                           size = 3, fontface = 'bold', color = 'black',
                           bg.color = 'white', bg.r = 0.15,
                           segment.color = 'gray50', segment.size = 0.2,
                           min.segment.length = 0, max.overlaps = 30,
                           box.padding = 0.5, point.padding = 0.2) +
  scale_color_manual(values = color_palette, name = 'Focus Area') +
  scale_shape_manual(values = c('16' = 16, '17' = 17), 
                     name = 'Type',
                     labels = c('Regular Institution', 'Data Provider')) +
  coord_sf(xlim = c(jhb_tshwane_bbox['xmin'], jhb_tshwane_bbox['xmax']), 
           ylim = c(jhb_tshwane_bbox['ymin'], jhb_tshwane_bbox['ymax']), expand = FALSE) +
  annotation_scale(location = 'bl', width_hint = 0.25, style = 'ticks') +
  labs(title = 'Johannesburg & Tshwane Climate & Health Initiatives',
       subtitle = 'Detailed view with underlying city context',
       caption = 'Source: Wellcome Climate Center') +
  theme_minimal() +
  theme(text = element_text(family = 'sans', color = 'black'),
        plot.title = element_text(size = 12, face = 'bold'),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 8, hjust = 0),
        legend.position = 'right',
        legend.title = element_text(size = 9, face = 'bold'),
        legend.text = element_text(size = 8),
        panel.grid.major = element_line(color = 'gray90', linewidth = 0.2),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 7))

# Save Johannesburg/Tshwane inset map
ggsave('johannesburg_tshwane_inset_map.png', jhb_tshwane_map, width = 6, height = 5, dpi = 300)

# Create Cape Town inset map with city details
cape_town_map <- ggplot() +
  geom_sf(data = afrika, fill = 'white', color = 'gray80', size = 0.3) +
  geom_sf(data = cape_cities, color = 'gray70', size = 1, shape = 1, alpha = 0.7) +
  geom_point(data = southern_africa_data, 
             aes(x = lon, y = lat, color = FocusType, shape = factor(Shape)), 
             size = 4, alpha = 0.9) +
  ggrepel::geom_text_repel(data = southern_africa_data %>% 
                              filter(lon >= cape_town_bbox['xmin'] & lon <= cape_town_bbox['xmax'] &
                                     lat >= cape_town_bbox['ymin'] & lat <= cape_town_bbox['ymax']),
                           aes(x = lon, y = lat, label = Institution),
                           size = 3, fontface = 'bold', color = 'black',
                           bg.color = 'white', bg.r = 0.15,
                           segment.color = 'gray50', segment.size = 0.2,
                           min.segment.length = 0, max.overlaps = 30,
                           box.padding = 0.5, point.padding = 0.2) +
  scale_color_manual(values = color_palette, name = 'Focus Area') +
  scale_shape_manual(values = c('16' = 16, '17' = 17), 
                     name = 'Type',
                     labels = c('Regular Institution', 'Data Provider')) +
  coord_sf(xlim = c(cape_town_bbox['xmin'], cape_town_bbox['xmax']), 
           ylim = c(cape_town_bbox['ymin'], cape_town_bbox['ymax']), expand = FALSE) +
  annotation_scale(location = 'bl', width_hint = 0.25, style = 'ticks') +
  labs(title = 'Cape Town Climate & Health Initiatives',
       subtitle = 'Detailed view with underlying city context',
       caption = 'Source: Wellcome Climate Center') +
  theme_minimal() +
  theme(text = element_text(family = 'sans', color = 'black'),
        plot.title = element_text(size = 12, face = 'bold'),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 8, hjust = 0),
        legend.position = 'right',
        legend.title = element_text(size = 9, face = 'bold'),
        legend.text = element_text(size = 8),
        panel.grid.major = element_line(color = 'gray90', linewidth = 0.2),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(size = 7))

# Save Cape Town inset map
ggsave('cape_town_inset_map.png', cape_town_map, width = 6, height = 5, dpi = 300)

# Create a combined map with Africa inset using adjusted positioning
inset_map <- ggplot() +
  geom_sf(data = afrika, fill = 'white', color = 'gray80', size = 0.2) +
  geom_rect(aes(xmin = 10, xmax = 40, ymin = -35, ymax = 0),
            fill = NA, color = 'red', size = 0.5) +
  coord_sf() +
  theme_void() +
  theme(panel.background = element_rect(fill = 'white'))

combined_map <- ggdraw() +
  draw_plot(main_map, 0, 0, 1, 1) +
  draw_plot(inset_map, 0.82, 0.05, 0.3, 0.3)  

# Save combined map
ggsave('southern_africa_map_with_inset.png', combined_map, width = 10, height = 8, dpi = 300)
