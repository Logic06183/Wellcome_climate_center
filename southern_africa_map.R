# Southern Africa Map with Focus Areas and Simplified Urban Insets
# Load required libraries (minimal set)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dplyr)
library(ggspatial) # For scale bar and north arrow
library(cowplot)   # For combining plots
library(ggrepel)   # For non-overlapping labels

# Read the data with full path and better error handling
tryCatch({
  base_data <- read.csv(file.path("C:", "Users", "CraigParker", "OneDrive - Wits PHR", "Desktop", "Wellcome_climate_center", "base.csv"), 
                       sep = ";", stringsAsFactors = FALSE)
}, error = function(e) {
  message("Error reading file: ", e$message)
  stop("Please check file path")
})

# Define Southern African countries
southern_africa_countries <- c(
  "South Africa", "Namibia", "Botswana", "Zimbabwe", "Mozambique", 
  "Lesotho", "Eswatini", "Zambia", "Malawi", "Angola"
)

# Filter data for Southern Africa with simplified processing
southern_africa_data <- base_data %>%
  filter(Country %in% southern_africa_countries) %>%
  # Create focus type using case_when (more robust)
  mutate(
    FocusType = case_when(
      Research == 1 ~ "Research",
      Policy == 1 ~ "Policy",
      `Engagement..Advocacy..and.Capacity.Building` == 1 ~ "Engagement, Advocacy and Capacity Building",
      Finance_programmes == 1 ~ "Finance and Programmes",
      TRUE ~ NA_character_
    ),
    # Shape for data providers
    Shape = ifelse(Data_Providers == 1, 17, 16),
    # Major partners
    focus_count = Research + Policy + `Engagement..Advocacy..and.Capacity.Building` + Finance_programmes,
    is_major_partner = Official.Partners == 1 & (focus_count > 1 | Data_Providers == 1),
    label_text = ifelse(is_major_partner, Short_Name, NA)
  ) %>%
  filter(!is.na(FocusType))

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")
africa <- world %>% filter(continent == "Africa")

# Get cities data for urban context
cities <- ne_cities(country = c("south africa"), returnclass = "sf")

# Define the color palette
color_palette <- c(
  "Research" = "#0F1F2C",
  "Finance and Programmes" = "#90876E",
  "Engagement, Advocacy and Capacity Building" = "#1E4611",
  "Policy" = "#CD1A1B"
)

# Define bounding boxes for the urban insets
jhb_tshwane_bbox <- c(
  xmin = 27.8, xmax = 28.4,  # longitude
  ymin = -26.3, ymax = -25.6 # latitude
)

cape_town_bbox <- c(
  xmin = 18.3, xmax = 18.7,  # longitude
  ymin = -34.2, ymax = -33.7 # latitude
)

# Create main map
main_map <- ggplot() +
  # Base map layer
  geom_sf(data = africa, fill = "white", color = "gray80", size = 0.3) +
  
  # Points for institutions
  geom_point(
    data = southern_africa_data,
    aes(x = lon, y = lat, color = FocusType, shape = factor(Shape)),
    size = 3,
    alpha = 0.9
  ) +
  
  # Labels for major partners
  geom_text_repel(
    data = southern_africa_data %>% filter(is_major_partner),
    aes(x = lon, y = lat, label = label_text),
    size = 2.5,
    fontface = "bold",
    color = "black",
    bg.color = "white",
    bg.r = 0.15,
    segment.color = "gray50",
    segment.size = 0.2,
    min.segment.length = 0,
    max.overlaps = 20,
    box.padding = 0.5,
    point.padding = 0.2
  ) +
  
  # Add bounding boxes for insets
  geom_rect(
    aes(
      xmin = jhb_tshwane_bbox[1], xmax = jhb_tshwane_bbox[2],
      ymin = jhb_tshwane_bbox[3], ymax = jhb_tshwane_bbox[4]
    ),
    fill = NA, color = "#0F1F2C", linewidth = 0.8, linetype = "solid"
  ) +
  geom_rect(
    aes(
      xmin = cape_town_bbox[1], xmax = cape_town_bbox[2],
      ymin = cape_town_bbox[3], ymax = cape_town_bbox[4]
    ),
    fill = NA, color = "#CD1A1B", linewidth = 0.8, linetype = "solid"
  ) +
  
  # Colors and shapes
  scale_color_manual(
    values = color_palette,
    name = "Focus Area"
  ) +
  scale_shape_manual(
    values = c("16" = 16, "17" = 17),
    name = "Type",
    labels = c("Regular Institution", "Data Provider")
  ) +
  
  # Map boundaries
  coord_sf(
    xlim = c(10, 40),
    ylim = c(-35, 0),
    expand = FALSE
  ) +
  
  # Scale bar and north arrow
  annotation_scale(
    location = "bl",
    width_hint = 0.4,
    style = "ticks",
    pad_x = unit(0.4, "cm"),
    pad_y = unit(0.4, "cm")
  ) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_minimal(),
    height = unit(1, "cm"),
    width = unit(1, "cm")
  ) +
  
  # Labels
  labs(
    title = "Climate & Health Initiatives in Southern Africa",
    subtitle = "Distribution of institutions by focus area and data provider status",
    caption = "Source: Wellcome Climate Center"
  ) +
  
  # Theme
  theme_minimal() +
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 9, hjust = 0),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.major = element_line(color = "gray95", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 8)
  )

# Function to create urban inset maps with additional city context
create_urban_inset <- function(bbox, area_name, data) {
  # Extract bbox values
  xmin <- bbox[1]
  xmax <- bbox[2]
  ymin <- bbox[3]
  ymax <- bbox[4]
  
  # Filter cities for this bbox
  area_cities <- cities %>%
    filter(longitude >= xmin & longitude <= xmax & 
           latitude >= ymin & latitude <= ymax)
  
  # Filter data points for this area
  area_data <- data %>%
    filter(lon >= xmin & lon <= xmax & lat >= ymin & lat <= ymax)
  
  ggplot() +
    # Add background grid
    geom_rect(
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "white", color = "gray50", size = 0.5
    ) +
    # Add grid lines for reference
    geom_hline(
      yintercept = seq(ceiling(ymin*10)/10, floor(ymax*10)/10, by = 0.1),
      color = "gray90", size = 0.2
    ) +
    geom_vline(
      xintercept = seq(ceiling(xmin*10)/10, floor(xmax*10)/10, by = 0.1),
      color = "gray90", size = 0.2
    ) +
    
    # Add city markers for reference
    geom_sf(
      data = area_cities,
      color = "black", size = 2
    ) +
    geom_sf_text(
      data = area_cities,
      aes(label = name),
      size = 3, 
      nudge_y = 0.02
    ) +
    
    # Add data points
    geom_point(
      data = area_data,
      aes(x = lon, y = lat, color = FocusType, shape = factor(Shape)),
      size = 4,
      alpha = 0.9
    ) +
    
    # Add labels
    geom_text_repel(
      data = area_data,
      aes(x = lon, y = lat, label = Short_Name),
      size = 3,
      fontface = "bold",
      color = "black",
      bg.color = "white",
      bg.r = 0.15,
      segment.color = "gray50",
      segment.size = 0.2,
      min.segment.length = 0,
      max.overlaps = 30,
      box.padding = 0.5,
      point.padding = 0.2
    ) +
    
    # Color and shape scales
    scale_color_manual(
      values = color_palette,
      name = "Focus Area"
    ) +
    scale_shape_manual(
      values = c("16" = 16, "17" = 17),
      name = "Type",
      labels = c("Regular Institution", "Data Provider")
    ) +
    
    # Set coordinates
    coord_sf(
      xlim = c(xmin, xmax),
      ylim = c(ymin, ymax),
      expand = FALSE
    ) +
    
    # Add scale bar
    annotation_scale(
      location = "bl",
      width_hint = 0.4,
      style = "ticks"
    ) +
    
    # Add labels
    labs(
      title = paste(area_name, "Climate & Health Initiatives"),
      subtitle = "Detailed view of institutional partners",
      caption = "Source: Wellcome Climate Center"
    ) +
    
    # Theme
    theme_minimal() +
    theme(
      text = element_text(family = "sans"),
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 10),
      plot.caption = element_text(size = 8, hjust = 0),
      legend.position = "bottom",
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8),
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_text(size = 7)
    )
}

# Create inset maps
jhb_tshwane_map <- create_urban_inset(
  jhb_tshwane_bbox, 
  "Johannesburg & Tshwane", 
  southern_africa_data
)

cape_town_map <- create_urban_inset(
  cape_town_bbox, 
  "Cape Town", 
  southern_africa_data
)

# Save individual maps
ggsave("southern_africa_map.pdf", main_map, width = 10, height = 8, device = cairo_pdf, dpi = 300)
ggsave("johannesburg_tshwane_map.pdf", jhb_tshwane_map, width = 7, height = 6, device = cairo_pdf, dpi = 300)
ggsave("cape_town_map.pdf", cape_town_map, width = 7, height = 6, device = cairo_pdf, dpi = 300)

# Create combined map
combined_map <- ggdraw() +
  draw_plot(main_map, 0, 0.35, 1, 0.65) +
  draw_plot(jhb_tshwane_map, 0, 0, 0.5, 0.35) +
  draw_plot(cape_town_map, 0.5, 0, 0.5, 0.35) +
  # Add panel labels
  draw_plot_label(c("A", "B", "C"), 
                 c(0.02, 0.02, 0.52), 
                 c(0.98, 0.35, 0.35), 
                 size = 20,
                 fontface = "bold")

# Save combined map
ggsave("combined_southern_africa_map.pdf",
       combined_map,
       width = 12,
       height = 16,
       device = cairo_pdf,
       dpi = 300)
