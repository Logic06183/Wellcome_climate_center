# Southern Africa Map with Focus Areas and Urban Insets
# Load required libraries
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(dplyr)
library(readr)
library(ggspatial) # For scale bar and north arrow
library(cowplot) # For publication-quality plots
library(ggrepel) # For non-overlapping labels

# Read the data
base_data <- read.csv("base.csv", sep = ";", stringsAsFactors = FALSE)

# Define Southern African countries
southern_africa_countries <- c(
  "South Africa", "Namibia", "Botswana", "Zimbabwe", "Mozambique", 
  "Lesotho", "Eswatini", "Zambia", "Malawi", "Angola"
)

# Filter data for Southern Africa
southern_africa_data <- base_data %>%
  filter(Country %in% southern_africa_countries)

# Create a new column for focus area type (for coloring)
# First, initialize with NA
southern_africa_data$FocusType <- NA

# Then assign focus areas based on priority order (to handle multiple focus areas)
# Priority: Research > Policy > Engagement > Finance
southern_africa_data$FocusType[southern_africa_data$Research == 1] <- "Research"
southern_africa_data$FocusType[southern_africa_data$Policy == 1 & is.na(southern_africa_data$FocusType)] <- "Policy"
# Use the exact column name with periods instead of commas and spaces
southern_africa_data$FocusType[southern_africa_data$Engagement..Advocacy..and.Capacity.Building == 1 & is.na(southern_africa_data$FocusType)] <- "Engagement, Advocacy and Capacity Building"
southern_africa_data$FocusType[southern_africa_data$Finance_programmes == 1 & is.na(southern_africa_data$FocusType)] <- "Finance and Programmes"

# Remove rows with NA focus type to avoid NA in legend
southern_africa_data <- southern_africa_data %>%
  filter(!is.na(FocusType))

# Create a shape column for Data Providers
southern_africa_data$Shape <- ifelse(southern_africa_data$Data_Providers == 1, 17, 16)

# Identify major partners (those with Official Partners = 1 and multiple focus areas or data providers)
# Use column names exactly as they appear in the CSV
southern_africa_data <- southern_africa_data %>%
  mutate(
    # Calculate focus count using exact column names
    focus_count = Research + Policy + 
                 Engagement..Advocacy..and.Capacity.Building + 
                 Finance_programmes,
    # Use exact column name "Official.Partners" with periods
    is_major_partner = ifelse(Official.Partners == 1 & 
                             (focus_count > 1 | Data_Providers == 1), 
                             TRUE, FALSE),
    label_text = ifelse(is_major_partner, Short_Name, NA)
  )

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter for Africa for context
africa <- world %>%
  filter(continent == "Africa")

# Define the color palette based on your specifications
color_palette <- c(
  "Research" = "#0F1F2C",
  "Finance and Programmes" = "#90876E",
  "Engagement, Advocacy and Capacity Building" = "#1E4611",
  "Policy" = "#CD1A1B"
)

# Create the main map with publication-quality improvements
main_map <- ggplot() +
  # Base map layer with subtle country borders
  geom_sf(data = africa, fill = "white", color = "gray80", size = 0.3) +
  
  # Points with focus area colors and shapes for data providers
  geom_point(
    data = southern_africa_data,
    aes(x = lon, y = lat, color = FocusType, shape = factor(Shape)),
    size = 3,
    alpha = 0.9
  ) +
  
  # Add labels for major partners with non-overlapping positioning
  ggrepel::geom_text_repel(
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
  
  # Apply the specified color palette
  scale_color_manual(
    values = color_palette,
    name = "Focus Area"
  ) +
  
  # Define shapes with clear labels
  scale_shape_manual(
    values = c("16" = 16, "17" = 17),
    name = "Type",
    labels = c("Regular Institution", "Data Provider")
  ) +
  
  # Set map boundaries for Southern Africa
  coord_sf(
    xlim = c(10, 40),
    ylim = c(-35, 0),
    expand = FALSE
  ) +
  
  # Add scale bar and north arrow
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    style = "ticks"
  ) +
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    style = north_arrow_minimal(),
    height = unit(1, "cm"),
    width = unit(1, "cm")
  ) +
  
  # Add informative labels
  labs(
    title = "Climate & Health Initiatives in Southern Africa",
    subtitle = "Distribution of institutions by focus area and data provider status",
    caption = "Source: Wellcome Climate Center"
  ) +
  
  # Apply a clean, publication-ready theme
  theme_minimal() +
  theme(
    # Use a standard font that should be available on all systems
    text = element_text(family = "sans", color = "black"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    plot.caption = element_text(size = 9, hjust = 0),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 8)
  )

# Define bounding boxes for the urban insets
# Johannesburg/Tshwane area (approximately)
jhb_tshwane_bbox <- c(
  xmin = 27.8, xmax = 28.4,  # longitude
  ymin = -26.3, ymax = -25.6 # latitude
)

# Cape Town area (approximately)
cape_town_bbox <- c(
  xmin = 18.3, xmax = 18.7,  # longitude
  ymin = -34.2, ymax = -33.7 # latitude
)

# Add bounding boxes to the main map
main_map_with_boxes <- main_map +
  # Add Johannesburg/Tshwane bounding box
  geom_rect(
    aes(
      xmin = jhb_tshwane_bbox[1], xmax = jhb_tshwane_bbox[2],
      ymin = jhb_tshwane_bbox[3], ymax = jhb_tshwane_bbox[4]
    ),
    fill = NA, color = "#0F1F2C", linewidth = 0.8, linetype = "solid"
  ) +
  # Add Cape Town bounding box
  geom_rect(
    aes(
      xmin = cape_town_bbox[1], xmax = cape_town_bbox[2],
      ymin = cape_town_bbox[3], ymax = cape_town_bbox[4]
    ),
    fill = NA, color = "#CD1A1B", linewidth = 0.8, linetype = "solid"
  )

# Create Johannesburg/Tshwane inset map
jhb_tshwane_map <- ggplot() +
  # Base map layer with subtle country borders
  geom_sf(data = africa, fill = "white", color = "gray80", size = 0.3) +
  
  # Points with focus area colors and shapes for data providers
  geom_point(
    data = southern_africa_data,
    aes(x = lon, y = lat, color = FocusType, shape = factor(Shape)),
    size = 4,  # Larger points for the inset
    alpha = 0.9
  ) +
  
  # Add labels for all partners in this area
  ggrepel::geom_text_repel(
    data = southern_africa_data %>% 
      filter(lon >= jhb_tshwane_bbox[1] & lon <= jhb_tshwane_bbox[2] & 
             lat >= jhb_tshwane_bbox[3] & lat <= jhb_tshwane_bbox[4]),
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
  
  # Apply the specified color palette
  scale_color_manual(
    values = color_palette,
    name = "Focus Area"
  ) +
  
  # Define shapes with clear labels
  scale_shape_manual(
    values = c("16" = 16, "17" = 17),
    name = "Type",
    labels = c("Regular Institution", "Data Provider")
  ) +
  
  # Set map boundaries for Johannesburg/Tshwane area
  coord_sf(
    xlim = c(jhb_tshwane_bbox[1], jhb_tshwane_bbox[2]),
    ylim = c(jhb_tshwane_bbox[3], jhb_tshwane_bbox[4]),
    expand = FALSE
  ) +
  
  # Add scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    style = "ticks"
  ) +
  
  # Add informative labels
  labs(
    title = "Johannesburg & Tshwane Climate & Health Initiatives",
    subtitle = "Detailed view of institutional partners",
    caption = "Source: Wellcome Climate Center"
  ) +
  
  # Apply a clean, publication-ready theme
  theme_minimal() +
  theme(
    # Use a standard font that should be available on all systems
    text = element_text(family = "sans", color = "black"),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8, hjust = 0),
    legend.position = "right",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 7)
  )

# Create Cape Town inset map
cape_town_map <- ggplot() +
  # Base map layer with subtle country borders
  geom_sf(data = africa, fill = "white", color = "gray80", size = 0.3) +
  
  # Points with focus area colors and shapes for data providers
  geom_point(
    data = southern_africa_data,
    aes(x = lon, y = lat, color = FocusType, shape = factor(Shape)),
    size = 4,  # Larger points for the inset
    alpha = 0.9
  ) +
  
  # Add labels for all partners in this area
  ggrepel::geom_text_repel(
    data = southern_africa_data %>% 
      filter(lon >= cape_town_bbox[1] & lon <= cape_town_bbox[2] & 
             lat >= cape_town_bbox[3] & lat <= cape_town_bbox[4]),
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
  
  # Apply the specified color palette
  scale_color_manual(
    values = color_palette,
    name = "Focus Area"
  ) +
  
  # Define shapes with clear labels
  scale_shape_manual(
    values = c("16" = 16, "17" = 17),
    name = "Type",
    labels = c("Regular Institution", "Data Provider")
  ) +
  
  # Set map boundaries for Cape Town area
  coord_sf(
    xlim = c(cape_town_bbox[1], cape_town_bbox[2]),
    ylim = c(cape_town_bbox[3], cape_town_bbox[4]),
    expand = FALSE
  ) +
  
  # Add scale bar
  annotation_scale(
    location = "bl",
    width_hint = 0.25,
    style = "ticks"
  ) +
  
  # Add informative labels
  labs(
    title = "Cape Town Climate & Health Initiatives",
    subtitle = "Detailed view of institutional partners",
    caption = "Source: Wellcome Climate Center"
  ) +
  
  # Apply a clean, publication-ready theme
  theme_minimal() +
  theme(
    # Use a standard font that should be available on all systems
    text = element_text(family = "sans", color = "black"),
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10),
    plot.caption = element_text(size = 8, hjust = 0),
    legend.position = "right",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    axis.title = element_blank(),
    axis.text = element_text(size = 7)
  )

# Create a combined map with Africa inset (for reference)
# Create the inset map showing all of Africa
inset_map <- ggplot() +
  geom_sf(data = africa, fill = "white", color = "gray80", size = 0.2) +
  geom_rect(
    aes(
      xmin = 10, xmax = 40,
      ymin = -35, ymax = 0
    ),
    fill = NA, color = "red", size = 0.5
  ) +
  coord_sf() +
  theme_void() +
  theme(panel.background = element_rect(fill = "white"))

# Save maps to PNG format
ggsave("southern_africa_map_with_labels.png", main_map, width = 8, height = 6, dpi = 300)
ggsave("southern_africa_map_with_boxes.png", main_map_with_boxes, width = 8, height = 6, dpi = 300)
ggsave("johannesburg_tshwane_inset_map.png", jhb_tshwane_map, width = 6, height = 5, dpi = 300)
ggsave("cape_town_inset_map.png", cape_town_map, width = 6, height = 5, dpi = 300)

# Create a combined map using cowplot
# Adjust the positioning based on the MEMORY about map positioning
combined_map <- ggdraw() +
  draw_plot(main_map, 0, 0, 1, 1) +
  # Moved the inset map further right (x coordinate changed from 0.72 to 0.82)
  draw_plot(inset_map, 0.82, 0.05, 0.3, 0.3)

# Save the combined map
ggsave("southern_africa_map_with_inset.png", combined_map, width = 10, height = 8, dpi = 300)
