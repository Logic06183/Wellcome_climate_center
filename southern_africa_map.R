# Make sure your file starts EXACTLY like this
# (with NO OpenStreetMap references)

# Add these packages near the top of your script
if (!require("tidyverse")) {
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require("sf")) {
  install.packages("sf")
  library(sf)
}
if (!require("rnaturalearth")) {
  message("Installing rnaturalearth package for simple basemaps...")
  install.packages("rnaturalearth")
  library(rnaturalearth)
}
if (!require("rnaturalearthdata")) {
  message("Installing rnaturalearthdata package...")
  install.packages("rnaturalearthdata")
  library(rnaturalearthdata)
}
if (!require("ggrepel")) {
  install.packages("ggrepel")
  library(ggrepel)
}
if (!require("ggspatial")) {
  install.packages("ggspatial")
  library(ggspatial)
}

# Set main font (fallback to a standard font if preferred one isn't available)
main_font <- tryCatch({
  if (requireNamespace("extrafont", quietly = TRUE)) {
    extrafont::loadfonts(quiet = TRUE)
    "Arial"  # or any other font you have available
  } else {
    "sans"
  }
}, error = function(e) "sans")

# Load and process the CSV data with hardcoded path
message("Loading base data...")
# Try to verify if the file exists
file_path <- "C:/Users/CraigParker/OneDrive - Wits PHR/Desktop/Wellcome_climate_center/base.csv"
if (!file.exists(file_path)) {
  message("WARNING: File not found at: ", file_path)
  message("Attempting to search for base.csv in current directory and parent directories...")
  
  # Alternative paths to try
  alt_paths <- c(
    "base.csv",
    "../base.csv",
    "./base.csv",
    "data/base.csv"
  )
  
  for (path in alt_paths) {
    if (file.exists(path)) {
      message("Found file at: ", path)
      file_path <- path
      break
    }
  }
}

# Try to read the file
tryCatch({
  base_data <- read.csv(file_path, sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")
  message("Successfully loaded data with ", nrow(base_data), " rows")
}, error = function(e) {
  stop("Could not load the file. Error: ", e$message)
})

# Modified data cleaning section to use just the four focus areas
base_data <- base_data %>%
  # Remove rows with missing coordinates
  filter(!is.na(lon) & !is.na(lat)) %>%
  # Create an institution_type field based on available columns
  mutate(
    institution_type = case_when(
      `Data_Providers` == 1 ~ "Data Provider",
      `Official.Partners` == 1 ~ "Official Partner",
      TRUE ~ "Other"
    ),
    # Instead of primary_focus, create separate columns for each focus area
    has_policy = ifelse(Policy == 1, TRUE, FALSE),
    has_research = ifelse(Research == 1, TRUE, FALSE),
    has_finance = ifelse(Finance_programmes == 1, TRUE, FALSE),
    has_engagement = ifelse(`Engagement..Advocacy..and.Capacity.Building` == 1, TRUE, FALSE),
    # Count number of focus areas (for point size)
    focus_areas = rowSums(across(c(Policy, Research, `Engagement..Advocacy..and.Capacity.Building`, Finance_programmes), ~ifelse(is.na(.), 0, .)))
  )

# Convert to SF object for mapping
message("Converting to spatial data...")
base_sf <- st_as_sf(base_data, coords = c("lon", "lat"), crs = 4326)

# Add this right after loading the sf package
# Turn off s2 geometry for less strict operations
sf_use_s2(FALSE)

# Add this function to manually create province boundaries
create_sa_provinces <- function() {
  # Create a simple dataframe with South African province boundaries
  # These are approximate coordinates for demonstration purposes
  
  # Western Cape (Cape Town province)
  western_cape <- data.frame(
    province = "Western Cape",
    lon = c(18.0, 19.5, 22.0, 22.8, 21.0, 19.0, 18.0, 18.0),
    lat = c(-34.5, -34.8, -34.0, -32.8, -31.5, -32.0, -33.0, -34.5)
  )
  
  # Gauteng (Johannesburg/Pretoria province)
  gauteng <- data.frame(
    province = "Gauteng",
    lon = c(27.5, 28.8, 28.6, 27.3, 27.5),
    lat = c(-26.5, -26.4, -25.5, -25.8, -26.5)
  )
  
  # Convert to sf objects
  western_cape_sf <- western_cape %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    group_by(province) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  
  gauteng_sf <- gauteng %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    group_by(province) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  
  # Combine provinces
  provinces <- rbind(western_cape_sf, gauteng_sf)
  return(provinces)
}

# Add the ward boundaries function BEFORE create_urban_map
# Function to get South African ward boundaries
get_sa_wards <- function() {
  # Try to download SA ward boundaries if not already downloaded
  ward_file <- "sa_wards.rds"
  
  if (file.exists(ward_file)) {
    message("Loading cached ward boundaries...")
    wards <- readRDS(ward_file)
  } else {
    message("Attempting to download South African ward boundaries...")
    # Municipal Demarcation Board data is a good source for ward boundaries
    # This is a simplified approach - in practice you might need to download from their site
    
    tryCatch({
      # Try from GADM (Global Administrative Areas Database)
      if (!require("GADMTools")) {
        install.packages("GADMTools")
        library(GADMTools)
      }
      
      sa_adm <- gadm_sf_loadCountries(c("ZAF"), level = 4, basefile = "./")
      wards <- sa_adm$sf$ZAF
      saveRDS(wards, ward_file)
    }, error = function(e) {
      message("Could not download ward boundaries: ", e$message)
      message("Using provincial boundaries instead")
      wards <- NULL
    })
  }
  
  return(wards)
}

# Create urban map function
create_urban_map <- function(city_name, bbox, border_color = "#FF5500") {
  message(paste("Creating", city_name, "detailed map..."))
  
  # Filter institutions for this city
  city_name_parts <- unlist(strsplit(city_name, " & "))
  
  # More flexible city matching
  city_matches <- sapply(city_name_parts, function(x) {
    grepl(x, base_data$City, ignore.case = TRUE)
  })
  
  if(is.null(dim(city_matches))) {
    # Handle case with only one institution
    city_sf <- base_sf[city_matches, ]
  } else {
    city_sf <- base_sf[rowSums(city_matches) > 0, ]
  }
  
  if(nrow(city_sf) == 0) {
    message(paste("Warning: No institutions found in", city_name, ". Using default bounding box."))
    # We'll still create a map, just without points
  }
  
  # Define bounding box
  xmin <- bbox[1]
  xmax <- bbox[3]
  ymin <- bbox[2]
  ymax <- bbox[4]
  
  # Get simple basemap layers - only countries and coast
  countries <- ne_countries(scale = "medium", returnclass = "sf")
  coastline <- ne_coastline(scale = "medium", returnclass = "sf")
  
  # Skip downloading urban areas, rivers, and lakes
  urban <- NULL
  rivers <- NULL
  lakes <- NULL
  
  # Try to get administrative boundaries instead (simpler)
  tryCatch({
    admin_boundaries <- ne_download(scale = "medium", type = "admin_1_states_provinces", 
                                    category = "cultural", returnclass = "sf")
  }, error = function(e) {
    message("Could not download admin boundaries, using countries only...")
    admin_boundaries <- NULL
  })
  
  # Manual list of major cities with coordinates - simplified replacement for ne_cities
  major_cities <- data.frame(
    name = c("Cape Town", "Johannesburg", "Pretoria", "Durban", "Port Elizabeth", 
             "Bloemfontein", "Kimberley", "Gaborone", "Maseru", "Maputo", "Mbabane", 
             "Harare", "Bulawayo", "Lusaka", "Windhoek"),
    longitude = c(18.4241, 28.0473, 28.2293, 31.0218, 25.6022, 
                  26.2041, 24.7499, 25.9231, 27.4833, 32.5732, 31.1367, 
                  31.0534, 28.5833, 28.2833, 17.0658),
    latitude = c(-33.9249, -26.2041, -25.7461, -29.8587, -33.9608, 
                 -29.0852, -28.7282, -24.6282, -29.3167, -25.9692, -26.3167, 
                 -17.8277, -20.1500, -15.4167, -22.5609),
    pop_max = c(4000000, 5000000, 2500000, 3500000, 1200000, 
                750000, 250000, 230000, 330000, 1100000, 95000, 
                1500000, 650000, 2000000, 325000)
  )
  
  # Convert to sf object
  major_cities_sf <- st_as_sf(major_cities, coords = c("longitude", "latitude"), crs = 4326)
  
  # Filter cities that fall within our bounding box with some buffer
  buffer <- 0.1
  cities_in_view <- major_cities[
    major_cities$longitude > (xmin - buffer) & 
    major_cities$longitude < (xmax + buffer) &
    major_cities$latitude > (ymin - buffer) & 
    major_cities$latitude < (ymax + buffer), 
  ]
  
  # Adjust display based on city
  cape_town <- grepl("cape town", tolower(city_name))
  gauteng <- grepl("(johannesburg|pretoria|gauteng)", tolower(city_name))
  
  # Create the map with the simple base layers
  p <- ggplot() +
    # Base country layer
    geom_sf(data = countries, fill = "antiquewhite1", color = "gray80", size = 0.2)
  
  # Create a safety wrapper for intersection operations
  safe_intersection <- function(x, y) {
    tryCatch({
      st_intersection(x, y)
    }, error = function(e) {
      message("Intersection error: ", e$message)
      message("Attempting to fix invalid geometries...")
      x_valid <- st_make_valid(x)
      y_valid <- st_make_valid(y)
      tryCatch({
        st_intersection(x_valid, y_valid)
      }, error = function(e2) {
        message("Still failed after fixing, returning NULL")
        return(NULL)
      })
    })
  }
  
  # For urban areas intersection
  if (!is.null(urban)) {
    tryCatch({
      urban_bbox <- st_as_sfc(st_bbox(c(xmin = xmin-buffer, ymin = ymin-buffer, 
                                       xmax = xmax+buffer, ymax = ymax+buffer), crs = 4326))
      urban_in_view <- safe_intersection(urban, urban_bbox)
      if (!is.null(urban_in_view) && nrow(urban_in_view) > 0) {
        p <- p + geom_sf(data = urban_in_view, fill = "lightyellow", color = NA, alpha = 0.4)
      }
    }, error = function(e) {
      message("Urban areas error: ", e$message)
    })
  }
  
  # Add water features if available
  if (!is.null(lakes)) {
    p <- p + geom_sf(data = lakes, fill = "aliceblue", color = "steelblue", size = 0.2)
  }
  if (!is.null(rivers)) {
    p <- p + geom_sf(data = rivers, color = "steelblue", size = 0.2)
  }
  
  # Continue with the map
  p <- p +
    # Add coastline
    geom_sf(data = coastline, color = "steelblue", size = 0.3) +
    # Add the nearest cities as points
    geom_sf(data = st_as_sf(cities_in_view, coords = c("longitude", "latitude"), crs = 4326),
            color = "gray30", size = 0.8, alpha = 0.7) +
    # Add institution points with each focus area as a separate layer
    geom_sf(data = filter(city_sf, has_policy), 
            color = "#CD1A1B", # Policy - Red
            aes(shape = institution_type, size = focus_areas),
            alpha = 0.9) +
    geom_sf(data = filter(city_sf, has_research), 
            color = "#0F1F2C", # Research - Dark blue/black
            aes(shape = institution_type, size = focus_areas),
            alpha = 0.9) +
    geom_sf(data = filter(city_sf, has_finance), 
            color = "#90876E", # Finance and Programmes - Taupe
            aes(shape = institution_type, size = focus_areas),
            alpha = 0.9) +
    geom_sf(data = filter(city_sf, has_engagement), 
            color = "#1E4611", # Engagement - Dark green
            aes(shape = institution_type, size = focus_areas),
            alpha = 0.9) +
    # Different shapes for institution types
    scale_shape_manual(
      values = c(
        "Data Provider" = 18,        # Diamond
        "Official Partner" = 16,     # Circle
        "Other" = 15                 # Square
      ),
      name = "Institution Type"
    ) +
    # Set size scale
    scale_size_continuous(range = c(3, 6), name = "Number of\nFocus Areas") +
    # Add prominent boundary box
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = NA, color = border_color, size = 2.5, linetype = "solid") +
    # Add inner box for emphasis
    geom_rect(aes(xmin = xmin + 0.02, xmax = xmax - 0.02, 
                  ymin = ymin + 0.02, ymax = ymax - 0.02),
              fill = NA, color = border_color, size = 0.8, linetype = "dotted") +
    # Map elements
    annotation_scale(location = "bl", width_hint = 0.25) +
    annotation_north_arrow(
      location = "tr", which_north = "true",
      pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
      style = north_arrow_fancy_orienteering
    ) +
    # Set the view
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE, datum = NA) +
    # Scientific theming
    theme_minimal() +
    theme(
      text = element_text(family = main_font, color = "black"),
      plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
      plot.subtitle = element_text(size = 9, hjust = 0.5),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      panel.grid.major = element_line(color = "gray90", size = 0.2),
      panel.grid.minor = element_line(color = "gray95", size = 0.1),
      legend.position = "right",
      legend.box = "vertical",
      legend.title = element_text(size = 8, face = "bold"),
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.8, "lines"),
      plot.caption = element_text(size = 7, hjust = 0),
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
    ) +
    # Add source attribution
    labs(
      title = paste(city_name, "Climate & Health Institutions"),
      subtitle = "Focus Areas by Institution Type",
      caption = "Data source: Wellcome Trust Climate Center, 2023\nMap created using R with sf and ggplot2"
    )
  
  # Add other city labels (from our manual list)
  if(nrow(cities_in_view) > 0) {
    p <- p + 
      ggrepel::geom_text_repel(
        data = cities_in_view,
        aes(x = longitude, y = latitude, label = name),
        size = 2.5, family = main_font,
        color = "gray30",
        box.padding = 0.5, point.padding = 0.5,
        force = 2, max.overlaps = 10,
        bg.color = "white", bg.r = 0.1,
        segment.size = 0.15,
        min.segment.length = 0,
        seed = 42
      )
  }
  
  # Add institution labels
  if(nrow(city_sf) > 0) {
    # Create a data frame with label information but use full institution names
    institution_labels <- st_coordinates(city_sf) %>%
      as.data.frame() %>%
      bind_cols(
        Institution = city_sf$Institution
      )
    
    # Add the labels with repel to avoid overlaps
    p <- p + 
      ggrepel::geom_text_repel(
        data = institution_labels,
        aes(x = X, y = Y, label = Institution),  # Use full Institution name
        size = 2.8, family = main_font,
        color = "black",
        box.padding = 0.35,
        point.padding = 0.3,
        force = 3,
        max.overlaps = 30,  # Increased to allow more labels to appear
        bg.color = "white", 
        bg.r = 0.15,
        segment.size = 0.2,
        min.segment.length = 0,
        seed = 42
      )
  }
  
  # Also add a custom legend for the four focus areas
  p <- p +
    annotate("rect", xmin = xmax-0.08, xmax = xmax-0.06, ymin = ymin+0.04, ymax = ymin+0.06, fill = "#CD1A1B", alpha = 0.9) +
    annotate("text", x = xmax-0.05, y = ymin+0.05, label = "Policy", hjust = 0, size = 3) +
    
    annotate("rect", xmin = xmax-0.08, xmax = xmax-0.06, ymin = ymin+0.07, ymax = ymin+0.09, fill = "#0F1F2C", alpha = 0.9) +
    annotate("text", x = xmax-0.05, y = ymin+0.08, label = "Research", hjust = 0, size = 3) +
    
    annotate("rect", xmin = xmax-0.08, xmax = xmax-0.06, ymin = ymin+0.10, ymax = ymin+0.12, fill = "#90876E", alpha = 0.9) +
    annotate("text", x = xmax-0.05, y = ymin+0.11, label = "Finance and Programmes", hjust = 0, size = 3) +
    
    annotate("rect", xmin = xmax-0.08, xmax = xmax-0.06, ymin = ymin+0.13, ymax = ymin+0.15, fill = "#1E4611", alpha = 0.9) +
    annotate("text", x = xmax-0.05, y = ymin+0.14, label = "Engagement, Advocacy and Capacity Building", hjust = 0, size = 3)
  
  # Add explicit provincial boundaries for South Africa
  sa_provinces <- create_sa_provinces()
  
  # Identify which province we're working with
  current_province <- if(cape_town) "Western Cape" else if(gauteng) "Gauteng" else NULL
  
  # If we're in a South African province, highlight it
  if(!is.null(current_province)) {
    province_boundary <- sa_provinces[sa_provinces$province == current_province,]
    if(nrow(province_boundary) > 0) {
      p <- p + 
        geom_sf(data = province_boundary, 
                fill = NA, 
                color = "#FF5500", 
                size = 1.5, 
                linetype = "dashed")
    }
  }
  
  # Add ward boundaries if available
  sa_wards <- get_sa_wards()
  if (!is.null(sa_wards)) {
    # Extract wards for the current region
    if (cape_town) {
      # Filter for Western Cape wards
      region_wards <- sa_wards[grepl("Western Cape", sa_wards$NAME_1),]
    } else if (gauteng) {
      # Filter for Gauteng wards
      region_wards <- sa_wards[grepl("Gauteng", sa_wards$NAME_1),]
    } else {
      region_wards <- NULL
    }
    
    # Add ward boundaries if we have them for this region
    if (!is.null(region_wards) && nrow(region_wards) > 0) {
      p <- p + 
        geom_sf(data = region_wards, 
                fill = NA, 
                color = "gray50", 
                size = 0.15, 
                alpha = 0.6)
    }
  }
  
  return(p)
}

# Add this before the create_regional_map function
# Standalone wrapper function for safe intersection that can be shared across functions
safe_intersection <- function(x, y) {
  tryCatch({
    st_intersection(x, y)
  }, error = function(e) {
    message("Intersection error: ", e$message)
    message("Attempting to fix invalid geometries...")
    x_valid <- st_make_valid(x)
    y_valid <- st_make_valid(y)
    tryCatch({
      st_intersection(x_valid, y_valid)
    }, error = function(e2) {
      message("Still failed after fixing, returning NULL")
      return(NULL)
    })
  })
}

# Create a southern Africa regional map function
create_regional_map <- function() {
  message("Creating Southern Africa regional map...")
  
  # Define region for Southern Africa
  southern_africa_countries <- c(
    "South Africa", "Namibia", "Botswana", "Zimbabwe", 
    "Mozambique", "Lesotho", "Eswatini", "Zambia", "Malawi"
  )
  
  # Filter data for Southern Africa
  southern_africa_sf <- base_sf[base_sf$Country %in% southern_africa_countries, ]
  
  # Get regional countries
  countries <- ne_countries(scale = "medium", returnclass = "sf")
  sa_countries <- countries[countries$name %in% southern_africa_countries, ]
  
  # Define bounding box based on the region - with tighter constraints
  bbox <- st_bbox(sa_countries)
  xmin <- bbox["xmin"] + 5  # Make the eastern boundary tighter
  xmax <- bbox["xmax"] - 5  # Make the western boundary tighter
  ymin <- bbox["ymin"] + 3  # Make the northern boundary tighter 
  ymax <- bbox["ymax"] - 3  # Make the southern boundary tighter
  
  # Get simple basemap layers
  coastline <- ne_coastline(scale = "medium", returnclass = "sf")
  
  # Skip downloading rivers and lakes
  rivers <- NULL
  lakes <- NULL
  
  # Try to get administrative boundaries instead
  tryCatch({
    admin_boundaries <- ne_download(scale = "medium", type = "admin_1_states_provinces", 
                                    category = "cultural", returnclass = "sf")
    # Filter to southern Africa
    if(!is.null(admin_boundaries)) {
      sa_admin <- admin_boundaries[admin_boundaries$admin %in% southern_africa_countries, ]
      if(nrow(sa_admin) > 0) {
        p <- p + geom_sf(data = sa_admin, fill = NA, color = "gray70", size = 0.2)
      }
    }
  }, error = function(e) {
    message("Could not download admin boundaries, using countries only...")
  })
  
  # Manual list of major cities with coordinates - simplified replacement for ne_cities
  major_cities <- data.frame(
    name = c("Cape Town", "Johannesburg", "Pretoria", "Durban", "Port Elizabeth", 
             "Bloemfontein", "Kimberley", "Gaborone", "Maseru", "Maputo", "Mbabane", 
             "Harare", "Bulawayo", "Lusaka", "Windhoek"),
    longitude = c(18.4241, 28.0473, 28.2293, 31.0218, 25.6022, 
                  26.2041, 24.7499, 25.9231, 27.4833, 32.5732, 31.1367, 
                  31.0534, 28.5833, 28.2833, 17.0658),
    latitude = c(-33.9249, -26.2041, -25.7461, -29.8587, -33.9608, 
                 -29.0852, -28.7282, -24.6282, -29.3167, -25.9692, -26.3167, 
                 -17.8277, -20.1500, -15.4167, -22.5609),
    pop_max = c(4000000, 5000000, 2500000, 3500000, 1200000, 
                750000, 250000, 230000, 330000, 1100000, 95000, 
                1500000, 650000, 2000000, 325000)
  )
  
  # Convert to sf object
  major_cities_sf <- st_as_sf(major_cities, coords = c("longitude", "latitude"), crs = 4326)
  
  # Filter cities in the region
  cities_in_view <- major_cities[
    major_cities$longitude > xmin & major_cities$longitude < xmax &
    major_cities$latitude > ymin & major_cities$latitude < ymax &
    major_cities$pop_max > 500000, 
  ]
  
  # Create the map
  p <- ggplot() +
    # Countries
    geom_sf(data = countries, fill = "gray95", color = "gray80", size = 0.2) +
    geom_sf(data = sa_countries, fill = "antiquewhite1", color = "gray60", size = 0.3)
  
  # Add water features if available
  if (!is.null(lakes)) {
    tryCatch({
      sa_bbox <- st_as_sfc(st_bbox(sa_countries))
      lakes_clipped <- safe_intersection(lakes, sa_bbox)
      if (!is.null(lakes_clipped) && nrow(lakes_clipped) > 0) {
        p <- p + geom_sf(data = lakes_clipped, fill = "aliceblue", color = "steelblue", size = 0.2)
      }
    }, error = function(e) {
      message("Lakes intersection error: ", e$message)
    })
  }
  
  if (!is.null(rivers)) {
    tryCatch({
      sa_bbox <- st_as_sfc(st_bbox(sa_countries))
      rivers_clipped <- safe_intersection(rivers, sa_bbox)
      if (!is.null(rivers_clipped) && nrow(rivers_clipped) > 0) {
        p <- p + geom_sf(data = rivers_clipped, color = "steelblue", size = 0.2)
      }
    }, error = function(e) {
      message("Rivers intersection error: ", e$message)
    })
  }
  
  # Add South African provinces
  sa_provinces <- create_sa_provinces()
  p <- p + 
    geom_sf(data = sa_provinces, 
            fill = "antiquewhite1", 
            color = "#FF5500", 
            size = 1.2, 
            alpha = 0.2)
    
  # Add highlight boxes for Cape Town and Gauteng areas
  p <- p +
    # Cape Town highlight
    geom_rect(aes(xmin = 18.25, xmax = 18.80, ymin = -34.25, ymax = -33.70),
              fill = NA, color = "#FF5500", size = 1.2, linetype = "dotted") +
    # Gauteng highlight
    geom_rect(aes(xmin = 27.6, xmax = 28.6, ymin = -26.5, ymax = -25.5),
              fill = NA, color = "#FF5500", size = 1.2, linetype = "dotted") +
    # Add labels for the highlighted areas
    annotate("text", x = 18.52, y = -33.65, label = "Cape Town", 
             color = "#FF5500", fontface = "bold", size = 3.5) +
    annotate("text", x = 28.1, y = -25.45, label = "Gauteng", 
             color = "#FF5500", fontface = "bold", size = 3.5)
  
  # Create institution labels for the regional map
  if(nrow(southern_africa_sf) > 0) {
    # Create a data frame with label information
    institution_labels <- st_coordinates(southern_africa_sf) %>%
      as.data.frame() %>%
      bind_cols(
        Institution = southern_africa_sf$Institution
      )
    
    # Add institution labels with repel to avoid overlaps
    p <- p + 
      ggrepel::geom_text_repel(
        data = institution_labels,
        aes(x = X, y = Y, label = Institution),  # Full institution name
        size = 2.5, family = main_font,
        color = "black",
        box.padding = 0.35,
        point.padding = 0.3,
        force = 5,              # Increased force to spread labels better
        max.overlaps = 50,      # Allow more overlaps for regional map
        bg.color = "white", 
        bg.r = 0.15,
        segment.size = 0.2,
        min.segment.length = 0,
        seed = 45               # Different seed for potentially better layout
      )
  }
  
  # Complete the map
  p <- p + 
    # Add coastline
    geom_sf(data = coastline, color = "steelblue", size = 0.3) +
    # Add the major cities
    geom_sf(data = st_as_sf(cities_in_view, coords = c("longitude", "latitude"), crs = 4326), 
           color = "gray30", size = 1, alpha = 0.7) +
    # Add institution points for each focus area as separate layers
    geom_sf(data = filter(southern_africa_sf, has_policy), 
            color = "#CD1A1B", # Policy - Red
            aes(shape = institution_type, size = focus_areas),
            alpha = 0.9) +
    geom_sf(data = filter(southern_africa_sf, has_research), 
            color = "#0F1F2C", # Research - Dark blue/black
            aes(shape = institution_type, size = focus_areas),
            alpha = 0.9) +
    geom_sf(data = filter(southern_africa_sf, has_finance), 
            color = "#90876E", # Finance and Programmes - Taupe
            aes(shape = institution_type, size = focus_areas),
            alpha = 0.9) +
    geom_sf(data = filter(southern_africa_sf, has_engagement), 
            color = "#1E4611", # Engagement - Dark green
            aes(shape = institution_type, size = focus_areas),
            alpha = 0.9) +
    # Different shapes for institution types
    scale_shape_manual(
      values = c(
        "Data Provider" = 18,        # Diamond  
        "Official Partner" = 16,     # Circle
        "Other" = 15                 # Square
      ),
      name = "Institution Type"
    ) +
    # Set size scale
    scale_size_continuous(range = c(3, 6), name = "Number of\nFocus Areas") +
    # Improve legend layout for Wellcome Trust style
    theme(
      legend.position = "right",
      legend.box = "vertical",
      legend.title = element_text(face = "bold", size = 9),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.8, "lines"),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5, margin = margin(b = 15)),
      plot.subtitle = element_text(size = 10, hjust = 0.5, margin = margin(b = 10))
    ) +
    # Map elements
    annotation_scale(location = "bl", width_hint = 0.25) +
    annotation_north_arrow(
      location = "tr", which_north = "true",
      pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
      style = north_arrow_fancy_orienteering
    ) +
    # Set the view
    coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
    # Theming
    theme_minimal() +
    labs(title = "Southern Africa Climate & Health Institutions",
         subtitle = "Focus Areas: Policy, Research, Finance and Programmes, Engagement, Advocacy and Capacity Building")
  
  # Add city labels
  if(nrow(cities_in_view) > 0) {
    p <- p + 
      ggrepel::geom_text_repel(
        data = cities_in_view,
        aes(x = longitude, y = latitude, label = name),
        size = 3, family = main_font,
        color = "gray30",
        box.padding = 0.5, point.padding = 0.5,
        force = 2, max.overlaps = 15,
        bg.color = "white", bg.r = 0.1,
        segment.size = 0.15,
        min.segment.length = 0,
        seed = 42
      )
  }
  
  # Enforce the map boundaries strictly
  p <- p + coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE)
  
  return(p)
}

# Now create the maps
message("Starting map creation...")

# Map for Cape Town
cape_town_bbox <- c(18.41, -33.98, 18.50, -33.89)  # Tightly focused on Cape Town CBD
cape_town_map <- create_urban_map("Cape Town", cape_town_bbox, "#FF5500")

# Map for Johannesburg and Pretoria region (Gauteng)
gauteng_bbox <- c(28.00, -26.22, 28.12, -26.10)  # Tightly focused on Johannesburg CBD
gauteng_map <- create_urban_map("Johannesburg & Pretoria", gauteng_bbox, "#FF5500")

# Map for Southern Africa
southern_africa_map <- create_regional_map()

# Check city names in your data
city_counts <- table(base_data$City)
print(city_counts)

# Check if Johannesburg institutions exist
joburg_institutions <- base_data[grepl("Johannesburg", base_data$City, ignore.case=TRUE), c("Institution", "City")]
print(joburg_institutions)

# Save the maps
message("Saving maps to files...")

# Create output directory if it doesn't exist
if (!dir.exists("output")) {
  dir.create("output")
}

ggsave("output/cape_town_map.png", cape_town_map, width = 10, height = 8, dpi = 300)
ggsave("output/gauteng_map.png", gauteng_map, width = 10, height = 8, dpi = 300)
ggsave("output/southern_africa_map.png", southern_africa_map, width = 12, height = 10, dpi = 300)

message("All maps created successfully!")