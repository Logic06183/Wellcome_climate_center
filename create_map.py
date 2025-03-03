import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import contextily as cx
import numpy as np
from matplotlib_scalebar.scalebar import ScaleBar
from matplotlib.patches import Rectangle
import matplotlib.patches as mpatches
import matplotlib.lines as mlines
import os
from shapely.geometry import Point
import matplotlib as mpl
import matplotlib
import folium
from matplotlib.patheffects import withStroke

# Set matplotlib params for better output
plt.rcParams['font.family'] = 'sans-serif'
plt.rcParams['font.size'] = 10
plt.rcParams['figure.dpi'] = 150

# File path - adjust as needed
base_data_path = r"C:\Users\CraigParker\OneDrive - Wits PHR\Desktop\Wellcome_climate_center\base.csv"

# Function to load and process data
def load_data(file_path):
    print(f"Loading data from {file_path}")
    
    # Read CSV data
    try:
        data = pd.read_csv(file_path, sep=';')
        print(f"Successfully loaded data with {len(data)} rows")
    except Exception as e:
        print(f"Error loading data: {e}")
        return None
    
    # Print actual column names for debugging
    print('Actual columns in CSV:', list(data.columns))
    
    # Create a mapping of expected column names to actual column names
    column_mapping = {
        'Official Partners': 'Official Partners',
        'Engagement, Advocacy, and Capacity Building': 'Engagement, Advocacy, and Capacity Building',
        'Finance_programmes': 'Finance_programmes',
        'Data_Providers': 'Data_Providers',
        'Research': 'Research',
        'Policy': 'Policy',
        'Finance and Programmes': 'Finance and Programmes',
        'Engagement, Advocacy and Capacity Building': 'Engagement, Advocacy and Capacity Building'
    }
    
    # Update the mapping with actual column names
    for expected_name in list(column_mapping.keys()):
        for actual_name in data.columns:
            if expected_name.lower() in actual_name.lower():
                column_mapping[expected_name] = actual_name
    
    # Define Southern African countries
    southern_africa_countries = [
        "South Africa", "Namibia", "Botswana", "Zimbabwe", "Mozambique", 
        "Lesotho", "Eswatini", "Zambia", "Malawi", "Angola"
    ]
    
    # Filter for Southern Africa
    southern_africa_data = data[data['Country'].isin(southern_africa_countries)].copy()
    
    # Process focus type with priority logic
    southern_africa_data['FocusType'] = None
    
    # Apply priority: Research > Policy > Engagement > Finance
    mask = southern_africa_data[column_mapping['Research']] == 1
    southern_africa_data.loc[mask, 'FocusType'] = 'Research'
    
    mask = (southern_africa_data[column_mapping['Policy']] == 1) & (southern_africa_data['FocusType'].isna())
    southern_africa_data.loc[mask, 'FocusType'] = 'Policy'
    
    mask = (southern_africa_data['Engagement, Advocacy, and Capacity Building'] == 1) & (southern_africa_data['FocusType'].isna())
    southern_africa_data.loc[mask, 'FocusType'] = 'Engagement, Advocacy and Capacity Building'
    
    mask = (southern_africa_data['Finance_programmes'] == 1) & (southern_africa_data['FocusType'].isna())
    southern_africa_data.loc[mask, 'FocusType'] = 'Finance and Programmes'
    
    # Remove rows with no focus type
    southern_africa_data = southern_africa_data.dropna(subset=['FocusType'])
    
    # Create shape column for Data Providers
    southern_africa_data['Shape'] = southern_africa_data['Data_Providers'].apply(lambda x: 'triangle' if x == 1 else 'circle')
    
    # Calculate focus count and determine major partners
    southern_africa_data['focus_count'] = (southern_africa_data[column_mapping['Research']] + 
                                           southern_africa_data[column_mapping['Policy']] + 
                                           southern_africa_data['Engagement, Advocacy, and Capacity Building'] + 
                                           southern_africa_data['Finance_programmes'])
    
    southern_africa_data['is_major_partner'] = ((southern_africa_data['Official Partners'] == 1) & 
                                               ((southern_africa_data['focus_count'] > 1) | 
                                                (southern_africa_data['Data_Providers'] == 1)))
    
    # Create Point geometries
    geometry = [Point(xy) for xy in zip(southern_africa_data['lon'], southern_africa_data['lat'])]
    southern_africa_data = gpd.GeoDataFrame(southern_africa_data, geometry=geometry, crs="EPSG:4326")
    
    return southern_africa_data

# Load natural earth data for country boundaries
def load_map_data():
    print("Loading country boundary data")
    try:
        # Load world data at 1:50m scale
        world = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))
        
        # Filter for African countries
        africa = world[world['continent'] == 'Africa'].copy()
        
        # Get southern African countries
        southern_africa_countries = [
            "South Africa", "Namibia", "Botswana", "Zimbabwe", "Mozambique", 
            "Lesotho", "eSwatini", "Zambia", "Malawi", "Angola"
        ]
        southern_africa = world[world['name'].isin(southern_africa_countries)].copy()
        
        print("Successfully loaded boundary data")
        return world, africa, southern_africa
    except Exception as e:
        print(f"Error loading country boundaries: {e}")
        return None, None, None

# Define color palette
color_palette = {
    "Research": "#0F1F2C",
    "Finance and Programmes": "#90876E",
    "Engagement, Advocacy and Capacity Building": "#1E4611",
    "Policy": "#CD1A1B"
}

# Create main map
def create_main_map(data, africa, southern_africa, include_boxes=False):
    print("Creating main map")
    
    fig, ax = plt.subplots(figsize=(10, 8))
    
    # Plot Africa background
    africa.plot(ax=ax, color='white', edgecolor='gray', linewidth=0.3, alpha=0.9)
    
    # Plot southern Africa with slightly darker borders
    if southern_africa is not None:
        southern_africa.plot(ax=ax, color='white', edgecolor='darkgray', linewidth=0.5, alpha=0.9)
    
    # Plot points with focus area colors
    for focus_type, color in color_palette.items():
        # Filter by focus type
        focus_data = data[data['FocusType'] == focus_type]
        
        # Plot points by shape
        circle_data = focus_data[focus_data['Shape'] == 'circle']
        triangle_data = focus_data[focus_data['Shape'] == 'triangle']
        
        # Plot circles
        if len(circle_data) > 0:
            ax.scatter(circle_data.geometry.x, circle_data.geometry.y, 
                      c=color, marker='o', s=50, label=f"{focus_type} (Regular)",
                      edgecolor='black', linewidth=0.5, alpha=0.9)
        
        # Plot triangles
        if len(triangle_data) > 0:
            ax.scatter(triangle_data.geometry.x, triangle_data.geometry.y, 
                      c=color, marker='^', s=60, label=f"{focus_type} (Data Provider)",
                      edgecolor='black', linewidth=0.5, alpha=0.9)
    
    # Add labels for major partners with overlap avoidance
    used_positions = {}  # Keep track of label positions
    
    # Sort by latitude to prioritize placement
    labeled_data = data[data['is_major_partner']].sort_values('lat')
    
    for idx, row in labeled_data.iterrows():
        x, y = row.geometry.x, row.geometry.y
        full_name = row['Institution']  # Use full institution name
        
        # Initialize label position
        label_x = x + 0.05
        label_y = y + 0.05
        
        # Check for overlaps and adjust position
        overlap = True
        offsets = [(0.05, 0.05), (-0.05, 0.05), (0.05, -0.05), (-0.05, -0.05),
                  (0.1, 0), (-0.1, 0), (0, 0.1), (0, -0.1)]
        
        for dx, dy in offsets:
            test_x = x + dx
            test_y = y + dy
            
            # Check if this position is far enough from other labels
            position_ok = True
            for used_x, used_y in used_positions.values():
                if abs(test_x - used_x) < 0.2 and abs(test_y - used_y) < 0.1:
                    position_ok = False
                    break
            
            if position_ok:
                label_x = test_x
                label_y = test_y
                overlap = False
                break
        
        # Store the used position
        used_positions[full_name] = (label_x, label_y)
        
        # Add label with background box
        bbox_props = dict(
            boxstyle="round,pad=0.3",
            fc="white",
            ec="gray",
            alpha=0.9,
            mutation_scale=0.5
        )
        
        # Create arrow connection
        ax.annotate(
            full_name,
            xy=(x, y),  # Institution point
            xytext=(label_x, label_y),  # Label position
            bbox=bbox_props,
            fontsize=8,
            fontweight='bold',
            arrowprops=dict(
                arrowstyle="->",
                connectionstyle="arc3,rad=0.2",
                color='gray',
                alpha=0.6
            )
        )
    
    # Add inset boxes if requested
    if include_boxes:
        # Johannesburg/Tshwane area
        jhb_tshwane_bbox = [27.5, 28.7, -26.5, -25.4]  # Wider zoom for less clutter
        jhb_rect = Rectangle((jhb_tshwane_bbox[0], jhb_tshwane_bbox[2]), 
                            jhb_tshwane_bbox[1]-jhb_tshwane_bbox[0], 
                            jhb_tshwane_bbox[3]-jhb_tshwane_bbox[2],
                            linewidth=1, edgecolor='#0F1F2C', facecolor='none')
        ax.add_patch(jhb_rect)
        ax.annotate('Johannesburg/\nTshwane', 
                   (jhb_tshwane_bbox[0]-0.5, jhb_tshwane_bbox[2]-0.2),
                   fontsize=10, fontweight='bold', color='#0F1F2C')
        
        # Cape Town area
        cape_town_bbox = [18.3, 18.7, -34.2, -33.7]  # xmin, xmax, ymin, ymax
        cape_rect = Rectangle((cape_town_bbox[0], cape_town_bbox[2]), 
                             cape_town_bbox[1]-cape_town_bbox[0], 
                             cape_town_bbox[3]-cape_town_bbox[2],
                             linewidth=1, edgecolor='#CD1A1B', facecolor='none')
        ax.add_patch(cape_rect)
        ax.annotate('Cape Town', 
                   (cape_town_bbox[0]-0.5, cape_town_bbox[2]-0.2),
                   fontsize=10, fontweight='bold', color='#CD1A1B')
    
    # Set map boundaries for Southern Africa
    ax.set_xlim(10, 40)
    ax.set_ylim(-35, 0)
    
    # Add scale bar
    ax.add_artist(ScaleBar(1.0, dimension='si-length', units='km', 
                         location='lower left', pad=0.5, 
                         frameon=True, color='black', box_alpha=0.5))
    
    # Add North arrow - simple approach
    arrow_x, arrow_y = 38, -2
    arrow_length = 1
    ax.annotate('N', xy=(arrow_x, arrow_y), xytext=(arrow_x, arrow_y-arrow_length),
               arrowprops=dict(facecolor='black', width=1, headwidth=5),
               ha='center', va='center', fontsize=10, fontweight='bold')
    
    # Add title and caption
    ax.set_title("Climate & Health Initiatives in Southern Africa", fontsize=14, fontweight='bold')
    ax.text(0.5, 1.05, "Distribution of institutions by focus area and data provider status", 
           transform=ax.transAxes, ha='center', fontsize=11)
    ax.text(0.5, -0.05, "Source: Wellcome Climate Center", 
           transform=ax.transAxes, ha='center', fontsize=9)
    
    # Create custom legend
    handles = []
    
    # Focus Area colors
    for focus_type, color in color_palette.items():
        handles.append(mpatches.Patch(color=color, label=focus_type))
    
    # Add spacing in legend
    handles.append(plt.Line2D([0], [0], color='none', label=' '))  # Empty row for spacing
    
    # Shape types
    handles.append(mlines.Line2D([], [], color='gray', marker='o', linestyle='None',
                                markersize=6, label='Regular Institution'))
    handles.append(mlines.Line2D([], [], color='gray', marker='^', linestyle='None',
                                markersize=6, label='Data Provider'))
    
    # Make the legend more visible with a better background
    legend = plt.legend(handles=handles, 
              loc='lower right',
              ncol=1,
              frameon=True, 
              framealpha=0.9,  # Increased opacity
              edgecolor='gray',  # Add border
              fontsize=8)

    # Position it better - adjust these values as needed
    bbox = legend.get_bbox_to_anchor().transformed(ax.transAxes.inverted())
    legend.set_bbox_to_anchor([bbox.x0, 0.05], transform=ax.transAxes)  # Explicit y position
    
    # Remove axis labels and ticks
    ax.axis('off')
    
    # Set background color
    ax.set_facecolor('aliceblue')
    
    # Add country labels to main map for better context
    countries = {
        "South Africa": (25, -29),
        "Namibia": (18, -22),
        "Botswana": (24, -22), 
        "Zimbabwe": (30, -19),
        "Mozambique": (35, -18)
    }

    for country, (lon, lat) in countries.items():
        ax.text(lon, lat, country, fontsize=8, ha='center', 
               path_effects=[withStroke(foreground='white', linewidth=3)],
               zorder=90)
    
    return fig, ax

# Create detailed inset map with contextily basemap
def create_inset_map(data, bbox, title, simplified=False):
    print(f"Creating inset map for {title}")
    
    # Extract coordinates
    xmin, xmax, ymin, ymax = bbox
    
    # Filter data to show only major institutions or use another criterion
    # This could be based on size, importance, or simply limit the number
    if "johannesburg" in title.lower():
        # If you have an 'importance' column
        if 'importance' in data.columns:
            filtered_data = data[data['importance'] > 3]  # Adjust threshold as needed
        else:
            # Or limit to a maximum number of institutions in dense areas
            filtered_data = data.head(8)  # Show only top 8 institutions
    else:
        filtered_data = data
    
    # Filter data for this area
    area_data = filtered_data[(filtered_data.geometry.x >= xmin) & (filtered_data.geometry.x <= xmax) & 
                             (filtered_data.geometry.y >= ymin) & (filtered_data.geometry.y <= ymax)].copy()
    
    print(f"Found {len(area_data)} points in the {title} area")
    
    # Check if we have any points in this area
    if len(area_data) == 0:
        print(f"WARNING: No data points found in the {title} area!")
        print(f"Bounding box: {bbox}")
        print("Check if your bounding box coordinates are correct.")
        # Expand the bbox slightly in case points are just outside
        xmin -= 0.1
        xmax += 0.1
        ymin -= 0.1
        ymax += 0.1
        print(f"Trying expanded bounding box: [{xmin}, {xmax}, {ymin}, {ymax}]")
        area_data = filtered_data[(filtered_data.geometry.x >= xmin) & (filtered_data.geometry.x <= xmax) & 
                                 (filtered_data.geometry.y >= ymin) & (filtered_data.geometry.y <= ymax)].copy()
        print(f"Found {len(area_data)} points with expanded box")
    
    # Calculate buffer to add around points to ensure they're all visible
    buffer = 0.02
    
    # Create figure
    fig, ax = plt.subplots(figsize=(8, 7))
    
    # Add country boundaries to inset maps for context
    try:
        world = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))
        country_boundaries = world.to_crs(area_data.crs)
        country_boundaries.plot(ax=ax, color='white', edgecolor='gray', linewidth=0.5, alpha=0.5, zorder=1)
        print(f"Added country boundaries to {title} map")
    except Exception as e:
        print(f"Could not add country boundaries: {e}")
    
    # Plot points with focus area colors
    for focus_type, color in color_palette.items():
        # Filter by focus type
        focus_data = area_data[area_data['FocusType'] == focus_type]
        
        if len(focus_data) > 0:
            print(f"Found {len(focus_data)} points with focus type '{focus_type}' in {title}")
        
        # Plot points by shape
        circle_data = focus_data[focus_data['Shape'] == 'circle']
        triangle_data = focus_data[focus_data['Shape'] == 'triangle']
        
        # Plot circles
        if len(circle_data) > 0:
            ax.scatter(circle_data.geometry.x, circle_data.geometry.y, 
                      c=color, marker='o', s=80, label=f"{focus_type} (Regular)",
                      edgecolor='black', linewidth=0.5, alpha=0.9, zorder=10)
        
        # Plot triangles
        if len(triangle_data) > 0:
            ax.scatter(triangle_data.geometry.x, triangle_data.geometry.y, 
                      c=color, marker='^', s=100, label=f"{focus_type} (Data Provider)",
                      edgecolor='black', linewidth=0.5, alpha=0.9, zorder=10)
    
    # Enhanced label positioning algorithm with improved anti-overlap
    used_positions = {}  # Keep track of label positions
    
    # Sort institutions by importance to prioritize positioning (e.g., major partners first)
    # You can also sort alphabetically if preferred
    for idx, row in area_data.sort_values(by=['lat']).iterrows():
        x, y = row.geometry.x, row.geometry.y
        full_name = row['Institution']  # Use full institution name
        
        # Calculate text size for better positioning
        text_width = len(full_name) * 0.002  # Approximate width based on text length
        
        # Create a much wider range of possible offsets with greater distances
        # Start with positions further away from points to reduce clutter
        offsets = []
        
        # Generate more varied offsets (further from points)
        for dist in [0.02, 0.03, 0.04, 0.05]:
            # 8 directions around the point
            for angle in [0, 45, 90, 135, 180, 225, 270, 315]:
                rad = np.radians(angle)
                dx = dist * np.cos(rad)
                dy = dist * np.sin(rad)
                offsets.append((dx, dy))
        
        # Check for overlaps with much larger minimum distances
        overlap = True
        label_x, label_y = x, y  # Default position
        
        for dx, dy in offsets:
            test_x = x + dx
            test_y = y + dy
            
            # Increase minimum spacing between labels
            position_ok = True
            for used_x, used_y in used_positions.values():
                # Increase these values to enforce more space between labels
                if abs(test_x - used_x) < 0.3 and abs(test_y - used_y) < 0.2:
                    position_ok = False
                    break
            
            if position_ok:
                label_x = test_x
                label_y = test_y
                overlap = False
                break
        
        # Store the used position
        used_positions[full_name] = (label_x, label_y)
        
        # Create arrow connection with different styles
        conn_style = "arc3,rad=0.2" if idx % 2 == 0 else "arc3,rad=-0.2"
        
        # Add label with better formatting
        ax.annotate(
            full_name,
            xy=(x, y),  # Point
            xytext=(label_x, label_y),  # Label position
            bbox=dict(
                boxstyle="round,pad=0.3",
                fc="white",
                ec="gray",
                alpha=0.9,
                mutation_scale=0.5
            ),
            fontsize=7,  # Smaller font
            fontweight='normal',  # Less emphasis 
            arrowprops=dict(
                arrowstyle="->",
                connectionstyle=conn_style,
                color='gray',
                alpha=0.6,
                linewidth=0.8  # Thinner arrow
            ),
            zorder=11,
            ha='center'  # Center alignment can help with layout
        )
    
    # Create custom legend
    handles = []
    
    # Focus Area colors
    for focus_type, color in color_palette.items():
        handles.append(mpatches.Patch(color=color, label=focus_type))
    
    # Add spacing in legend
    handles.append(plt.Line2D([0], [0], color='none', label=''))
    
    # Shape types
    handles.append(mlines.Line2D([], [], color='gray', marker='o', linestyle='None',
                                markersize=6, label='Regular Institution'))
    handles.append(mlines.Line2D([], [], color='gray', marker='^', linestyle='None',
                                markersize=6, label='Data Provider'))
    
    # Add legend with two columns
    plt.legend(handles=handles, loc='upper left', ncol=1, frameon=True, 
              framealpha=0.9, fontsize=8)
    
    # Ensure map boundaries are set properly, with a small buffer
    if len(area_data) > 0:
        # Set boundaries based on data points with buffer
        x_range = area_data.geometry.x.max() - area_data.geometry.x.min()
        y_range = area_data.geometry.y.max() - area_data.geometry.y.min()
        
        # Add larger buffer as percentage of range (increased from 0.1 to 0.2)
        buffer_x = max(0.2 * x_range, 0.05)  # Increased buffer
        buffer_y = max(0.2 * y_range, 0.05)  # Increased buffer
        
        # Set boundaries
        ax.set_xlim(area_data.geometry.x.min() - buffer_x, area_data.geometry.x.max() + buffer_x)
        ax.set_ylim(area_data.geometry.y.min() - buffer_y, area_data.geometry.y.max() + buffer_y)
    else:
        # Use provided bbox if no data
        ax.set_xlim(xmin, xmax)
        ax.set_ylim(ymin, ymax)
    
    print(f"Set {title} map boundaries to: x=[{ax.get_xlim()[0]}, {ax.get_xlim()[1]}], y=[{ax.get_ylim()[0]}, {ax.get_ylim()[1]}]")
    
    # Try to add contextily basemap with simplified approach
    try:
        # Use a known-good provider and explicit approach
        print(f"Adding basemap for {title} using simplified approach")
        
        # First convert to web mercator
        area_mercator = area_data.to_crs(epsg=3857)
        
        # Create a new axis for the web mercator projection
        fig_mercator = plt.figure(figsize=(10, 9))  # Larger size for better quality
        ax_mercator = fig_mercator.add_subplot(111)
        
        # Plot the data on the mercator axis (make points transparent)
        area_mercator.plot(ax=ax_mercator, alpha=0, color='none')
        
        # Set the limits based on data with a larger buffer
        x_range = area_mercator.geometry.x.max() - area_mercator.geometry.x.min()
        y_range = area_mercator.geometry.y.max() - area_mercator.geometry.y.min()
        buffer_x = 0.25 * x_range  # Increased buffer for more context
        buffer_y = 0.25 * y_range  # Increased buffer for more context
        
        ax_mercator.set_xlim(area_mercator.geometry.x.min() - buffer_x, 
                            area_mercator.geometry.x.max() + buffer_x)
        ax_mercator.set_ylim(area_mercator.geometry.y.min() - buffer_y, 
                            area_mercator.geometry.y.max() + buffer_y)
        
        # Try different basemap providers for better clarity
        try:
            # First try Stamen Terrain which is often clearer for context
            cx.add_basemap(ax_mercator, source=cx.providers.Stamen.Terrain, zoom=13)
        except Exception:
            try:
                # Fall back to CartoDB which is a good alternative
                cx.add_basemap(ax_mercator, source=cx.providers.CartoDB.Positron, zoom=13)
            except Exception:
                # Last resort
                cx.add_basemap(ax_mercator, source=cx.providers.OpenStreetMap.Mapnik, zoom=13)
        
        # Save with higher DPI
        temp_map_file = f"python_maps/temp_{title.replace(' ', '_')}.png"
        plt.savefig(temp_map_file, dpi=200, bbox_inches='tight')
        plt.close(fig_mercator)
        
        # Now read the map as an image and display in our original axes
        from matplotlib.image import imread
        basemap_img = imread(temp_map_file)
        
        # Display the basemap image in our original axes
        ax.imshow(basemap_img, extent=ax.get_xlim() + ax.get_ylim(), 
                 alpha=0.8, zorder=0)
        
        # Remove the temporary file
        if os.path.exists(temp_map_file):
            os.remove(temp_map_file)
            
        print(f"Successfully added basemap for {title}")
        
    except Exception as e:
        print(f"Error adding basemap: {e}")
        # Continue with fallback static map
        add_static_basemap(ax, title)
    
    # Add scale bar
    ax.add_artist(ScaleBar(1.0, dimension='si-length', units='km', 
                         location='lower left', pad=0.5, 
                         frameon=True, color='black', box_alpha=0.5))
    
    # Add North arrow - simple approach
    arrow_x = xmax - 0.05 * (xmax - xmin)
    arrow_y = ymax - 0.1 * (ymax - ymin)
    arrow_length = 0.05 * (ymax - ymin)
    ax.annotate('N', xy=(arrow_x, arrow_y), xytext=(arrow_x, arrow_y-arrow_length),
               arrowprops=dict(facecolor='black', width=1, headwidth=5),
               ha='center', va='center', fontsize=10, fontweight='bold')
    
    # Add title and caption
    ax.set_title(f"{title} Climate & Health Initiatives", fontsize=14, fontweight='bold')
    ax.text(0.5, 1.05, "Detailed view of institutional partners", 
           transform=ax.transAxes, ha='center', fontsize=11)
    ax.text(0.5, -0.05, "Base map: OpenStreetMap contributors | Source: Wellcome Climate Center", 
           transform=ax.transAxes, ha='center', fontsize=9)
    
    # Before returning, set the figure's DPI to avoid oversized output
    fig.set_dpi(150)  # Reduce from your current 600 DPI setting
    
    # Ensure aspect ratio is reasonable
    ax.set_aspect('equal', adjustable='box')
    
    if simplified and title.lower() == "johannesburg & tshwane":
        # Create a colorblind-friendly colormap
        colors = plt.cm.tab10(range(len(data['FocusType'].unique())))
        
        # Plot points with legend but use varying sizes based on importance
        for i, category in enumerate(data['FocusType'].unique()):
            subset = data[data['FocusType'] == category]
            
            # If the data has official partners or other importance metrics
            if 'Official Partners' in subset.columns:
                sizes = subset['Official Partners'].apply(lambda x: 100 if x==1 else 60)
            else:
                sizes = 80  # Default size
            
            ax.scatter(
                subset.geometry.x, subset.geometry.y,
                s=sizes, alpha=0.8, 
                color=colors[i],
                label=f"{category} ({len(subset)} institutions)"
            )
        
        # Add a more prominently positioned legend
        legend = ax.legend(loc='upper right', fontsize=8, framealpha=0.9)
        legend.set_zorder(100)  # Ensure legend is on top
    
    return fig, ax

# Create combined layout
def create_combined_layout(main_fig, jhb_fig, cape_fig):
    print("Creating combined layout")
    
    # Function to copy artists from source to target axis
    def copy_artists(ax_source, ax_target):
        for artist in ax_source.get_children():
            if isinstance(artist, (plt.Line2D, plt.Polygon, plt.Text, plt.Rectangle, mpatches.Patch)):
                try:
                    artist_copy = artist.copy()
                    ax_target.add_artist(artist_copy)
                except AttributeError:
                    # Skip annotations that can't be copied
                    continue
    
    # Create new figure for combined layout
    fig = plt.figure(figsize=(15, 10))
    
    # Create main map axis
    ax_main = fig.add_axes([0.1, 0.1, 0.8, 0.8])
    
    # Copy main map content
    copy_artists(main_fig.axes[0], ax_main)
    
    # Create Johannesburg inset
    ax_jhb = fig.add_axes([0.82, 0.5, 0.15, 0.35])
    copy_artists(jhb_fig.axes[0], ax_jhb)
    
    # Create Cape Town inset
    ax_cape = fig.add_axes([0.82, 0.1, 0.15, 0.35])
    copy_artists(cape_fig.axes[0], ax_cape)
    
    return fig

# Create dashboard style layout
def create_dashboard(main_fig, jhb_fig, cape_fig):
    print("Creating dashboard layout")
    
    # Create a new figure
    fig = plt.figure(figsize=(15, 18))
    
    # Create a GridSpec with 2 rows and 2 columns
    gs = fig.add_gridspec(2, 2, height_ratios=[1.2, 1])
    
    # Create the main map in the top row (spanning both columns)
    ax_main = fig.add_subplot(gs[0, :])
    
    # Create the two inset maps in the bottom row
    bottom_left = fig.add_subplot(gs[1, 0])
    bottom_right = fig.add_subplot(gs[1, 1])
    
    # Function to recreate plots instead of copying
    def recreate_plot(ax_target, source_fig, title=None):
        # Get the source axes
        ax_source = source_fig.axes[0]
        
        # Copy all collections (like scatter plots)
        for collection in ax_source.collections:
            if isinstance(collection, matplotlib.collections.PathCollection):
                # Recreate scatter plot
                offsets = collection.get_offsets()
                sizes = collection.get_sizes() if hasattr(collection, 'get_sizes') else 100
                colors = collection.get_facecolors() if hasattr(collection, 'get_facecolors') else 'blue'
                
                # Create new scatter with same properties
                ax_target.scatter(
                    offsets[:, 0], offsets[:, 1],
                    s=sizes, c=colors,
                    alpha=collection.get_alpha() if hasattr(collection, 'get_alpha') else 1.0,
                    zorder=collection.get_zorder() if hasattr(collection, 'get_zorder') else 1
                )
        
        # Copy title if needed
        if title:
            ax_target.set_title(title)
        
        # Copy limits
        ax_target.set_xlim(ax_source.get_xlim())
        ax_target.set_ylim(ax_source.get_ylim())
        
        # Turn off axes
        ax_target.axis('off')
    
    # Recreate the three maps in their appropriate positions
    recreate_plot(ax_main, main_fig, "Southern Africa Climate & Health Initiatives")
    recreate_plot(bottom_left, jhb_fig, "Johannesburg & Tshwane Focus")
    recreate_plot(bottom_right, cape_fig, "Cape Town Focus")
    
    # Add panel labels
    ax_main.text(-0.05, 1.0, 'A', transform=ax_main.transAxes, 
                fontsize=20, fontweight='bold', va='top')
    bottom_left.text(-0.1, 1.0, 'B', transform=bottom_left.transAxes, 
                    fontsize=20, fontweight='bold', va='top')
    bottom_right.text(-0.1, 1.0, 'C', transform=bottom_right.transAxes, 
                     fontsize=20, fontweight='bold', va='top')
    
    # Add overall title
    fig.suptitle("Climate & Health Initiatives in Southern Africa", 
                fontsize=18, fontweight='bold', y=0.98)
    fig.text(0.5, 0.96, "Regional Overview and Urban Focus Areas", 
            ha='center', fontsize=14)
    fig.text(0.5, 0.02, "Source: Wellcome Climate Center | Base maps: OpenStreetMap contributors",
            ha='center', fontsize=10)
    
    # Adjust layout
    plt.tight_layout(rect=[0, 0.03, 1, 0.95])
    
    return fig

# Alternative approach - add this function to your script

def add_static_basemap(ax, area_name):
    """Add a static background map as a last resort."""
    # Use a simple world map as background
    try:
        world = gpd.read_file(gpd.datasets.get_path('naturalearth_lowres'))
        world = world.to_crs(epsg=4326)
        
        # Plot with appropriate styling
        world.plot(ax=ax, color='lightgray', edgecolor='dimgray', linewidth=0.5, alpha=0.5, zorder=0)
        
        # Add some major cities for context
        cities = {
            "Johannesburg": (28.0473, -26.2041),
            "Pretoria": (28.1881, -25.7461),
            "Cape Town": (18.4241, -33.9249),
            "Durban": (31.0218, -29.8587),
            "Gaborone": (25.9231, -24.6282),
            "Maputo": (32.6082, -25.9692),
            "Harare": (31.0522, -17.8312)
        }
        
        # Plot city markers for context
        for city, (lon, lat) in cities.items():
            ax.plot(lon, lat, 'o', color='darkblue', markersize=5, alpha=0.7, zorder=5)
            ax.text(lon, lat, f" {city}", fontsize=8, ha='left', va='center', alpha=0.7, zorder=5)
        
        print(f"Added static world map as basemap for {area_name}")
        return True
    except Exception as e:
        print(f"Failed to add static world map: {e}")
        return False

# Main function to run the workflow
def main():
    print("Starting map generation workflow")
    
    # Create output directory if it doesn't exist
    os.makedirs("python_maps", exist_ok=True)
    
    # Load data
    data = load_data(base_data_path)
    if data is None:
        print("Failed to load data. Exiting.")
        return
    
    # Load map boundary data
    world, africa, southern_africa = load_map_data()
    if africa is None:
        print("Failed to load boundary data. Exiting.")
        return
    
    # Define bounding boxes - adjust these if needed
    jhb_tshwane_bbox = [27.5, 28.7, -26.5, -25.4]  # Wider zoom for less clutter
    cape_town_bbox = [18.3, 18.7, -34.2, -33.7]  # xmin, xmax, ymin, ymax

    # If these don't work, try expanding them:
    # jhb_tshwane_bbox = [27.5, 28.5, -26.5, -25.4]
    # cape_town_bbox = [18.0, 19.0, -34.5, -33.5]
    
    # Create main map
    main_fig, _ = create_main_map(data, africa, southern_africa, include_boxes=False)
    main_fig_boxes, _ = create_main_map(data, africa, southern_africa, include_boxes=True)
    
    # Save main maps with PDF format
    main_fig.savefig("python_maps/southern_africa_map.pdf", dpi=300, bbox_inches='tight')
    main_fig_boxes.savefig("python_maps/southern_africa_map_with_boxes.pdf", dpi=300, bbox_inches='tight')
    
    # Create inset maps
    jhb_fig, _ = create_inset_map(data, jhb_tshwane_bbox, "Johannesburg & Tshwane", simplified=True)
    cape_fig, _ = create_inset_map(data, cape_town_bbox, "Cape Town")
    
    # Save inset maps with PDF format
    jhb_fig.savefig("python_maps/johannesburg_tshwane_map.pdf", dpi=300, bbox_inches='tight')
    cape_fig.savefig("python_maps/cape_town_map.pdf", dpi=300, bbox_inches='tight')
    
    # Also save PNG versions for compatibility
    jhb_fig.savefig("python_maps/johannesburg_tshwane_map.png", dpi=300, bbox_inches='tight')
    cape_fig.savefig("python_maps/cape_town_map.png", dpi=300, bbox_inches='tight')
    
    # Create combined layout
    combined_fig = create_combined_layout(main_fig_boxes, jhb_fig, cape_fig)
    combined_fig.savefig("python_maps/southern_africa_combined.pdf", dpi=300, bbox_inches='tight')
    
    # Create dashboard
    dashboard_fig = create_dashboard(main_fig, jhb_fig, cape_fig)
    dashboard_fig.savefig("python_maps/southern_africa_dashboard.pdf", dpi=300, bbox_inches='tight')
    
    print("Map generation complete. Files saved to 'python_maps' directory.")

if __name__ == "__main__":
    main()