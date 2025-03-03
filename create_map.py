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
        jhb_tshwane_bbox = [27.8, 28.4, -26.3, -25.6]  # xmin, xmax, ymin, ymax
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
    handles.append(plt.Line2D([0], [0], color='none', label=''))
    
    # Shape types
    handles.append(mlines.Line2D([], [], color='gray', marker='o', linestyle='None',
                                markersize=6, label='Regular Institution'))
    handles.append(mlines.Line2D([], [], color='gray', marker='^', linestyle='None',
                                markersize=6, label='Data Provider'))
    
    # Add legend with two columns
    plt.legend(handles=handles, loc='lower right', ncol=2, frameon=True, 
              framealpha=0.8, fontsize=8)
    
    # Remove axis labels and ticks
    ax.axis('off')
    
    # Set background color
    ax.set_facecolor('aliceblue')
    
    return fig, ax

# Create detailed inset map with contextily basemap
def create_inset_map(data, bbox, area_name):
    print(f"Creating inset map for {area_name}")
    
    # Extract coordinates
    xmin, xmax, ymin, ymax = bbox
    
    # Filter data for this area
    area_data = data[(data.geometry.x >= xmin) & (data.geometry.x <= xmax) & 
                     (data.geometry.y >= ymin) & (data.geometry.y <= ymax)].copy()
    
    # Calculate buffer to add around points to ensure they're all visible
    buffer = 0.02
    
    # Create figure
    fig, ax = plt.subplots(figsize=(8, 7))
    
    # Plot points with focus area colors
    for focus_type, color in color_palette.items():
        # Filter by focus type
        focus_data = area_data[area_data['FocusType'] == focus_type]
        
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
    
    # Add labels for all partners in this area with better positioning
    for idx, row in area_data.iterrows():
        ax.annotate(row['Short_Name'], 
                   (row.geometry.x, row.geometry.y),
                   xytext=(5, 5), textcoords='offset points',
                   bbox=dict(boxstyle="round,pad=0.3", fc="white", ec="gray", alpha=0.8),
                   fontsize=9, fontweight='bold', zorder=11)
    
    # Set map boundaries
    ax.set_xlim(xmin, xmax)
    ax.set_ylim(ymin, ymax)
    
    # Try to add contextily basemap (OpenStreetMap)
    try:
        cx.add_basemap(ax, crs=area_data.crs.to_string(), source=cx.providers.OpenStreetMap.Mapnik, 
                      zoom='auto', alpha=0.8)
        print(f"Successfully added basemap for {area_name}")
    except Exception as e:
        print(f"Error adding basemap: {e}")
        print("Using plain background instead")
        # Add a grid as fallback
        ax.grid(True, linestyle='--', alpha=0.3, zorder=1)
        ax.set_facecolor('aliceblue')
    
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
    ax.set_title(f"{area_name} Climate & Health Initiatives", fontsize=14, fontweight='bold')
    ax.text(0.5, 1.05, "Detailed view of institutional partners", 
           transform=ax.transAxes, ha='center', fontsize=11)
    ax.text(0.5, -0.05, "Base map: OpenStreetMap contributors | Source: Wellcome Climate Center", 
           transform=ax.transAxes, ha='center', fontsize=9)
    
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
    plt.legend(handles=handles, loc='lower right', ncol=2, frameon=True, 
              framealpha=0.8, fontsize=8)
    
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
    
    # Function to copy artists from source to target axis
    def copy_artists(ax_source, ax_target):
        # Copy collections (scatter plots, etc)
        for collection in ax_source.collections:
            if isinstance(collection, mpl.collections.PathCollection):
                # Handle scatter plots
                collection_copy = ax_target.scatter(
                    collection.get_offsets()[:, 0],
                    collection.get_offsets()[:, 1],
                    s=collection.get_sizes(),
                    c=collection.get_facecolors(),
                    marker=collection.get_paths()[0] if len(collection.get_paths()) > 0 else 'o',
                    alpha=collection.get_alpha(),
                    zorder=collection.get_zorder()
                )
            elif isinstance(collection, mpl.collections.PatchCollection):
                # Handle rectangles and other patches
                for patch in collection.get_paths():
                    patch_copy = mpatches.PathPatch(
                        patch,
                        facecolor=collection.get_facecolor(),
                        edgecolor=collection.get_edgecolor(),
                        linewidth=collection.get_linewidth(),
                        alpha=collection.get_alpha()
                    )
                    ax_target.add_patch(patch_copy)
        
        # Copy other artists
        for artist in ax_source.get_children():
            if isinstance(artist, (plt.Line2D, plt.Polygon, plt.Text, plt.Rectangle, mpatches.Patch)):
                try:
                    artist_copy = artist.copy()
                    ax_target.add_artist(artist_copy)
                except (AttributeError, ValueError):
                    # Skip artists that can't be copied
                    continue
        
        # Set the limits
        ax_target.set_xlim(ax_source.get_xlim())
        ax_target.set_ylim(ax_source.get_ylim())
    
    # Create a new figure for the combined layout
    fig, axes = plt.subplots(2, 1, figsize=(15, 18), gridspec_kw={'height_ratios': [1.2, 1]})
    
    # Create nested grid for bottom row
    bottom_grid = axes[1].inset_axes([0, 0, 1, 1])
    bottom_left = bottom_grid.inset_axes([0, 0, 0.5, 1])
    bottom_right = bottom_grid.inset_axes([0.5, 0, 0.5, 1])
    
    # Hide the original bottom axis
    axes[1].axis('off')
    bottom_grid.axis('off')
    
    # Copy content from individual figures
    copy_artists(main_fig.axes[0], axes[0])
    copy_artists(jhb_fig.axes[0], bottom_left)
    copy_artists(cape_fig.axes[0], bottom_right)
    
    # Turn off axes
    axes[0].axis('off')
    bottom_left.axis('off')
    bottom_right.axis('off')
    
    # Add panel labels
    axes[0].text(-0.05, 1.0, 'A', transform=axes[0].transAxes, 
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
    
    # Define bounding boxes
    jhb_tshwane_bbox = [27.8, 28.4, -26.3, -25.6]  # xmin, xmax, ymin, ymax
    cape_town_bbox = [18.3, 18.7, -34.2, -33.7]  # xmin, xmax, ymin, ymax
    
    # Create main map
    main_fig, _ = create_main_map(data, africa, southern_africa, include_boxes=False)
    main_fig_boxes, _ = create_main_map(data, africa, southern_africa, include_boxes=True)
    
    # Save main maps
    main_fig.savefig("python_maps/southern_africa_map.png", dpi=600, bbox_inches='tight')
    main_fig_boxes.savefig("python_maps/southern_africa_map_with_boxes.png", dpi=600, bbox_inches='tight')
    
    # Create inset maps
    jhb_fig, _ = create_inset_map(data, jhb_tshwane_bbox, "Johannesburg & Tshwane")
    cape_fig, _ = create_inset_map(data, cape_town_bbox, "Cape Town")
    
    # Save inset maps
    jhb_fig.savefig("python_maps/johannesburg_tshwane_map.png", dpi=600, bbox_inches='tight')
    cape_fig.savefig("python_maps/cape_town_map.png", dpi=600, bbox_inches='tight')
    
    # Create combined layout
    combined_fig = create_combined_layout(main_fig_boxes, jhb_fig, cape_fig)
    combined_fig.savefig("python_maps/southern_africa_combined.png", dpi=600, bbox_inches='tight')
    
    # Create dashboard
    dashboard_fig = create_dashboard(main_fig, jhb_fig, cape_fig)
    dashboard_fig.savefig("python_maps/southern_africa_dashboard.png", dpi=600, bbox_inches='tight')
    
    print("Map generation complete. Files saved to 'python_maps' directory.")

if __name__ == "__main__":
    main()