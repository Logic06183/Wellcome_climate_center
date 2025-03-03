import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# Load the data
df = pd.read_csv('base.csv', sep=';')

# Define custom colors
colors = {
    'Policy': '#CD1A1B',
    'Research': '#0F1F2C',
    'Engagement & Advocacy': '#1E4611',
    'Finance & Programmes': '#90876E'
}

# Define Southern African countries
southern_africa_countries = ['South Africa', 'Zimbabwe', 'Mozambique', 'Botswana', 
                            'Namibia', 'Lesotho', 'Eswatini', 'Malawi', 'Zambia', 'Angola']

# Filter for Southern Africa
southern_africa_df = df[df['Country'].isin(southern_africa_countries)]

# Filter for Gauteng (Johannesburg and Pretoria)
gauteng_df = df[(df['City'] == 'Johannesburg') | (df['City'] == 'Pretoria') | 
                (df['City'] == 'Soweto')]

# Function to create visualizations for a given dataframe and title
def create_visualizations(dataframe, region_name):
    # Count organizations in each category
    policy_count = dataframe['Policy'].sum()
    research_count = dataframe['Research'].sum()
    engagement_count = dataframe['Engagement, Advocacy, and Capacity Building'].sum()
    finance_count = dataframe['Finance_programmes'].sum()
    
    # Create bar chart
    categories = ['Policy', 'Research', 'Engagement & Advocacy', 'Finance & Programmes']
    counts = [policy_count, research_count, engagement_count, finance_count]
    category_colors = [colors[cat] for cat in categories]
    
    plt.figure(figsize=(10, 6))
    bars = plt.bar(categories, counts, color=category_colors)
    plt.title(f'Distribution of Organizations in {region_name} by Category', fontsize=16)
    plt.ylabel('Number of Organizations', fontsize=14)
    plt.xticks(fontsize=12)
    plt.yticks(fontsize=12)
    
    # Add count labels on top of bars
    for bar in bars:
        height = bar.get_height()
        plt.text(bar.get_x() + bar.get_width()/2., height + 0.5,
                 f'{int(height)}', ha='center', fontsize=12)
    
    plt.tight_layout()
    plt.savefig(f'{region_name.lower().replace(" ", "_")}_distribution.png')
    plt.show()
    
    # Create a pie chart showing the percentage of organizations in each category
    plt.figure(figsize=(10, 8))
    plt.pie(counts, labels=categories, autopct='%1.1f%%', startangle=90, 
            colors=category_colors)
    plt.title(f'Percentage of Organizations in {region_name} by Category', fontsize=16)
    plt.axis('equal')
    plt.tight_layout()
    plt.savefig(f'{region_name.lower().replace(" ", "_")}_percentage.png')
    plt.show()
    
    # Print summary
    print(f"\n--- {region_name} Summary ---")
    print(f"Total organizations: {len(dataframe)}")
    print(f"Organizations with Policy role: {policy_count} ({policy_count/len(dataframe)*100:.1f}%)")
    print(f"Organizations with Research role: {research_count} ({research_count/len(dataframe)*100:.1f}%)")
    print(f"Organizations with Engagement role: {engagement_count} ({engagement_count/len(dataframe)*100:.1f}%)")
    print(f"Organizations with Finance role: {finance_count} ({finance_count/len(dataframe)*100:.1f}%)")
    
    # Create a table of organizations by city
    if region_name == "Southern Africa":
        city_counts = dataframe['City'].value_counts()
        plt.figure(figsize=(12, 8))
        plt.bar(city_counts.index, city_counts.values, color='#4472C4')
        plt.title('Number of Organizations by City in Southern Africa', fontsize=16)
        plt.ylabel('Number of Organizations', fontsize=14)
        plt.xticks(rotation=45, ha='right', fontsize=10)
        plt.tight_layout()
        plt.savefig('southern_africa_cities.png')
        plt.show()

# Create visualizations for Southern Africa
create_visualizations(southern_africa_df, "Southern Africa")

# Create visualizations for Gauteng
create_visualizations(gauteng_df, "Gauteng")

# Create a map of Southern African partners (if coordinates are available)
# This requires additional libraries like geopandas and contextily
# If you want to implement this, please let me know 