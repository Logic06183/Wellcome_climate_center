import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from matplotlib_venn import venn2, venn3, venn3_circles

# Load the data
# Replace 'path_to_file.csv' with the actual path to your CSV file
df = pd.read_csv('base.csv', sep=';')

# Count organizations in each category
policy_count = df['Policy'].sum()
research_count = df['Research'].sum()
engagement_count = df['Engagement, Advocacy, and Capacity Building'].sum()
finance_count = df['Finance_programmes'].sum()

# Check for any rows with all zeros in the four categories
missing_classification = df[(df['Policy'] == 0) & 
                           (df['Research'] == 0) & 
                           (df['Engagement, Advocacy, and Capacity Building'] == 0) & 
                           (df['Finance_programmes'] == 0)]

# Create combinations for Venn diagram
policy_only = df[(df['Policy'] == 1) & (df['Research'] == 0) & 
                (df['Engagement, Advocacy, and Capacity Building'] == 0) & 
                (df['Finance_programmes'] == 0)].shape[0]

research_only = df[(df['Policy'] == 0) & (df['Research'] == 1) & 
                  (df['Engagement, Advocacy, and Capacity Building'] == 0) & 
                  (df['Finance_programmes'] == 0)].shape[0]

engagement_only = df[(df['Policy'] == 0) & (df['Research'] == 0) & 
                    (df['Engagement, Advocacy, and Capacity Building'] == 1) & 
                    (df['Finance_programmes'] == 0)].shape[0]

finance_only = df[(df['Policy'] == 0) & (df['Research'] == 0) & 
                 (df['Engagement, Advocacy, and Capacity Building'] == 0) & 
                 (df['Finance_programmes'] == 1)].shape[0]

# Define custom colors
colors = {
    'Policy': '#CD1A1B',
    'Research': '#0F1F2C',
    'Engagement & Advocacy': '#1E4611',
    'Finance & Programmes': '#90876E'
}

# Create bar chart
categories = ['Policy', 'Research', 'Engagement & Advocacy', 'Finance & Programmes']
counts = [policy_count, research_count, engagement_count, finance_count]
category_colors = [colors[cat] for cat in categories]

plt.figure(figsize=(10, 6))
bars = plt.bar(categories, counts, color=category_colors)
plt.title('Distribution of Organizations by Category', fontsize=16)
plt.ylabel('Number of Organizations', fontsize=14)
plt.xticks(fontsize=12)
plt.yticks(fontsize=12)

# Add count labels on top of bars
for bar in bars:
    height = bar.get_height()
    plt.text(bar.get_x() + bar.get_width()/2., height + 0.5,
             f'{int(height)}', ha='center', fontsize=12)

plt.tight_layout()
plt.savefig('category_distribution.png')
plt.show()

# Create a pie chart showing the percentage of organizations in each category
plt.figure(figsize=(10, 8))
plt.pie(counts, labels=categories, autopct='%1.1f%%', startangle=90, 
        colors=category_colors)
plt.title('Percentage of Organizations by Category', fontsize=16)
plt.axis('equal')
plt.tight_layout()
plt.savefig('category_percentage.png')
plt.show()

# Print summary
print(f"Total organizations: {len(df)}")
print(f"Organizations with Policy role: {policy_count} ({policy_count/len(df)*100:.1f}%)")
print(f"Organizations with Research role: {research_count} ({research_count/len(df)*100:.1f}%)")
print(f"Organizations with Engagement role: {engagement_count} ({engagement_count/len(df)*100:.1f}%)")
print(f"Organizations with Finance role: {finance_count} ({finance_count/len(df)*100:.1f}%)")
print(f"Organizations with no classification: {len(missing_classification)}")

if len(missing_classification) > 0:
    print("Organizations missing classification:")
    print(missing_classification[['Institution', 'City', 'Country']])
