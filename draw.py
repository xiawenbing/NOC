import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
import sys

latencyFile = open("simu-out/latency.csv", 'r')
latencyData = np.genfromtxt(latencyFile, delimiter=',', skip_header=1)

# Separate latency and count columns
latency = latencyData[:,0]  
count = latencyData[:,1]

# Plot bar chart
plt.bar(latency, count, color="#002554")

# Add labels and title
plt.xlabel('Latency/cycle')
plt.ylabel('Number of Packets')  
plt.title('Latency Distribution of Packets')

# Save the figure as a file 
plt.savefig('simu-out/latency.png')

latencyDistanceDf = pd.read_csv('simu-out/latency_distance.csv')
# latency = latencyDistanceDf['latency']
# distance = latencyDistanceDf['distance']
# count = latencyDistanceDf['count']

# plt.scatter(latency, distance, c=count, cmap='viridis')

# Create a heatmap using seaborn
sns.heatmap(data = latencyDistanceDf.
            pivot(index='distance', columns='latency', values='count').
            fillna(0), cmap=sns.cubehelix_palette(as_cmap=True), square=True, 
            cbar_kws={"shrink": .5, 'label': 'Number of Packets'}).invert_yaxis()

plt.xlabel('Latency')
plt.ylabel('Distance')
# plt.colorbar(label='Number of Packets')
plt.title('Latency Distribution of Packets')

plt.savefig('simu-out/latency_distance.png')