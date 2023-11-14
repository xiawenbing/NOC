import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
import sys

#################### RECORD LATENCY ####################

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
plt.savefig('simu-out/latency.png', bbox_inches='tight', pad_inches=0.05)

#################### RECORD LATENCY-DISTANCE ####################

latencyDistanceDf = pd.read_csv('simu-out/latency_distance.csv')
range = np.arange(1, 60, 3)
range = range.tolist() + [100]
cut = pd.cut(latencyDistanceDf['latency'], range)
latencyDistanceDf = latencyDistanceDf.groupby([cut,'distance'])['count'].sum().to_frame()
latencyDistanceDf = latencyDistanceDf.loc[latencyDistanceDf['count'] != 0]
data = latencyDistanceDf.pivot_table(index='distance', columns='latency', values='count').fillna(0)

fig, ax = plt.subplots(figsize=(15,6))
# Create a heatmap using seaborn
sns.heatmap(data = data, 
            cmap=sns.cubehelix_palette(as_cmap=True), square=True, 
            cbar_kws={"shrink": .7, 'label': 'Number of Packets', "pad": 0.02},
            linewidths=.5, linecolor='white').invert_yaxis()

plt.xlabel('Latency')
plt.ylabel('Distance')
plt.title('Latency Distribution of Packets')

plt.savefig('simu-out/latency_distance.png', bbox_inches='tight', pad_inches=0.05)

#################### RECORD BUFFERS UTILIZATION ####################

buffersUtil = np.loadtxt('simu-out/buffers_util.txt').reshape(2, 2)

f, ax = plt.subplots()
sns.heatmap(data = buffersUtil, annot=True, fmt='.2f', square=True, 
            cmap=sns.cubehelix_palette(start=.5, rot=-.5, as_cmap=True),
            cbar_kws={"shrink": .7, 'label': 'Average Buffer Usage (%)', "pad": 0.02},
            linewidths=.5, linecolor='white').invert_yaxis()

plt.xlabel('X')
plt.ylabel('Y')
plt.title('Buffers Utilization Rate of Routers')

plt.savefig('simu-out/buffers_util.png', bbox_inches='tight', pad_inches=0.05)
