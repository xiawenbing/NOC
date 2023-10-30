import numpy as np
import matplotlib.pyplot as plt
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