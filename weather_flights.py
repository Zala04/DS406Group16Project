import pandas as pd
import numpy as np
import scipy.stats as stats
import matplotlib.pyplot as plt

# Load the datasets using pandas
flights_url = "https://raw.githubusercontent.com/vaibhavwalvekar/NYC-Flights-2013-Dataset-Analysis/master/flights.csv"
weather_url = "https://raw.githubusercontent.com/tidyverse/nycflights13/main/data-raw/weather.csv"

flights = pd.read_csv(flights_url)
weather = pd.read_csv(weather_url)

# The 'flights' dataset from this URL has an extra 'Unnamed: 0' index column we should drop
if 'Unnamed: 0' in flights.columns:
    flights = flights.drop(columns=['Unnamed: 0'])

# 1. Prepare and merge the datasets
df = pd.merge(flights, weather, 
              on=['origin', 'year', 'month', 'day', 'hour'], 
              how='inner')

# Group A: Flights during hours with NO precipitation
# Group B: Flights during hours WITH precipitation
X = df.loc[df['precip'] == 0, 'dep_delay'].dropna()
Y = df.loc[df['precip'] > 0, 'dep_delay'].dropna()

# Absolute change function
def BootstrapAbsChange(X, Y):
    Xb = np.random.choice(X, len(X))
    Yb = np.random.choice(Y, len(Y))
    return Yb.mean() - Xb.mean()

# Relative change function
def BootstrapRelChange(X, Y):
    Xb = np.random.choice(X, len(X))
    Yb = np.random.choice(Y, len(Y))
    return (Yb.mean() / Xb.mean()) - 1.0

# Run the bootstrap for 10,000 iterations
BS_ITER = int(1e4)
Db = np.repeat(0.0, BS_ITER)
Rb = np.repeat(0.0, BS_ITER)

for iter in range(0, BS_ITER):
    Db[iter] = BootstrapAbsChange(X, Y)
    Rb[iter] = BootstrapRelChange(X, Y)

# Calculate point estimates
D_mean = Y.mean() - X.mean()
R_mean = D_mean / X.mean()

print(f"Absolute Change Point Estimate: {D_mean:.2f} mins")
print("Absolute Change 95% CI:", np.quantile(Db, [0.025, 0.975]))

print(f"Relative Change Point Estimate: {R_mean:.2%}")
print("Relative Change 95% CI:", np.quantile(Rb, [0.025, 0.975]))

# Define the airports and assign a distinct, visually appealing color to each
airports = ['EWR', 'JFK', 'LGA']
color_map = {'EWR': '#1f77b4',  # Blue
             'JFK': '#ff7f0e',  # Orange
             'LGA': '#2ca02c'}  # Green

data_to_plot = []
labels = []
box_colors = []

# 1. First gather all "No Precipitation" data
for airport in airports:
    no_precip = df.loc[(df['origin'] == airport) & (df['precip'] == 0), 'dep_delay'].dropna()
    data_to_plot.append(no_precip)
    labels.append(f"{airport}\nNo Precip")
    box_colors.append(color_map[airport])

# 2. Then gather all "Precipitation" data
for airport in airports:
    precip = df.loc[(df['origin'] == airport) & (df['precip'] > 0), 'dep_delay'].dropna()
    data_to_plot.append(precip)
    labels.append(f"{airport}\nPrecip")
    box_colors.append(color_map[airport])

# Generate the plot
plt.figure(figsize=(10, 6))

# Create the boxplot, enabling patch_artist to fill the boxes with color
bplot = plt.boxplot(data_to_plot, labels=labels, showfliers=False, patch_artist=True,
                    medianprops=dict(color='firebrick', linewidth=2),
                    whiskerprops=dict(color='black', linewidth=1.5),
                    capprops=dict(color='black', linewidth=1.5))

# Apply the mapped colors to the boxes with slight transparency
for patch, color in zip(bplot['boxes'], box_colors):
    patch.set_facecolor(color)
    patch.set_alpha(0.7)

# Add a vertical line exactly in the middle (between x=3 and x=4) to separate the weather groups
plt.axvline(x=3.5, color='gray', linestyle='--', linewidth=2, alpha=0.7)

# Styling and labels
plt.ylabel("Departure Delay (minutes)", fontsize=12)
plt.title("Effect of Precipitation on Flight Delays by Airport (Outliers Excluded)", fontsize=14, pad=15)
plt.grid(axis='y', linestyle=':', alpha=0.7)

plt.tight_layout()
plt.show()

#boxplot  -> check difference when percipitation vs not not the effect of percipitation if its more intense
df['precPresent'] = (df['precip'] > 0)
df.boxplot(column='dep_delay', by='precPresent')
plt.xticks([1,2], ['No Precipitation', 'Precipitation'])
plt.ylabel("Departure Delay")
plt.title("Effect of Precipitation on flight delays")
plt.suptitle("")
plt.ylim(-20, 100)
plt.show()
