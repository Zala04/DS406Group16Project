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

# 1. Prepare and merge the datasets just like before
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
BS_ITER = int(100)
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

# 1. Isolate precipitation and departure delays, dropping missing values
df2 = df[['precip', 'dep_delay']].dropna()

# 2. Filter the data
# Let's focus on instances where there was actually some precipitation to see its effect
# and remove extreme outliers (e.g., > 1 inch per hour) to avoid skewed results
df2 = df2[(df2['precip'] > 0) & (df2['precip'] <= 1.0)]

x = df2['precip']
y = df2['dep_delay']

# 3. Fit the linear regression model
mod2 = stats.linregress(x, y)
print(mod2)

# 4. Calculate 95% confidence intervals for the slope and intercept
tinv = lambda p, dfree: abs(stats.t.ppf(p/2, dfree))
ts = tinv(0.05, len(x)-2)

print(f"slope (95%): {mod2.slope:.4f} +/- {ts*mod2.stderr:.4f}")
print(f"intercept (95%): {mod2.intercept:.4f} +/- {ts*mod2.intercept_stderr:.4f}")

# 5. Save the regression results to CSV
results_precip = pd.DataFrame({
    'Metric': ['Slope', 'Intercept', 'R-squared', 'P-value', 'Std Error'],
    'Value': [mod2.slope, mod2.intercept, mod2.rvalue**2, mod2.pvalue, mod2.stderr]
})
results_precip.to_csv("precip_regression_results.csv", index=False)

print("Data successfully saved to 'precip_regression_results.csv'")

# 6. Generate the scatter plot
plt.scatter(x, y, label='original data', alpha=0.3)
plt.plot([x.min(), x.max()], 
         [mod2.intercept + mod2.slope * x.min(), mod2.intercept + mod2.slope * x.max()], 
         'r', label='fitted line', linewidth=3)
plt.legend()
plt.xlabel("Precipitation (inches/hour)")
plt.ylabel("Departure Delay (minutes)")
plt.title("Impact of Precipitation on Flight Delays")
plt.show()
