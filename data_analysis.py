import pandas as pd
import numpy as np
import glob
from scipy import stats
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit
from scipy.stats import norm

# =========================
# PARAMETERS
# =========================

DATA_PATH = "input_*.csv"

# normalize time:
# "relative" → T / T_winner
# "delay"    → T - T_winner
TIME_MODE = "relative"

# number of bins for averaging
NBINS = 500

# =========================
# FUNCTIONS
# =========================

def process_race(file):
    df = pd.read_csv(file, sep=' ')

    # drop invalid rows
    df = df.dropna(subset=["Performance_sec"]).copy()

    # sort by time
    df = df.sort_values("Performance_sec").reset_index(drop=True)

    N = len(df)
    if N < 10:
        return None  # skip too small races

    # normalized rank
    df["x"] = (np.arange(N) + 1) / N

    # normalize time
    T0 = df["Performance_sec"].iloc[0]

    if TIME_MODE == "delay":
        df["Tnorm"] = df["Performance_sec"] - T0
    elif TIME_MODE == "relative":
        df["Tnorm"] = df["Performance_sec"] / T0

    return df[["x", "Tnorm", "Performance_sec"]]


def bin_curve(df, nbins=100):
    bins = np.linspace(0, 1, nbins + 1)
    df["bin"] = np.digitize(df["x"], bins)

    grouped = df.groupby("bin").agg({
        "x": "mean",
        "Tnorm": "mean"
    })

    return grouped.dropna()


# =========================
# LOAD ALL RACES
# =========================

files = glob.glob(DATA_PATH)

curves = []
all_times = []

for f in files:
    race = process_race(f)
    if race is None:
        continue

    curves.append(bin_curve(race, NBINS))
    all_times.append(race["Performance_sec"].values)

# =========================
# AVERAGE CURVE
# =========================

# align bins
common_x = curves[0]["x"].values
T_matrix = []

for c in curves:
    if len(c) == len(common_x):
        T_matrix.append(c["Tnorm"].values)

T_matrix = np.array(T_matrix)

T_mean = np.mean(T_matrix, axis=0)
T_std = np.std(T_matrix, axis=0)

# save result
out = pd.DataFrame({
    "x": common_x,
    "T_mean": T_mean,
    "T_std": T_std
})

out.to_csv("average_curve.csv", index=False)

print("Computed average curve from", len(T_matrix), "races")

# =========================
# DISTRIBUTION ANALYSIS
# =========================

# concatenate all times
all_times = np.concatenate(all_times)

# remove zeros or negatives just in case
all_times = all_times[all_times > 0]

log_times = np.log(all_times)

# --- Fit normal ---
mu_T, sigma_T = np.mean(all_times), np.std(all_times)
ll_normal = np.sum(stats.norm.logpdf(all_times, mu_T, sigma_T))

# --- Fit log-normal ---
mu_log, sigma_log = np.mean(log_times), np.std(log_times)
ll_logn = np.sum(stats.norm.logpdf(log_times, mu_log, sigma_log) - np.log(all_times))

# --- AIC ---
AIC_normal = 2*2 - 2*ll_normal
AIC_logn   = 2*2 - 2*ll_logn

print("\nModel comparison:")
print("AIC Normal   :", AIC_normal)
print("AIC LogNormal:", AIC_logn)

# --- Normality test on log-times ---
shapiro = stats.shapiro(log_times[:5000])  # limit size
print("\nShapiro test on log(T):", shapiro)


import matplotlib.pyplot as plt
from scipy.optimize import curve_fit
from scipy.stats import norm

# =========================
# PREP DATA
# =========================

x = out["x"].values
T = out["T_mean"].values
Tstd = out["T_std"].values

# avoid infinities in probit
eps = 1e-6
x_fit = np.clip(x, eps, 1 - eps)

# =========================
# MODELS
# =========================

def probit_model(x, a, b):
    return a + b * norm.ppf(x)

def linear_model(x, a, b):
    return a + b * x

# =========================
# FITS
# =========================

# probit fit
popt_probit, pcov_probit = curve_fit(probit_model, x_fit, T)
T_probit = probit_model(x_fit, *popt_probit)

# linear fit (baseline)
popt_lin, _ = curve_fit(linear_model, x, T)
T_lin = linear_model(x, *popt_lin)

# =========================
# RESIDUALS
# =========================

res_probit = T - T_probit
res_lin = T - T_lin

rss_probit = np.sum(res_probit**2)
rss_lin = np.sum(res_lin**2)

print("\nFit comparison:")
print("Probit params (a, b):", popt_probit)
print("Linear params (a, b):", popt_lin)
print("RSS probit:", rss_probit)
print("RSS linear:", rss_lin)





# =========================
# PLOTS
# =========================

# --- 1. Main curve ---
plt.figure()
plt.plot(x, T, label="Average curve")
plt.fill_between(x, T - Tstd, T + Tstd, alpha=0.2)

plt.plot(x, T_probit, "--", label="Probit fit")
plt.plot(x, T_lin, ":", label="Linear fit")

plt.xlabel("Normalized rank x")
plt.ylabel("Time (normalized)")
plt.legend()
plt.title("Average time vs rank")

plt.savefig("curve_fit.png", dpi=150)
plt.close()

# --- 2. Log-log plot (tail behavior) ---
plt.figure()
plt.loglog(x, T, label="Data")
plt.loglog(x, T_probit, "--", label="Probit")

plt.xlabel("x (log)")
plt.ylabel("T (log)")
plt.legend()
plt.title("Log-log behavior")

plt.savefig("loglog_curve.png", dpi=150)
plt.close()

# --- 3. Residuals ---
plt.figure()
plt.plot(x, res_probit, label="Probit residuals")
plt.plot(x, res_lin, label="Linear residuals")

plt.axhline(0)
plt.xlabel("x")
plt.ylabel("Residual")

plt.legend()
plt.title("Fit residuals")

plt.savefig("residuals.png", dpi=150)
plt.close()

# --- 4. Probit linearization ---
plt.figure()
z = norm.ppf(x_fit)

plt.plot(z, T, label="Data")
plt.plot(z, T_probit, "--", label="Fit")

plt.xlabel("Probit(x)")
plt.ylabel("T")
plt.legend()
plt.title("Probit linearization")

plt.savefig("probit_linear.png", dpi=150)
plt.close()