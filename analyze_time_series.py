import numpy as np
import scipy as sp
import pandas as pd
import glob
from scipy.interpolate import interp1d
from scipy.stats import gaussian_kde

# =========================
# PARAMETERS
# =========================

N = 512
DATA_PATH = "../data/input_*.csv"
Ny = 500
y_min = 1.0
y_max = 5.0   # you can refine dynamically later
u_min = 0.0
u_max = 2.0

y_grid = np.linspace(y_min, y_max, Ny)
u_grid = np.linspace(u_min, u_max, Ny)


pdf_sum = np.zeros(Ny)
pdf_log_sum = np.zeros(Ny)
pdf_u_sum = np.zeros(Ny)
pdf_u_log_sum = np.zeros(Ny)
pdf_count = 0


# =========================
# UTILITIES
# =========================

def read_race(file):
    df = pd.read_csv(file, sep=' ')
    df = df.dropna(subset=["Performance"]).copy()

    # convert time string → seconds
    t = pd.to_timedelta(df["Performance"], errors="coerce").dt.total_seconds()
    s = pd.to_numeric(df["Avg.Speed km/h"], errors="coerce")

    mask = (~t.isna()) & (~s.isna())
    t = t[mask].values
    s = s[mask].values

    # sort by time
    idx = np.argsort(t)
    return t[idx], s[idx]


def interp_data(y, N):
    x_old = np.linspace(0, 1, len(y))
    x_new = np.linspace(0, 1, N)
    f = interp1d(x_old, y, kind='cubic', fill_value="extrapolate")
    return f(x_new)


def compute_pdf(data, npts=500):
    kde = gaussian_kde(data)
    x = np.linspace(y_min, y_max, npts)
    return x, kde(x)

# =========================
# LOAD ALL RACES
# =========================

files = glob.glob(DATA_PATH)

Ey = np.zeros(N)
Et = np.zeros(N)
Es = np.zeros(N)

Ty = np.zeros(N)
Tt = np.zeros(N)
Ts = np.zeros(N)

ta = 0.0
te = 0.0

valid_races = 0

for file in files:
    print("Processing:", file)

    t, s = read_race(file)

    if len(t) < 10:
        continue

    valid_races += 1

    t0 = t[0]

    # --- save raw output (like Fortran) ---
    x = np.arange(len(t)) / len(t)
    y = t / t0
    z = t - t0
    out = np.column_stack([
        x,
        y,        # relative
        z,        # delay
        s
    ])
    np.savetxt(file.replace("input_", "output_").replace(".csv", ".dat"), out)

    # --- interpolation ---
    yi = interp_data(y, N)
    ti = interp_data(t, N)
    si = interp_data(s, N)

    # --- arithmetic averages ---
    Ey += yi
    Et += ti
    Es += si
    te += t0

    # --- geometric averages ---
    Ty += np.log(yi)
    Tt += np.log(ti/t0)
    Ts += np.log(si)
    ta += np.log(t0)


    # --- KDE on common grid ---
    d = y
    kde = gaussian_kde(d)
    pdf_vals = kde(y_grid)
    pdf_sum     += pdf_vals
    pdf_log_sum += np.log(pdf_vals + 1e-12)  # avoid log(0)

    u = np.log(y)
    kde = gaussian_kde(u)
    pdf_u = kde(u_grid)
    pdf_u_sum     += pdf_u

    pdf_count += 1


    # --- PDF ---
    xpdf, ypdf = compute_pdf(y[1:], 500)
    xpdf, zpdf = compute_pdf(t-t0, 500)

    np.savetxt(file.replace("input_", "relative_PDF_").replace(".csv", ".dat"),
               np.column_stack([xpdf, ypdf]))    
    np.savetxt(file.replace("input_", "delay_PDF_").replace(".csv", ".dat"),
               np.column_stack([xpdf, zpdf]))

# =========================
# FINAL AVERAGES
# =========================

Ey /= valid_races
Et /= valid_races
Es /= valid_races
te /= valid_races

Ty = np.exp(Ty / valid_races)
Tt = np.exp(Tt / valid_races)
Ts = np.exp(Ts / valid_races)
ta = ta / valid_races

pdf_avg = pdf_sum / pdf_count
pdf_u_avg = pdf_u_sum / pdf_count
pdf_geo = np.exp(pdf_log_sum / pdf_count)

# --- renormalize ---
pdf_avg /= np.trapezoid(pdf_avg, y_grid)
pdf_geo /= np.trapezoid(pdf_geo, y_grid)
y_grid = np.exp(u_grid)
pdf_y_from_u = pdf_u_avg / y_grid
pdf_y_from_u /= np.trapezoid(pdf_y_from_u, y_grid)


# --- save ---
np.savetxt("PDF_avg.dat", np.column_stack([y_grid, pdf_avg]))
np.savetxt("PDF_geo.dat", np.column_stack([y_grid, pdf_geo]))
np.savetxt("PDF_log.dat", np.column_stack([y_grid, pdf_y_from_u]))


print("PDF averaged over", pdf_count, "races")
# =========================
# OUTPUT
# =========================

x = np.arange(N) / N

# --- Edata (arithmetic) ---
Edata = np.column_stack([
    x,
    Ey,   # relative
    Et,   # delay
    Es,
])
np.savetxt("Edata.dat", Edata)

# --- Tdata (geometric / relative) ---
Tdata = np.column_stack([
    x,
    Ty, 
    (Tt-1.0)*np.exp(ta),
    Ts,
])
np.savetxt("Tdata.dat", Tdata)

print("Processed", valid_races, "races")