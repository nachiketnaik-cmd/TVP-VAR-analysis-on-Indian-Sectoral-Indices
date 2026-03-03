# TVP-VAR Connectedness Analysis of Indian Sectoral Indices

> Examining return and volatility spillovers across NSE sector indices using Time-Varying Parameter VAR and the Diebold-Yilmaz connectedness framework — with oil market structural shocks as key driving variables.

---

## 📌 Overview

This project investigates the **dynamic connectedness** among major Indian stock market sectoral indices listed on the **National Stock Exchange (NSE)**, incorporating the role of **global oil market structural shocks** (demand, supply, and risk shocks). The analysis spans both **return spillovers** and **volatility spillovers**, employing a combination of static and dynamic econometric frameworks.

The study contributes to the growing literature on financial contagion, sector-level integration, and the transmission of commodity market shocks to equity markets in emerging economies — with a specific focus on India.

---

## 🎯 Research Objectives

- Measure **static and dynamic connectedness** across Indian NSE sectoral indices
- Assess the role of **oil demand shocks, supply shocks, and risk shocks** in transmitting spillovers to equity sectors
- Identify which sectors are net **transmitters** vs. net **receivers** of shocks
- Examine **pairwise directional connectedness** between oil shocks and individual sectors
- Validate findings using the **TVP-VAR** model as a robustness check against the rolling-window VAR
- Explore **frequency-domain connectedness** following Baruník & Krehlík (2018)

---

## 📊 Data

### Sectoral Indices (NSE)
| Sector | Variable |
|---|---|
| Automobile | NSE AUTO |
| Banking | NSE BANK |
| Energy | NSE ENERGY |
| Financial Services | NSE FIN |
| FMCG | NSE FMCG |
| Information Technology | NSE IT |
| Media | NSE MED |
| Metals | NSE MET |
| Pharmaceuticals | NSE PHARMA |

### Oil Market Structural Shocks
| Variable | Description |
|---|---|
| `Demand_Shocks` | Global oil demand shocks |
| `Supply_Shocks` | Global oil supply shocks |
| `Risk_Shocks` | Oil market risk/uncertainty shocks |

- **Sample Period:** January 2008 – present (~4,066 daily observations)
- **Frequency:** Daily
- **Format:** Log returns (continuously compounded)

### Files
| File | Description |
|---|---|
| `Data.csv` | Full dataset with all variables |
| `Data_x.csv` | Processed return data used in VAR estimation |
| `Data_r.csv` | Alternative dataset with broader sector coverage |
| `Static_Connectedness_Return.csv` | Full-sample static connectedness table (returns) |
| `Static_Connectedness_Volatility.csv` | Full-sample static connectedness table (volatility) |
| `Dynamic_Connectedness_Return.csv` | Rolling-window dynamic connectedness (returns) |
| `Dynamic_Connectedness_Return_TVPVAR.csv` | TVP-VAR dynamic connectedness (returns) |
| `Dynamic_Connectedness_Volatility.csv` | Rolling-window dynamic connectedness (volatility) |

---

## 🔧 Methodology

### 1. Static Connectedness (Diebold & Yilmaz, 2012)
A fixed-parameter VAR(1) model is estimated over the full sample. The **Generalized Forecast Error Variance Decomposition (GFEVD)** is computed at a 10-step ahead forecast horizon to construct the connectedness table, which reports:
- **TO**: Total directional connectedness transmitted to all others
- **FROM**: Total directional connectedness received from all others
- **NET**: Net transmitter/receiver status (`TO − FROM`)
- **TCI**: Total Connectedness Index (system-wide measure)
- **NPSO**: Net Pairwise Directional Connectedness

### 2. Dynamic Connectedness (Rolling-Window VAR)
A **200-day rolling window VAR(1)** is estimated to capture time-varying spillovers. This generates dynamic series for `TO`, `FROM`, `NET`, `TCI`, and `NPSO`, allowing identification of spillover surges around crisis episodes (e.g., GFC, COVID-19, oil price crashes).

### 3. TVP-VAR Connectedness (Primiceri, 2005; Antonakakis et al., 2020)
As the primary methodology and robustness check, the **Time-Varying Parameter VAR (TVP-VAR)** with **Minnesota Prior** and forgetting factors `κ₁ = 0.99`, `κ₂ = 0.99` is estimated. This approach:
- Avoids the arbitrary choice of rolling window size
- Uses all available data at each point in time
- Produces smoother and more efficient time-varying connectedness estimates

### 4. Frequency Connectedness (Baruník & Krehlík, 2018)
Connectedness is decomposed into **frequency bands** to distinguish short-run from long-run spillovers, following the spectral decomposition of the GFEVD. Partition used: `[π, π/4, 0]`.

### 5. Asymmetric Connectedness (Baruník et al., 2016)
Using the `SI'N_MASTER.R` script, returns are decomposed into **positive (Yp)** and **negative (Yn)** components to examine asymmetric transmission — testing whether bad news (negative returns) generates stronger spillovers than good news.

---

## 📁 Repository Structure

```
├── Data.csv                                  # Main dataset
├── Data_x.csv                                # Processed returns for VAR
├── Data_r.csv                                # Extended sector dataset
├── Data_x.xlsx                               # Excel version of data
├── r.mat                                     # MATLAB data file (for Wavelets)
│
├── DY_ret.R                                  # Main script: DY + TVP-VAR return spillovers
├── SI'N_MASTER.R                             # Asymmetric connectedness analysis
├── Frequency connectedness approach(2018).R  # Frequency-domain connectedness
├── figures.R                                 # Publication-quality figures
├── test.R / test2.R                          # Exploratory / testing scripts
├── Wavelets.m                                # MATLAB wavelet analysis
│
├── functions.R                               # Core VAR, GFEVD, DCA functions
├── function_tvpvar.R                         # TVP-VAR estimation functions
│
├── Static_Connectedness_Return.csv           # Output: static return connectedness
├── Static_Connectedness_Volatility.csv       # Output: static volatility connectedness
├── Dynamic_Connectedness_Return.csv          # Output: dynamic return connectedness
├── Dynamic_Connectedness_Return_TVPVAR.csv   # Output: TVP-VAR return connectedness
├── Dynamic_Connectedness_Volatility.csv      # Output: dynamic volatility connectedness
│
└── returns graphs.docx                       # Figures/graphs document
```

---

## ⚙️ Requirements

### R Packages
```r
install.packages(c(
  "ConnectednessApproach",  # Main connectedness framework
  "vars",                   # VAR estimation
  "MASS",                   # Matrix operations
  "rugarch",                # GARCH for volatility
  "quantmod",               # Financial data
  "xts", "zoo",             # Time series
  "dplyr", "data.table",    # Data manipulation
  "readr",                  # CSV reading
  "psych",                  # Descriptive statistics
  "knitr", "kableExtra",    # Table formatting
  "PerformanceAnalytics"    # Return analysis
))
```

### MATLAB (optional)
- Required only for `Wavelets.m`
- Wavelet Toolbox recommended

---

## 🚀 How to Run

### Step 1: Prepare Data
Ensure `Data_x.csv` is present in the working directory. This contains daily returns for all oil shock variables and NSE sectoral indices.

### Step 2: Run Main Return Spillover Analysis
```r
source("functions.R")
source("function_tvpvar.R")
source("DY_ret.R")
```
This will:
- Estimate static VAR connectedness and save to `Static_Connectedness_Return.csv`
- Run the 200-day rolling-window dynamic connectedness
- Estimate TVP-VAR and save to `Dynamic_Connectedness_Return_TVPVAR.csv`
- Plot all directional connectedness figures

### Step 3: Asymmetric Connectedness
```r
source("SI'N_MASTER.R")
```

### Step 4: Frequency Connectedness
```r
source("Frequency connectedness approach(2018).R")
```

### Step 5: Generate Figures
```r
source("figures.R")
```

---

## 📈 Key Outputs

- **Total Connectedness Index (TCI):** System-wide spillover intensity over time
- **Directional TO/FROM plots:** Each sector's role as transmitter or receiver
- **NET connectedness plots:** Whether sectors are net exporters or importers of shocks
- **NPSO plots:** Bilateral relationships between oil shocks and each sector
- **VAR vs. TVP-VAR comparison:** Robustness of dynamic connectedness estimates

---

## 📚 References

- Diebold, F. X., & Yilmaz, K. (2012). Better to give than to receive: Predictive directional measurement of volatility spillovers. *International Journal of Forecasting*, 28(1), 57–66.
- Antonakakis, N., Chatziantoniou, I., & Gabauer, D. (2020). Refined measures of dynamic connectedness based on time-varying parameter vector autoregressions. *Journal of Risk and Financial Management*, 13(4), 84.
- Baruník, J., & Krehlík, T. (2018). Measuring the frequency dynamics of financial connectedness and systemic risk. *Journal of Financial Econometrics*, 16(2), 271–296.
- Primiceri, G. E. (2005). Time varying structural vector autoregressions and monetary policy. *Review of Economic Studies*, 72(3), 821–852.
- Kilian, L. (2009). Not all oil price shocks are alike: Disentangling demand and supply shocks in the crude oil market. *American Economic Review*, 99(3), 1053–1069.

---

## 👤 Author

**[Your Name]**
Department of [Your Department]
[Your Institution]

---

## 📄 License

This repository is intended for academic and research purposes. Please cite appropriately if you use any part of this code or methodology.
