# Undetected & Undertreated

**Racial and Gender Disparities in Hidden Hypoxemia and Their Economic Consequences**

COMP/STAT 212 · Macalester College · Spring 2026

**Authors:** William Acosta Lora · Patricia Escobar Contreras · Nayla Trigueros Ortiz

---

## Overview

This project examines pulse oximeter bias across skin tone, race, and sex using two independent datasets — the UCSF OpenOximetry lab dataset and New York State SPARCS hospital discharge data — to connect clinical misdiagnosis rates to downstream economic consequences for patients of color.

**Live site:** https://mac-stat212-s26.github.io/project-healthcare-william-patricia-nayla/
**Shiny app:** https://1wrn0x-liam-acosta0lora.shinyapps.io/project-healthcare-william-patricia-nayla/

---

## Key Findings

- **Occult hypoxemia rate:** 6.2% for Light skin (Fitzpatrick I–II) vs. 11.6% for Dark skin (V–VI) — nearly double
- **Black females:** 12.6% occult hypoxemia rate, the highest of any race × sex subgroup
- **Charge gap at 70+:** Black patients billed ~$16,000 more than White patients at the same age, with the same insurance, at the same illness severity
- **Bias is systemic:** Present across all device models tested — not a single-manufacturer defect

---

## Bias Correction Tool

The Shiny app includes a **Provider Correction Tool** built on empirical bias parameters from OpenOximetry:

```
Estimated SaO₂  =  SpO₂  −  Group mean bias  −  Sex adjustment
95% CI          =  ±1.96 × SD / √30
```

| Skin tone group    | Mean bias | Occult hypoxemia rate | SD       |
|--------------------|-----------|------------------------|----------|
| Light (I–II)       | +0.62 pp  | 6.2%                   | ±3.21 pp |
| Medium (III–IV)    | +1.08 pp  | 6.6%                   | ±3.45 pp |
| Dark (V–VI)        | +1.82 pp  | 11.6%                  | ±3.89 pp |

Sex adjustment: −0.32 pp applied to Black and Asian female patients based on intersectional analysis.

> This tool is for clinical awareness only. Statistical estimates based on group-level distributions. Clinical judgment and ABG confirmation should always prevail.

---

## Repository Structure

```
project-healthcare-william-patricia-nayla/
├── index.qmd                  # Main report (home page)
├── _quarto.yml                # Site configuration
├── app.R                      # Shiny dashboard + provider correction tool
├── renv.lock                  # Pinned R package versions (managed by renv)
├── style/
│   └── custom.css             # Site stylesheet
├── data/
│   └── raw/                   # OpenOximetry CSVs (not committed — see Data Access)
│       ├── patient.csv
│       ├── encounter.csv
│       ├── pulseoximeter.csv
│       ├── bloodgas.csv
│       └── spectrophotometer.csv
├── eda/
│   ├── eda-william_acosta.qmd
│   ├── eda-patricia.qmd
│   └── eda-nayla_trigueros.qmd
├── appx/
│   ├── app.qmd                # Page that embeds the deployed Shiny app
│   ├── proposal.qmd
│   └── case-study.qmd
├── assets/
│   └── sidebar-logo.png       # Sidebar branding; add other static assets here
└── docs/                      # Rendered site output — committed and served via GitHub Pages
```

---

## Prerequisites

Before running anything locally, make sure you have the following installed:

- [R 4.4.1](https://cran.r-project.org/)
- [Quarto](https://quarto.org/docs/get-started/) (for rendering the site)
- [Git](https://git-scm.com/)
- A terminal or RStudio IDE

Clone the repository:

```bash
git clone https://github.com/mac-stat212-s26/project-healthcare-william-patricia-nayla.git
cd project-healthcare-william-patricia-nayla
```

---

## Data Access

### OpenOximetry (UCSF Hypoxia Lab)

The raw CSVs are **not committed** to this repository due to the PhysioNet Data Use Agreement.

1. Create a free account at https://physionet.org
2. Complete the required DUA training
3. Download **OpenOximetry 1.1.1** from https://physionet.org/content/openox-repo/1.1.1/
4. Place the five CSV files in `data/raw/`

### NY SPARCS

Pulled live from the NY Health Data API — no download required. The data chunk is fetched at render time:

```r
read_csv("https://health.data.ny.gov/resource/tg3i-cinn.csv?$where=ccsr_diagnosis_code%20like%20%27RSP%25%27&$limit=500000")
```

---

## R Package Requirements

This project uses **R 4.4.1** and [`renv`](https://rstudio.github.io/renv/) for reproducible package management. To restore the exact package versions recorded in `renv.lock`, run:

```r
install.packages("renv")
renv::restore()
```

Alternatively, if you prefer to install packages manually without `renv`:

```r
install.packages(c(
  # ── Core data wrangling ──────────────────────────
  "tidyverse",      # ggplot2, dplyr, tidyr, readr, purrr, stringr, forcats
  "janitor",        # clean_names(), tabyl()
  "lubridate",      # date handling

  # ── Visualization ────────────────────────────────
  "scales",         # percent(), dollar(), comma() formatters
  "patchwork",      # combining ggplot panels
  "ggrepel",        # non-overlapping text labels
  "RColorBrewer",   # color palettes

  # ── EDA & summaries ──────────────────────────────
  "visdat",         # vis_dat(), vis_miss()
  "DataExplorer",   # plot_missing(), create_report()
  "gtsummary",      # tbl_summary(), bold_labels()
  "knitr",          # kable() tables

  # ── Shiny app ─────────────────────────────────────
  "shiny",          # core Shiny framework
  "bslib",          # Bootstrap themes, page_navbar(), value_box()
  "bsicons",        # Bootstrap icons for value boxes
  "plotly",         # interactive plots via ggplotly() and plot_ly()
  "DT",             # interactive datatable()

  # ── Deployment ────────────────────────────────────
  "rsconnect"       # deployApp() to shinyapps.io
))
```

For exact versions used during development, see `renv.lock`.

---

## Running Locally

### Render the Quarto site

```bash
# From project root
quarto preview         # live preview in browser
quarto render          # full render to docs/
quarto publish gh-pages --no-browser   # deploy to GitHub Pages
```

> **Speed tip:** Add `freeze: auto` under `execute:` in `_quarto.yml`. Only changed files will re-render on each publish.

### Run the Shiny app

```r
setwd("/path/to/project-healthcare-william-patricia-nayla")
shiny::runApp("app.R")
```

### Deploy the Shiny app to shinyapps.io

```r
rsconnect::deployApp()
```

> **Troubleshooting:** If `rsconnect` fails with an `renv` sync error, temporarily move the lockfile out of the way, deploy, then restore it:
> ```r
> file.rename("renv.lock", "renv.lock.bak")
> rsconnect::deployApp()
> file.rename("renv.lock.bak", "renv.lock")
> ```

---

## License

**Data:** OpenOximetry data is subject to the [PhysioNet Credentialed Health Data License](https://physionet.org/about/licenses/physionet-credentialed-health-data-license-150/). NY SPARCS data is publicly available under New York State open data terms.

**Code:** MIT License — see `LICENSE` for details.
