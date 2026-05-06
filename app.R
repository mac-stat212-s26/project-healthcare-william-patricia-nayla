library(shiny)
library(tidyverse)
library(scales)
library(bslib)
library(plotly)
library(DT)

# ── EMPIRICAL VALUES FROM EDA ─────────────────────────────────────────────────
bias_params <- tibble(
  skin_group  = c("Light (I–II)", "Medium (III–IV)", "Dark (V–VI)"),
  mean_bias   = c(0.62, 1.08, 1.82),
  sd_bias     = c(3.21, 3.45, 3.89),
  occult_rate = c(0.062, 0.066, 0.116)
)

sex_race_oh <- tibble(
  race_eth     = rep(c("White", "Hispanic/Latino", "Asian", "Black"), each = 2),
  assigned_sex = rep(c("Female", "Male"), 4),
  oh_rate      = c(0.058, 0.063, 0.069, 0.063, 0.076, 0.068, 0.126, 0.100)
)

sparcs_age <- tibble(
  race_eth       = rep(c("Black", "Hispanic/Latino", "White"), 3),
  age_group      = rep(c("18–49", "50–69", "70+"), each = 3),
  median_charges = c(29155, 29921, 28700, 37102, 40045, 31870, 51944, 48671, 35915),
  mean_severity  = c(2.32, 2.22, 2.36, 2.66, 2.58, 2.68, 2.78, 2.69, 2.79),
  pct_medicaid   = c(0.64, 0.70, 0.39, 0.45, 0.51, 0.19, 0.04, 0.10, 0.01),
  mortality_rate = c(0.021, 0.018, 0.019, 0.038, 0.032, 0.035, 0.082, 0.071, 0.078),
  median_los     = c(3.0, 2.0, 3.0, 4.0, 4.0, 4.0, 5.0, 5.0, 5.0)
)

bridge <- tibble(
  race_eth              = c("White", "Hispanic/Latino", "Black"),
  occult_hypoxemia_rate = c(0.061, 0.066, 0.105),
  mean_bias             = c(0.58, 0.71, 1.24),
  median_charges        = c(32940, 34051, 34263),
  pct_unprotected       = c(0.19, 0.55, 0.44),
  mean_severity         = c(2.61, 2.44, 2.52)
)

device_bias <- tibble(
  device_label = rep(paste0("Model ", c(21, 59, 60, 65, 73, 77)), each = 3),
  skin_group   = rep(c("Light (I–II)", "Medium (III–IV)", "Dark (V–VI)"), 6),
  mean_bias    = c(0.31,0.74,1.20, 0.58,1.02,1.95, 0.44,0.88,1.65,
                   0.72,1.28,2.31, 0.52,0.98,1.78, 0.41,0.85,1.55)
)

# ── PALETTE ───────────────────────────────────────────────────────────────────
NAVY  <- "#0D1F35"
TEAL  <- "#028090"
CREAM <- "#F4F7F9"
RED   <- "#D94F3D"
GOLD  <- "#E8A838"

skin_pal <- c("Light (I–II)" = "#E8C97A", "Medium (III–IV)" = "#B06840", "Dark (V–VI)" = "#4A1E0E")

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = tags$span(
    tags$span("Undetected & Undertreated", style = "font-weight:800;color:#028090;"),
    tags$span(" | Hidden Hypoxemia Dashboard", style = "color:#64748B;font-size:0.85em;")
  ),
  theme = bs_theme(
    bg = CREAM, fg = NAVY, primary = TEAL, secondary = GOLD,
    base_font    = font_google("DM Sans"),
    heading_font = font_google("DM Serif Display"),
    bootswatch   = "flatly"
  ),
  fillable = TRUE,

  # ── TAB 1: DASHBOARD ────────────────────────────────────────────────────────
  nav_panel(
    title = "Dashboard",
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(
        title = "Occult hypoxemia — Dark skin", value = "11.6%",
        showcase = bsicons::bs_icon("exclamation-triangle-fill"), theme = "danger",
        p("vs. 6.2% for Light skin — nearly double")
      ),
      value_box(
        title = "Black patients — median charges at 70+", value = "$51,944",
        showcase = bsicons::bs_icon("cash-stack"), theme = "warning",
        p("$16K more than White patients at the same age")
      ),
      value_box(
        title = "Black females — occult hypoxemia rate", value = "12.6%",
        showcase = bsicons::bs_icon("person-fill-exclamation"), theme = "danger",
        p("Highest of any race × sex subgroup")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Occult Hypoxemia Rate by Skin Tone"),
           plotlyOutput("plot_occult_skin", height = "300px")),
      card(card_header("Pulse Oximeter Bias by Skin Tone"),
           plotlyOutput("plot_bias_violin", height = "300px"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Occult Hypoxemia Rate by Race and Sex"),
           plotlyOutput("plot_sex_race", height = "320px")),
      card(card_header("Device Bias by Model and Skin Tone"),
           plotlyOutput("plot_device_heat", height = "320px"))
    )
  ), # end TAB 1

  # ── TAB 2: ECONOMIC BURDEN ───────────────────────────────────────────────────
  nav_panel(
    title = "Economic Burden",
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Median Hospital Charges by Age Group and Race"),
        p("Black and Hispanic/Latino patients are billed more at every age band.",
          class = "text-muted px-2 pt-1"),
        plotlyOutput("plot_charges_age", height = "380px")
      )
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      card(card_header("Medicaid Share by Age and Race"),
           plotlyOutput("plot_medicaid", height = "300px")),
      card(card_header("Mean Severity at Admission"),
           plotlyOutput("plot_severity", height = "300px")),
      card(card_header("In-Hospital Mortality Rate"),
           plotlyOutput("plot_mortality", height = "300px"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("The Double Burden: Misdiagnosis Risk × Financial Vulnerability"),
        plotlyOutput("plot_double_burden", height = "340px")
      ),
      card(
        card_header("The 70+ Convergence — The Strongest Evidence"),
        fill = FALSE,
        div(
          class = "p-3",
          div(
            class = "d-flex gap-3 mb-3",
            div(
              class = "rounded p-3 text-center flex-fill",
              style = paste0("background:", TEAL, ";color:white;"),
              tags$div(style = "font-size:0.75em;font-weight:700;letter-spacing:2px;", "INSURANCE CONVERGES"),
              tags$div(style = "font-size:2em;font-weight:800;margin:6px 0;", "1% / 4% / 10%"),
              tags$div(style = "font-size:0.8em;", "White / Black / Hispanic on Medicaid at 70+")
            ),
            div(
              class = "rounded p-3 text-center flex-fill",
              style = paste0("background:", GOLD, ";color:white;"),
              tags$div(style = "font-size:0.75em;font-weight:700;letter-spacing:2px;", "SEVERITY CONVERGES"),
              tags$div(style = "font-size:2em;font-weight:800;margin:6px 0;", "2.79 / 2.78 / 2.69"),
              tags$div(style = "font-size:0.8em;", "White / Black / Hispanic APR score at 70+")
            ),
            div(
              class = "rounded p-3 text-center flex-fill",
              style = paste0("background:", RED, ";color:white;"),
              tags$div(style = "font-size:0.75em;font-weight:700;letter-spacing:2px;", "CHARGE GAP WIDENS"),
              tags$div(style = "font-size:2em;font-weight:800;margin:6px 0;", "$16K more"),
              tags$div(style = "font-size:0.8em;", "Black vs. White at 70+ — same insurance, same severity")
            )
          ),
          div(
            class = "alert alert-danger",
            tags$strong("Key finding: "),
            "When both insurance and severity equalize, the charge gap reaches its maximum.
             Consistent with a pre-admission clinical mechanism — delayed treatment from
             undetected hypoxemia inflating true disease burden."
          )
        )
      )
    )
  ), # end TAB 2

  # ── TAB 3: PROVIDER TOOL ─────────────────────────────────────────────────────
  nav_panel(
    title = "Provider Correction Tool",
    layout_columns(
      col_widths = c(4, 8),

      # LEFT: inputs
      card(
        card_header("Patient Information"),
        fill = FALSE,
        div(
          class = "p-3",
          div(
            class = "alert mb-3",
            style = paste0("background:", TEAL, "22;border-left:4px solid ", TEAL, ";color:", NAVY),
            tags$strong("Clinical guidance: "),
            "Pulse oximeters overestimate oxygen saturation in patients with darker
             skin tones. Enter the reading and patient demographics to estimate
             the likely true SaO₂ and occult hypoxemia risk."
          ),
          numericInput("spo2_input",
                       label = tags$span("Pulse oximeter reading (SpO₂ %)", style = "font-weight:600;"),
                       value = 94, min = 70, max = 100, step = 0.5
          ),
          selectInput("skin_group_input",
                      label = tags$span("Fitzpatrick skin tone group", style = "font-weight:600;"),
                      choices = c("Light (I–II)", "Medium (III–IV)", "Dark (V–VI)"),
                      selected = "Dark (V–VI)"
          ),
          selectInput("race_input",
                      label = tags$span("Self-reported race / ethnicity", style = "font-weight:600;"),
                      choices = c("White", "Hispanic/Latino", "Asian", "Black"),
                      selected = "Black"
          ),
          selectInput("sex_input",
                      label = tags$span("Assigned sex at birth", style = "font-weight:600;"),
                      choices = c("Female", "Male"), selected = "Female"
          ),
          hr(),
          tags$small(
            class = "text-muted",
            "Bias estimates from OpenOximetry 1.1.1 (UCSF Hypoxia Lab).
             Statistical estimates only — clinical judgment should always prevail."
          )
        )
      ), # end LEFT card

      # RIGHT: outputs wrapped in a single div
      div(
        layout_columns(
          col_widths = c(4, 4, 4),
          uiOutput("box_estimated_sao2"),
          uiOutput("box_occult_risk"),
          uiOutput("box_clinical_flag")
        ),
        br(),
        layout_columns(
          col_widths = c(7, 5),
          card(
            card_header("Estimated True SaO₂ with Uncertainty Range"),
            plotlyOutput("plot_correction_gauge", height = "280px")
          ),
          card(
            card_header("Clinical Interpretation"),
            fill = FALSE,
            uiOutput("clinical_interpretation")
          )
        ),
        br(),
        card(
          card_header("Bias Distribution: Where Does This Patient Fall?"),
          plotlyOutput("plot_bias_context", height = "220px")
        )
      ) # end RIGHT div

    ) # end layout_columns
  ), # end TAB 3

  # ── TAB 4: DATA ──────────────────────────────────────────────────────────────
  nav_panel(
    title = "Data",
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Race-Level Bridge: Clinical Disparity → Economic Burden"),
        p("Ecological association across two independent populations —
           not a patient-level causal link.", class = "text-muted px-2 pt-1"),
        DTOutput("bridge_table")
      )
    ),
    layout_columns(
      col_widths = c(12),
      card(
        card_header("Age-Stratified Outcomes by Race — SPARCS (103,907 records)"),
        DTOutput("age_table")
      )
    )
  ) # end TAB 4

) # end page_navbar

# ── SERVER ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  plotly_cfg <- function(p) {
    p |> config(displayModeBar = FALSE) |>
      layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  }

  # ── DASHBOARD ────────────────────────────────────────────────────────────────

  output$plot_occult_skin <- renderPlotly({
    bias_params |>
      mutate(skin_group = factor(skin_group, levels = c("Light (I–II)", "Medium (III–IV)", "Dark (V–VI)"))) |>
      plot_ly(x = ~skin_group, y = ~occult_rate, type = "bar",
              marker = list(color = c("#E8C97A", "#B06840", "#4A1E0E")),
              text = ~paste0(round(occult_rate * 100, 1), "%"),
              textposition = "outside",
              hovertemplate = "<b>%{x}</b><br>Rate: %{y:.1%}<extra></extra>") |>
      layout(yaxis = list(tickformat = ".0%", title = "Rate", gridcolor = "#E2E8F0"),
             xaxis = list(title = ""), showlegend = FALSE) |>
      plotly_cfg()
  })

  output$plot_bias_violin <- renderPlotly({
    set.seed(42)
    sim_data <- bias_params |>
      rowwise() |>
      mutate(samples = list(rnorm(2000, mean_bias, sd_bias))) |>
      unnest(samples) |>
      filter(between(samples, -15, 15)) |>
      rename(bias = samples)

    plot_ly(sim_data, type = "violin") |>
      add_trace(x = ~skin_group, y = ~bias, split = ~skin_group,
                box = list(visible = TRUE), meanline = list(visible = TRUE),
                colors = skin_pal,
                hovertemplate = "<b>%{x}</b><br>Bias: %{y:.2f} pp<extra></extra>") |>
      layout(yaxis = list(title = "Bias (pp)", gridcolor = "#E2E8F0"),
             xaxis = list(title = ""), showlegend = FALSE,
             shapes = list(list(type = "line", x0 = -0.5, x1 = 2.5, y0 = 0, y1 = 0,
                                line = list(color = "grey", dash = "dash", width = 1)))) |>
      plotly_cfg()
  })

  output$plot_sex_race <- renderPlotly({
    sex_race_oh |>
      mutate(race_eth     = factor(race_eth, levels = c("White", "Hispanic/Latino", "Asian", "Black")),
             assigned_sex = factor(assigned_sex, levels = c("Female", "Male"))) |>
      plot_ly(x = ~race_eth, y = ~oh_rate, color = ~assigned_sex,
              colors = c("Female" = "#C2185B", "Male" = "#1565C0"),
              type = "bar", barmode = "group",
              text = ~paste0(round(oh_rate * 100, 1), "%"),
              textposition = "outside",
              hovertemplate = "<b>%{x} — %{fullData.name}</b><br>Rate: %{y:.1%}<extra></extra>") |>
      layout(yaxis = list(tickformat = ".0%", title = "Occult hypoxemia rate", gridcolor = "#E2E8F0"),
             xaxis = list(title = ""),
             legend = list(title = list(text = "Assigned sex"))) |>
      plotly_cfg()
  })

  output$plot_device_heat <- renderPlotly({
    device_bias |>
      mutate(skin_group = factor(skin_group, levels = c("Light (I–II)", "Medium (III–IV)", "Dark (V–VI)"))) |>
      plot_ly(x = ~skin_group, y = ~device_label, z = ~mean_bias, type = "heatmap",
              colorscale = list(c(0,"blue"), c(0.5,"white"), c(1,"red")),
              zmid = 0, zmin = -2, zmax = 4,
              text = ~sprintf("%+.2f pp", mean_bias), texttemplate = "%{text}",
              hovertemplate = "<b>%{y} — %{x}</b><br>Bias: %{z:.2f} pp<extra></extra>",
              colorbar = list(title = "Bias (pp)")) |>
      layout(xaxis = list(title = "Skin tone group"), yaxis = list(title = "")) |>
      plotly_cfg()
  })

  # ── ECONOMIC BURDEN ───────────────────────────────────────────────────────────

  output$plot_charges_age <- renderPlotly({
    sparcs_age |>
      mutate(race_eth  = factor(race_eth,  levels = c("Black", "Hispanic/Latino", "White")),
             age_group = factor(age_group, levels = c("18–49", "50–69", "70+"))) |>
      plot_ly(x = ~age_group, y = ~median_charges, color = ~race_eth,
              colors = c("Black" = RED, "Hispanic/Latino" = GOLD, "White" = "#5B7FA6"),
              type = "scatter", mode = "lines+markers+text",
              text = ~dollar(median_charges, accuracy = 1), textposition = "top center",
              line = list(width = 3), marker = list(size = 10),
              hovertemplate = "<b>%{fullData.name}</b><br>%{x}: %{y:$,.0f}<extra></extra>") |>
      layout(yaxis = list(tickformat = "$,.0f", title = "Median charges", gridcolor = "#E2E8F0"),
             xaxis = list(title = "Age group"),
             legend = list(title = list(text = "Race / Ethnicity"))) |>
      plotly_cfg()
  })

  output$plot_medicaid <- renderPlotly({
    sparcs_age |>
      mutate(race_eth = factor(race_eth, levels = c("Black","Hispanic/Latino","White")),
             age_group = factor(age_group, levels = c("18–49","50–69","70+"))) |>
      plot_ly(x = ~age_group, y = ~pct_medicaid, color = ~race_eth,
              colors = c("Black"=RED,"Hispanic/Latino"=GOLD,"White"="#5B7FA6"),
              type="scatter", mode="lines+markers",
              line=list(width=2.5), marker=list(size=8),
              hovertemplate="<b>%{fullData.name}</b><br>%{x}: %{y:.0%}<extra></extra>") |>
      layout(yaxis=list(tickformat=".0%",title="% on Medicaid",gridcolor="#E2E8F0"),
             xaxis=list(title=""), showlegend=FALSE) |>
      plotly_cfg()
  })

  output$plot_severity <- renderPlotly({
    sparcs_age |>
      mutate(race_eth = factor(race_eth, levels = c("Black","Hispanic/Latino","White")),
             age_group = factor(age_group, levels = c("18–49","50–69","70+"))) |>
      plot_ly(x = ~age_group, y = ~mean_severity, color = ~race_eth,
              colors = c("Black"=RED,"Hispanic/Latino"=GOLD,"White"="#5B7FA6"),
              type="scatter", mode="lines+markers",
              line=list(width=2.5), marker=list(size=8),
              hovertemplate="<b>%{fullData.name}</b><br>%{x}: %{y:.2f}<extra></extra>") |>
      layout(yaxis=list(title="Mean APR severity (1–4)",gridcolor="#E2E8F0",range=c(1.8,3.1)),
             xaxis=list(title=""), showlegend=FALSE) |>
      plotly_cfg()
  })

  output$plot_mortality <- renderPlotly({
    sparcs_age |>
      mutate(race_eth = factor(race_eth, levels = c("Black","Hispanic/Latino","White")),
             age_group = factor(age_group, levels = c("18–49","50–69","70+"))) |>
      plot_ly(x = ~age_group, y = ~mortality_rate, color = ~race_eth,
              colors = c("Black"=RED,"Hispanic/Latino"=GOLD,"White"="#5B7FA6"),
              type="scatter", mode="lines+markers",
              line=list(width=2.5), marker=list(size=8),
              hovertemplate="<b>%{fullData.name}</b><br>%{x}: %{y:.1%}<extra></extra>") |>
      layout(yaxis=list(tickformat=".1%",title="Mortality rate",gridcolor="#E2E8F0"),
             xaxis=list(title=""), showlegend=FALSE) |>
      plotly_cfg()
  })

  output$plot_double_burden <- renderPlotly({
    bridge |>
      plot_ly(x = ~occult_hypoxemia_rate, y = ~pct_unprotected,
              size = ~median_charges, color = ~race_eth,
              colors = c("Black"=RED,"Hispanic/Latino"=GOLD,"White"="#5B7FA6"),
              type="scatter", mode="markers+text",
              text = ~race_eth, textposition = "top center",
              marker = list(sizemode="diameter", sizeref=0.05, opacity=0.85),
              hovertemplate = paste0("<b>%{text}</b><br>",
                                     "Hypoxemia rate: %{x:.1%}<br>% Medicaid/Self-Pay: %{y:.0%}<extra></extra>")) |>
      layout(xaxis=list(tickformat=".0%",title="Occult hypoxemia rate (OpenOximetry)",gridcolor="#E2E8F0"),
             yaxis=list(tickformat=".0%",title="% Medicaid or Self-Pay (SPARCS)",gridcolor="#E2E8F0"),
             showlegend=FALSE) |>
      plotly_cfg()
  })

  # ── PROVIDER TOOL ─────────────────────────────────────────────────────────────

  correction <- reactive({
    skin   <- input$skin_group_input
    race   <- input$race_input
    sex    <- input$sex_input
    spo2   <- input$spo2_input
    params <- bias_params |> filter(skin_group == skin)
    sex_adj <- if (race %in% c("Black","Asian") && sex == "Female") 0.32 else 0
    est_sao2 <- spo2 - params$mean_bias - sex_adj
    ci_lo    <- est_sao2 - 1.96 * params$sd_bias / sqrt(30)
    ci_hi    <- est_sao2 + 1.96 * params$sd_bias / sqrt(30)
    prob_occ <- params$occult_rate * if (race %in% c("Black","Asian") && sex == "Female") 1.18 else 1
    prob_occ <- min(prob_occ, 0.95)
    list(spo2 = spo2, est_sao2 = round(est_sao2,1),
         ci_lo = round(ci_lo,1), ci_hi = round(ci_hi,1),
         mean_bias = round(params$mean_bias + sex_adj, 2),
         sd_bias = round(params$sd_bias, 2),
         prob_occult = round(prob_occ, 3),
         threshold_88 = spo2 >= 88 && est_sao2 < 88,
         threshold_92 = spo2 >= 92 && est_sao2 < 92)
  })

  output$box_estimated_sao2 <- renderUI({
    cr <- correction()
    col <- if (cr$est_sao2 < 88) "danger" else if (cr$est_sao2 < 92) "warning" else "success"
    value_box(title = "Estimated True SaO₂", value = paste0(cr$est_sao2, "%"),
              showcase = bsicons::bs_icon("droplet-fill"), theme = col,
              p(paste0("95% CI: ", cr$ci_lo, "–", cr$ci_hi, "%")),
              p(paste0("Correction: −", cr$mean_bias, " pp")))
  })

  output$box_occult_risk <- renderUI({
    cr  <- correction()
    col <- if (cr$prob_occult > 0.09) "danger" else if (cr$prob_occult > 0.06) "warning" else "success"
    value_box(title = "Occult Hypoxemia Risk", value = percent(cr$prob_occult, accuracy = 0.1),
              showcase = bsicons::bs_icon("exclamation-triangle"), theme = col,
              p("Probability device is masking true hypoxia"))
  })

  output$box_clinical_flag <- renderUI({
    cr <- correction()
    if (cr$threshold_88) {
      value_box(title = "Threshold Alert", value = "MASKED",
                showcase = bsicons::bs_icon("shield-fill-exclamation"), theme = "danger",
                p("Device reads ≥88% but estimated SaO₂ < 88%."),
                p("Patient may need supplemental oxygen."))
    } else if (cr$threshold_92) {
      value_box(title = "Clinical Flag", value = "MONITOR",
                showcase = bsicons::bs_icon("shield-fill-check"), theme = "warning",
                p("Device reads ≥92% but estimated SaO₂ < 92%."),
                p("Consider ABG confirmation."))
    } else {
      value_box(title = "Clinical Flag", value = "WITHIN RANGE",
                showcase = bsicons::bs_icon("shield-fill-check"), theme = "success",
                p("Estimated SaO₂ above key thresholds."),
                p("Continue monitoring."))
    }
  })

  output$plot_correction_gauge <- renderPlotly({
    cr <- correction()
    plot_ly() |>
      add_trace(type="scatter", mode="none",
                x=c(70,88,88,70), y=c(0,0,1,1), fill="toself",
                fillcolor="rgba(211,47,47,0.12)", line=list(color="transparent"),
                name="Hypoxic zone") |>
      add_trace(type="scatter", mode="none",
                x=c(88,92,92,88), y=c(0,0,1,1), fill="toself",
                fillcolor="rgba(232,168,56,0.12)", line=list(color="transparent"),
                name="Borderline zone") |>
      add_trace(type="scatter", mode="none",
                x=c(92,100,100,92), y=c(0,0,1,1), fill="toself",
                fillcolor="rgba(2,192,154,0.10)", line=list(color="transparent"),
                name="Normal zone") |>
      add_trace(type="scatter", mode="none",
                x=c(cr$ci_lo,cr$ci_hi,cr$ci_hi,cr$ci_lo), y=c(0.25,0.25,0.75,0.75),
                fill="toself", fillcolor="rgba(2,128,144,0.25)",
                line=list(color="transparent"), name="95% CI") |>
      add_trace(type="scatter", mode="lines",
                x=c(cr$spo2,cr$spo2), y=c(0.05,0.95),
                line=list(color=GOLD, width=3, dash="dash"),
                name=paste0("SpO₂ reading: ", cr$spo2, "%")) |>
      add_trace(type="scatter", mode="lines",
                x=c(cr$est_sao2,cr$est_sao2), y=c(0.05,0.95),
                line=list(color=NAVY, width=3),
                name=paste0("Est. SaO₂: ", cr$est_sao2, "%")) |>
      add_trace(type="scatter", mode="lines",
                x=c(88,88), y=c(0,1),
                line=list(color=RED, width=1.5, dash="dot"), showlegend=FALSE) |>
      add_trace(type="scatter", mode="lines",
                x=c(92,92), y=c(0,1),
                line=list(color=GOLD, width=1.5, dash="dot"), showlegend=FALSE) |>
      layout(xaxis=list(title="Oxygen saturation (%)", range=c(78,100), gridcolor="#E2E8F0"),
             yaxis=list(showticklabels=FALSE, showgrid=FALSE, zeroline=FALSE, title=""),
             legend=list(orientation="h", y=-0.2),
             annotations=list(
               list(x=79,y=0.5,text="Hypoxic",showarrow=FALSE,font=list(color=RED,size=10)),
               list(x=90,y=0.5,text="Border",showarrow=FALSE,font=list(color=GOLD,size=10)),
               list(x=96,y=0.5,text="Normal",showarrow=FALSE,font=list(color=TEAL,size=10)))) |>
      plotly_cfg()
  })

  output$clinical_interpretation <- renderUI({
    cr   <- correction()
    skin <- input$skin_group_input
    race <- input$race_input
    sex  <- input$sex_input
    col  <- if (cr$threshold_88) "danger" else if (cr$threshold_92) "warning" else "success"
    body <- if (cr$threshold_88) {
      tags$div(tags$p(tags$strong("Consider immediate action:")),
               tags$ul(tags$li("Device may be masking true hypoxia"),
                       tags$li("Consider arterial blood gas (ABG)"),
                       tags$li(paste0("Patient may be eligible for O₂ (est. SaO₂ ", cr$est_sao2, "%)")),
                       tags$li("Do not discharge on device reading alone")))
    } else if (cr$threshold_92) {
      tags$div(tags$p(tags$strong("⚡ Elevated caution:")),
               tags$ul(tags$li("Est. SaO₂ below 92% despite device reading"),
                       tags$li("Consider closer monitoring and possible ABG")))
    } else {
      tags$div(tags$p(tags$strong("Within range after correction:")),
               tags$ul(tags$li(paste0("Est. SaO₂ ", cr$est_sao2, "% above key thresholds")),
                       tags$li("Continue routine monitoring")))
    }
    div(class="p-3",
        div(class=paste0("alert alert-", col), body),
        hr(),
        tags$small(
          tags$strong("Correction applied:"), tags$br(),
          paste0("Skin tone (", skin, "): −", bias_params$mean_bias[bias_params$skin_group == skin], " pp"),
          tags$br(),
          if (race %in% c("Black","Asian") && sex == "Female")
            paste0("Sex adjustment (", sex, " ", race, "): −0.32 pp")
          else
            "No additional sex adjustment",
          tags$br(), tags$br(),
          tags$em("OpenOximetry 1.1.1 (UCSF). Statistical estimates only.")
        ))
  })

  output$plot_bias_context <- renderPlotly({
    cr     <- correction()
    skin   <- input$skin_group_input
    params <- bias_params |> filter(skin_group == skin)
    x_vals <- seq(-15, 15, 0.1)
    y_vals <- dnorm(x_vals, mean = params$mean_bias, sd = params$sd_bias)
    plot_ly() |>
      add_trace(x=x_vals, y=y_vals, type="scatter", mode="lines",
                fill="tozeroy", fillcolor="rgba(2,128,144,0.15)",
                line=list(color=TEAL, width=2),
                name=paste0("Bias dist — ", skin)) |>
      add_trace(x=c(params$mean_bias,params$mean_bias), y=c(0,max(y_vals)*0.9),
                type="scatter", mode="lines",
                line=list(color=NAVY, width=2, dash="dash"),
                name=paste0("Group mean: +", params$mean_bias, " pp")) |>
      add_trace(x=c(0,0), y=c(0,max(y_vals)),
                type="scatter", mode="lines",
                line=list(color="grey", width=1, dash="dot"),
                name="Zero bias (perfect device)") |>
      layout(xaxis=list(title="Bias (SpO₂ − SaO₂, pp)", gridcolor="#E2E8F0", range=c(-10,12)),
             yaxis=list(title="Density", showticklabels=FALSE, gridcolor="#E2E8F0"),
             legend=list(orientation="h", y=-0.3),
             annotations=list(list(x=params$mean_bias, y=max(y_vals),
                                   text=paste0("Mean: +", params$mean_bias," pp"),
                                   showarrow=TRUE, arrowhead=2, ax=40, ay=-30,
                                   font=list(size=10, color=NAVY)))) |>
      plotly_cfg()
  })

  # ── DATA TABLES ───────────────────────────────────────────────────────────────

  output$bridge_table <- renderDT({
    bridge |>
      mutate(occult_hypoxemia_rate = percent(occult_hypoxemia_rate, accuracy=0.1),
             mean_bias    = paste0("+", round(mean_bias,2), " pp"),
             median_charges = dollar(median_charges),
             pct_unprotected = percent(pct_unprotected, accuracy=0.1),
             mean_severity = round(mean_severity,2)) |>
      rename("Race / Ethnicity"="race_eth", "Occult hypoxemia rate"="occult_hypoxemia_rate",
             "Mean SpO₂ bias"="mean_bias", "Median charges"="median_charges",
             "% Medicaid/Self-Pay"="pct_unprotected", "Mean APR severity"="mean_severity") |>
      datatable(rownames=FALSE, options=list(dom="t", pageLength=10), class="stripe hover") |>
      formatStyle("Race / Ethnicity", fontWeight="bold")
  })

  output$age_table <- renderDT({
    sparcs_age |>
      mutate(median_charges = dollar(median_charges),
             mean_severity  = round(mean_severity,2),
             mortality_rate = percent(mortality_rate, accuracy=0.1),
             pct_medicaid   = percent(pct_medicaid, accuracy=0.1),
             median_los     = round(median_los,1)) |>
      arrange(age_group, race_eth) |>
      rename("Race"="race_eth","Age group"="age_group","Median charges"="median_charges",
             "Mean severity"="mean_severity","Mortality rate"="mortality_rate",
             "% Medicaid"="pct_medicaid","Median LOS"="median_los") |>
      datatable(rownames=FALSE, options=list(dom="t", pageLength=12), class="stripe hover") |>
      formatStyle("Race", fontWeight="bold") |>
      formatStyle("Median charges", background=styleColorBar(c(0,60000),"#D94F3D44"))
  })

} # end server

shinyApp(ui, server)
