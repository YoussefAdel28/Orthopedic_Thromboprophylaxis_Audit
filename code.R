## ---- 1. Install/load required packages ----


library(readxl)
library(dplyr)
library(tidyverse)
library(broom)
library(ggplot2)
library(pscl)   # for McFadden's pseudo R^2
library(gtsummary)
library(gt)
library(flextable)

cycle1_raw <- Cycle_1
cycle2_raw <- Cycle_2

## STEP 2a: Check your column names BEFORE doing anything else
## Run these two lines and look at the output carefully
## The exact column names matter -- copy them as they appear
cat("=== Cycle 1 Column Names ===\n")
print(colnames(cycle1_raw))

cat("\n=== Cycle 2 Column Names ===\n")
print(colnames(cycle2_raw))

## STEP 2b: Check how key variables are coded in your sheets
cat("\n=== DVT_diagnosis coding ===\n")
print(table(cycle1_raw$DVT_diagnosis, useNA = "ifany"))
print(table(cycle2_raw$DVT_diagnosis, useNA = "ifany"))

cat("\n=== Sex coding ===\n")
print(table(cycle1_raw$Sex, useNA = "ifany"))
print(table(cycle2_raw$Sex, useNA = "ifany"))

cat("\n=== High_BMI coding ===\n")
print(table(cycle1_raw$High_BMI, useNA = "ifany"))
print(table(cycle2_raw$High_BMI, useNA = "ifany"))

cat("\n=== Smoking coding ===\n")
print(table(cycle1_raw$Smoking, useNA = "ifany"))
print(table(cycle2_raw$Smoking, useNA = "ifany"))

## ---- STEP 3: Define recoding helper functions ----

## Converts Yes/No columns to 1/0
recode_yn <- function(x) {
  case_when(
    tolower(trimws(as.character(x))) %in% c("yes", "1", "true")  ~ 1L,
    tolower(trimws(as.character(x))) %in% c("no",  "0", "false") ~ 0L,
    TRUE ~ NA_integer_
  )
}

## Converts DVT diagnosis column to 1/0
recode_dvt <- function(x) {
  case_when(
    tolower(trimws(as.character(x))) %in% c("yes", "1", "true", "confirmed", "positive") ~ 1L,
    tolower(trimws(as.character(x))) %in% c("no", "0", "false", "not confirmed", "negative") ~ 0L,
    TRUE ~ NA_integer_
  )
}

## ---- STEP 4: Recode and prepare each cycle ----
## Column names below match exactly what was confirmed from your files:
## Name, ID, Age, Sex, DM, High_BMI, HTN, CHD, CKD, CPD, PVD,
## Smoking, Hx_of_DVT, Coagulation_disorder, Hx_malignancy,
## MOT, Hospital_stay, Fracture_site, DVT_diagnosis, Doppler Info

prepare_cycle <- function(df, cycle_label) {
  df %>%
    mutate(
      ## Cycle label
      Cycle = cycle_label,
      
      ## Continuous
      Age = as.numeric(Age),
      
      ## Sex — already named "Sex" in your sheet, just standardise labels
      Sex = case_when(
        tolower(trimws(as.character(Sex))) == "male"   ~ "Male",
        tolower(trimws(as.character(Sex))) == "female" ~ "Female",
        TRUE ~ NA_character_
      ),
      
      ## Comorbidities — column names match your sheet exactly
      DM           = recode_yn(DM),
      HTN          = recode_yn(HTN),
      CHD          = recode_yn(CHD),
      CKD          = recode_yn(CKD),
      CPD          = recode_yn(CPD),
      High_BMI     = recode_yn(High_BMI),
      Smoking      = recode_yn(Smoking),
      PVD          = recode_yn(PVD),
      Prior_DVT    = recode_yn(Hx_of_DVT),
      Coagulopathy = recode_yn(Coagulation_disorder),
      Malignancy   = recode_yn(Hx_malignancy),
      
      ## Outcome
      DVT = recode_dvt(DVT_diagnosis)
    ) %>%
    select(Cycle, Age, Sex, DM, HTN, CHD, CKD, CPD,
           High_BMI, Smoking, PVD, Prior_DVT,
           Coagulopathy, Malignancy, DVT)
}

c1 <- prepare_cycle(cycle1_raw, "Cycle 1")
c2 <- prepare_cycle(cycle2_raw, "Cycle 2")

## ---- STEP 5: Combine both cycles ----
combined <- bind_rows(c1, c2) %>%
  mutate(Cycle = factor(Cycle, levels = c("Cycle 1", "Cycle 2")))

## Sanity check
cat("\n=== Sample sizes ===\n")
print(table(combined$Cycle))

cat("\n=== DVT counts by cycle ===\n")
print(table(DVT = combined$DVT, Cycle = combined$Cycle, useNA = "ifany"))

## ---- STEP 6: Check for any 100% missing variables (failed recoding) ----
na_check <- combined %>%
  summarise(across(everything(), ~ sum(is.na(.)) / n() * 100)) %>%
  tidyr::pivot_longer(everything(), names_to = "Variable", values_to = "Pct_Missing") %>%
  filter(Pct_Missing == 100)

if (nrow(na_check) > 0) {
  cat("\n=== WARNING: These variables are 100% missing (column name mismatch) ===\n")
  print(na_check)
  cat("Go back to Step 4 and fix the column name for these variables.\n")
} else {
  cat("\n=== All variables recoded successfully ===\n")
}

## ---- STEP 7: Build the demographics table ----
## Statistical methods:
## - Age:                     Welch two-sample t-test (unequal variance assumed)
## - Sex, DM, HTN, CHD, CKD,
##   CPD, High_BMI, Smoking,
##   DVT:                     Pearson chi-squared test
## - PVD, Prior_DVT,
##   Coagulopathy, Malignancy: Fisher's exact test
##                             (used when expected cell count < 5)

demo_table <- combined %>%
  tbl_summary(
    by = Cycle,
    statistic = list(
      Age               ~ "{mean} ({sd})",
      all_dichotomous() ~ "{n} ({p}%)"
    ),
    digits = list(
      Age               ~ 1,
      all_dichotomous() ~ c(0, 1)
    ),
    label = list(
      Age          ~ "Age, years — mean (SD)",
      Sex          ~ "Sex",
      DM           ~ "Diabetes mellitus",
      HTN          ~ "Hypertension",
      CHD          ~ "Coronary heart disease",
      CKD          ~ "Chronic kidney disease",
      CPD          ~ "Chronic pulmonary disease",
      High_BMI     ~ "High BMI (overweight/obese)",
      Smoking      ~ "Smoking (current or former)",
      PVD          ~ "Peripheral vascular disease",
      Prior_DVT    ~ "Prior history of DVT",
      Coagulopathy ~ "Coagulation disorder",
      Malignancy   ~ "History of malignancy",
      DVT          ~ "Pre-operative DVT confirmed"
    ),
    missing = "no"
  ) %>%
  add_p(
    test = list(
      Age          ~ "t.test",
      ## All variables with expected cell count < 5 → Fisher's exact
      CKD          ~ "fisher.test",
      CPD          ~ "fisher.test",
      PVD          ~ "fisher.test",
      Prior_DVT    ~ "fisher.test",
      Coagulopathy ~ "fisher.test",
      Malignancy   ~ "fisher.test",
      ## All other binary variables → chi-square
      all_dichotomous() ~ "chisq.test"
    ),
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%
  add_overall() %>%
  bold_labels() %>%
  modify_header(label ~ "**Characteristic**") %>%
  modify_footnote(
    p.value ~ paste(
      "Welch two-sample t-test used for age;",
      "Pearson chi-squared test used for categorical variables;",
      "Fisher's exact test used for variables with expected cell count <5",
      "(chronic kidney disease, chronic pulmonary disease, peripheral vascular",
      "disease, prior DVT history, coagulation disorder, and malignancy)"
    )
  ) %>%
  modify_caption(
    "**Table 3. Patient Demographics and Comorbidity Profile: Cycle 1 vs Cycle 2**"
  )

## ---- STEP 8: Print and export ----
print(demo_table)

demo_table %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "Table3_Demographics.docx")

cat("\nTable saved as 'Table3_Demographics.docx'\n")
cat("Working directory:", getwd(), "\n")

## ============================================================
## END OF SCRIPT
## ============================================================

## ---- DVT by BMI category: counts and row percentages ----

## Cycle 1
cat("=== Cycle 1: DVT by BMI category ===\n")
cat("\nCounts:\n")
print(table(High_BMI = c1$High_BMI, DVT = c1$DVT))

cat("\nRow percentages (% with DVT within each BMI group):\n")
print(round(prop.table(table(High_BMI = c1$High_BMI, DVT = c1$DVT), margin = 1) * 100, 1))

## Cycle 2
cat("\n=== Cycle 2: DVT by BMI category ===\n")
cat("\nCounts:\n")
print(table(High_BMI = c2$High_BMI, DVT = c2$DVT))

cat("\nRow percentages (% with DVT within each BMI group):\n")
print(round(prop.table(table(High_BMI = c2$High_BMI, DVT = c2$DVT), margin = 1) * 100, 1))

## Combined across both cycles
cat("\n=== Combined (both cycles): DVT by BMI category ===\n")
cat("\nCounts:\n")
print(table(High_BMI = combined$High_BMI, DVT = combined$DVT))

cat("\nRow percentages (% with DVT within each BMI group):\n")
print(round(prop.table(table(High_BMI = combined$High_BMI, DVT = combined$DVT), margin = 1) * 100, 1))

## Chi-square test for association between BMI and DVT in each cycle
cat("\n=== Chi-square test: BMI vs DVT association ===\n")
cat("\nCycle 1 p-value:\n")
print(chisq.test(table(c1$High_BMI, c1$DVT)))

cat("\nCycle 2 p-value:\n")
print(chisq.test(table(c2$High_BMI, c2$DVT)))



## MULTI-VARIATE REGRESSION


## ============================================================
## MULTIVARIATE LOGISTIC REGRESSION
## Cycle 1, Cycle 2, and Combined — standard + Firth's
## ============================================================

if (!requireNamespace("logistf", quietly = TRUE)) install.packages("logistf")
library(logistf)
library(dplyr)
library(broom)

## ---- Event counts check first ----
## Rule of thumb: need at least 10 events per predictor variable
## With 7 predictors you need at least 70 DVT events for a stable standard model
cat("=== DVT event counts ===\n")
cat("Cycle 1 DVT events:", sum(c1$DVT, na.rm = TRUE), "| n =", nrow(c1), "\n")
cat("Cycle 2 DVT events:", sum(c2$DVT, na.rm = TRUE), "| n =", nrow(c2), "\n")
cat("Combined DVT events:", sum(combined$DVT, na.rm = TRUE), "| n =", nrow(combined), "\n")
cat("Events per variable (Cycle 1):", round(sum(c1$DVT, na.rm=TRUE)/7, 1), "\n")
cat("Events per variable (Cycle 2):", round(sum(c2$DVT, na.rm=TRUE)/7, 1), "\n")
cat("Events per variable (Combined):", round(sum(combined$DVT, na.rm=TRUE)/7, 1), "\n")

## ---- Prepare model datasets ----
## Uses variables already recoded in c1, c2, combined from the demographics script
## If running fresh, make sure those objects exist in your environment first

prep_model_data <- function(df) {
  df %>%
    mutate(
      Age      = as.numeric(Age),
      sex_male = if_else(Sex == "Male", 1L, 0L)
    ) %>%
    select(DVT, Age, sex_male, High_BMI, Smoking,
           HTN, DM, CHD) %>%
    filter(complete.cases(.))
}

d1  <- prep_model_data(c1)
d2  <- prep_model_data(c2)
d_all <- prep_model_data(combined) %>%
  mutate(
    Cycle2 = if_else(combined$Cycle[complete.cases(
      combined %>% select(DVT, Age, Sex, High_BMI, Smoking, HTN, DM, CHD)
    )] == "Cycle 2", 1L, 0L)
  )

## Model formula
formula_main <- DVT ~ Age + sex_male + High_BMI + Smoking + HTN + DM + CHD

## Formula for combined model includes cycle as a covariate
## to adjust for which cycle the patient came from
formula_combined <- DVT ~ Age + sex_male + High_BMI + Smoking + HTN + DM + CHD + Cycle2

## ============================================================
## HELPER FUNCTION: runs both glm and Firth, prints comparison
## ============================================================

run_regression <- function(data, formula, label) {
  
  cat("\n", strrep("=", 60), "\n")
  cat(label, "\n")
  cat("n =", nrow(data), "| DVT events =", sum(data$DVT), "\n")
  cat(strrep("=", 60), "\n")
  
  ## --- Standard logistic regression ---
  cat("\n--- Standard logistic regression (glm) ---\n")
  glm_model <- glm(formula, data = data, family = binomial(link = "logit"))
  
  glm_results <- tidy(glm_model, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term != "(Intercept)") %>%
    mutate(
      term = recode(term,
                    Age      = "Age (per year)",
                    sex_male = "Male sex",
                    High_BMI = "High BMI",
                    Smoking  = "Smoking",
                    HTN      = "Hypertension",
                    DM       = "Diabetes mellitus",
                    CHD      = "Coronary heart disease",
                    Cycle2   = "Cycle 2 (vs Cycle 1)"
      )
    ) %>%
    select(term, OR = estimate, CI_lower = conf.low,
           CI_upper = conf.high, p.value)
  
  print(glm_results, digits = 3)
  
  ## --- Firth's penalized logistic regression ---
  cat("\n--- Firth's penalized logistic regression ---\n")
  firth_model <- logistf(formula, data = data)
  
  firth_results <- data.frame(
    term     = names(coef(firth_model))[-1],
    OR       = exp(coef(firth_model))[-1],
    CI_lower = exp(firth_model$ci.lower)[-1],
    CI_upper = exp(firth_model$ci.upper)[-1],
    p.value  = firth_model$prob[-1]
  )
  
  firth_results$term <- recode(firth_results$term,
                               Age      = "Age (per year)",
                               sex_male = "Male sex",
                               High_BMI = "High BMI",
                               Smoking  = "Smoking",
                               HTN      = "Hypertension",
                               DM       = "Diabetes mellitus",
                               CHD      = "Coronary heart disease",
                               Cycle2   = "Cycle 2 (vs Cycle 1)"
  )
  
  print(firth_results, digits = 3, row.names = FALSE)
  
  ## --- Side by side comparison ---
  cat("\n--- Side-by-side: glm OR vs Firth OR ---\n")
  comparison <- merge(
    glm_results %>%
      rename(OR_glm = OR, p_glm = p.value) %>%
      select(term, OR_glm, p_glm),
    firth_results %>%
      rename(OR_firth = OR, CI_low_firth = CI_lower,
             CI_high_firth = CI_upper, p_firth = p.value),
    by = "term"
  )
  print(comparison, digits = 3, row.names = FALSE)
  
  ## Return Firth model for further use
  return(invisible(list(glm = glm_model, firth = firth_model)))
}

## ============================================================
## RUN REGRESSIONS
## ============================================================

models_c1  <- run_regression(d1,    formula_main,     "CYCLE 1")
models_c2  <- run_regression(d2,    formula_main,     "CYCLE 2")
models_all <- run_regression(d_all, formula_combined, "COMBINED (both cycles)")

## ============================================================
## LIKELIHOOD RATIO TEST + PSEUDO R2 (Firth models)
## ============================================================

cat("\n=== Model fit statistics (Firth's models) ===\n")

for (label in c("Cycle 1", "Cycle 2", "Combined")) {
  m <- switch(label,
              "Cycle 1"  = models_c1$firth,
              "Cycle 2"  = models_c2$firth,
              "Combined" = models_all$firth
  )
  lr_stat <- -2 * (m$loglik["null"] - m$loglik["full"])
  df_val  <- length(coef(m)) - 1
  p_val   <- pchisq(lr_stat, df = df_val, lower.tail = FALSE)
  cat("\n", label, "\n")
  cat("LR chi-square:", round(lr_stat, 3),
      "| df:", df_val,
      "| p:", format.pval(p_val, digits = 3), "\n")
}



## ============================================================
## FOREST PLOTS: Firth's Regression Results
## Cycle 1, Cycle 2, and Combined
## ============================================================

library(ggplot2)
library(dplyr)

## ---- STEP 1: Extract Firth results into tidy dataframes ----
## This uses the model objects (models_c1, models_c2, models_all)
## already in your environment from the regression script

extract_firth <- function(firth_model, label) {
  data.frame(
    Dataset  = label,
    term     = names(coef(firth_model))[-1],
    OR       = exp(coef(firth_model))[-1],
    CI_lower = exp(firth_model$ci.lower)[-1],
    CI_upper = exp(firth_model$ci.upper)[-1],
    p.value  = firth_model$prob[-1]
  ) %>%
    mutate(
      ## Clean up variable names for display
      term = recode(term,
                    Age      = "Age (per year)",
                    sex_male = "Male sex",
                    High_BMI = "High BMI",
                    Smoking  = "Smoking",
                    HTN      = "Hypertension",
                    DM       = "Diabetes mellitus",
                    CHD      = "Coronary heart disease",
                    Cycle2   = "Cycle 2 (vs Cycle 1)"
      ),
      ## Flag significant results for colour coding
      Significant = if_else(p.value < 0.05, "p < 0.05", "p ≥ 0.05"),
      ## Create label showing OR (95% CI) and p-value for display on plot
      OR_label = paste0(
        round(OR, 2),
        " (", round(CI_lower, 2), "–", round(CI_upper, 2), ")",
        "\np=", round(p.value, 3)
      )
    )
}

c1_results  <- extract_firth(models_c1$firth,  "Cycle 1")
c2_results  <- extract_firth(models_c2$firth,  "Cycle 2")
all_results <- extract_firth(models_all$firth, "Combined")

## ============================================================
## STEP 2: Define a reusable forest plot function
## ============================================================

make_forest_plot <- function(data, title, x_limit = NULL) {
  
  ## Order variables by OR for visual clarity
  data <- data %>%
    arrange(OR) %>%
    mutate(term = factor(term, levels = term))
  
  ## Set x-axis limit: default to max CI upper + buffer, min 0.01
  if (is.null(x_limit)) {
    x_limit <- min(max(data$CI_upper) * 1.2, 150)
  }
  
  ggplot(data, aes(x = OR, y = term, colour = Significant)) +
    
    ## Reference line at OR = 1 (no effect)
    geom_vline(
      xintercept = 1,
      linetype   = "dashed",
      colour     = "grey40",
      linewidth  = 0.7
    ) +
    
    ## Confidence interval lines
    geom_errorbarh(
      aes(xmin = CI_lower, xmax = CI_upper),
      height    = 0.25,
      linewidth = 0.8
    ) +
    
    ## OR point estimate
    geom_point(size = 3.5, shape = 18) +
    
    ## OR and CI label to the right of each point
    geom_text(
      aes(x = CI_upper, label = OR_label),
      hjust  = -0.1,
      size   = 2.8,
      colour = "grey20",
      lineheight = 0.85
    ) +
    
    ## Colour scheme: red for significant, grey for non-significant
    scale_colour_manual(
      values = c("p < 0.05" = "#C0392B", "p ≥ 0.05" = "grey50")
    ) +
    
    ## Log scale is standard for OR forest plots
    scale_x_log10(
      limits = c(0.01, x_limit),
      breaks = c(0.1, 0.25, 0.5, 1, 2, 5, 10, 25, 50, 100),
      labels = c("0.1", "0.25", "0.5", "1", "2", "5", "10", "25", "50", "100")
    ) +
    
    labs(
      title    = title,
      x        = "Adjusted Odds Ratio (95% CI, log scale)",
      y        = NULL,
      colour   = NULL
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      plot.title       = element_text(face = "bold", size = 12, hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "grey90"),
      axis.text.y      = element_text(size = 10, colour = "grey20"),
      axis.text.x      = element_text(size = 9),
      legend.position  = "bottom",
      plot.margin      = margin(10, 120, 10, 10)  # extra right margin for labels
    )
}

## ============================================================
## STEP 3: Generate and save individual plots
## ============================================================

## --- Cycle 1 forest plot ---
p1 <- make_forest_plot(
  c1_results,
  "Figure A: Adjusted Odds Ratios for DVT Predictors — Cycle 1 (Firth's Regression)"
)
print(p1)
ggsave("forest_plot_cycle1.png", p1, width = 10, height = 5.5,
       dpi = 300, bg = "white")
cat("Saved: forest_plot_cycle1.png\n")

## --- Cycle 2 forest plot ---
p2 <- make_forest_plot(
  c2_results,
  "Figure B: Adjusted Odds Ratios for DVT Predictors — Cycle 2 (Firth's Regression)"
)
print(p2)
ggsave("forest_plot_cycle2.png", p2, width = 10, height = 5.5,
       dpi = 300, bg = "white")
cat("Saved: forest_plot_cycle2.png\n")

## --- Combined forest plot ---
p3 <- make_forest_plot(
  all_results,
  "Figure C: Adjusted Odds Ratios for DVT Predictors — Combined Cycles (Firth's Regression)"
)
print(p3)
ggsave("forest_plot_combined.png", p3, width = 10, height = 6,
       dpi = 300, bg = "white")
cat("Saved: forest_plot_combined.png\n")

## ============================================================
## STEP 4: Combined three-panel plot (all cycles side by side)
## ============================================================

## Useful for a single manuscript figure showing all three models together
if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")
library(patchwork)

## Fix variable order to be consistent across all three panels
## (same variable order makes comparison across panels easier)
var_order <- c(
  "Age (per year)",
  "Male sex",
  "Diabetes mellitus",
  "Coronary heart disease",
  "Hypertension",
  "High BMI",
  "Smoking",
  "Cycle 2 (vs Cycle 1)"   # only appears in combined
)

reorder_vars <- function(data) {
  data %>%
    mutate(term = factor(term, levels = rev(var_order))) %>%
    arrange(term)
}

## Rebuild plots with consistent variable ordering
make_panel_plot <- function(data, title) {
  data <- reorder_vars(data)
  ggplot(data, aes(x = OR, y = term, colour = Significant)) +
    geom_vline(xintercept = 1, linetype = "dashed",
               colour = "grey40", linewidth = 0.6) +
    geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper),
                   height = 0.25, linewidth = 0.7) +
    geom_point(size = 3, shape = 18) +
    scale_colour_manual(
      values = c("p < 0.05" = "#C0392B", "p ≥ 0.05" = "grey50")
    ) +
    scale_x_log10(
      limits = c(0.01, 200),
      breaks = c(0.1, 0.5, 1, 5, 25, 100),
      labels = c("0.1", "0.5", "1", "5", "25", "100")
    ) +
    labs(title = title, x = "OR (95% CI, log scale)", y = NULL, colour = NULL) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title       = element_text(face = "bold", size = 10),
      panel.grid.minor = element_blank(),
      legend.position  = "none",
      axis.text.y      = element_text(size = 9)
    )
}

panel1 <- make_panel_plot(c1_results,  "Cycle 1")
panel2 <- make_panel_plot(c2_results,  "Cycle 2")
panel3 <- make_panel_plot(all_results, "Combined")

combined_plot <- (panel1 | panel2 | panel3) +
  plot_annotation(
    title    = "Adjusted Odds Ratios for Predictors of Pre-operative DVT (Firth's Penalized Logistic Regression)",
    caption  = "Red = p<0.05; Grey = p≥0.05. OR displayed on log scale. Cycle 2 variable only appears in combined model.",
    theme    = theme(
      plot.title   = element_text(face = "bold", size = 11, hjust = 0.5),
      plot.caption = element_text(size = 8, colour = "grey40")
    )
  ) &
  theme(legend.position = "none")

print(combined_plot)
ggsave("forest_plot_all_three.png", combined_plot,
       width = 15, height = 6, dpi = 300, bg = "white")
cat("Saved: forest_plot_all_three.png\n")
cat("Working directory:", getwd(), "\n")

## ============================================================
## END OF SCRIPT
## ============================================================


# ============================================================
## PROCESS MEASURES TABLE: Cycle 1 vs Cycle 2
## Formatted to match the demographics table style
## ============================================================

library(dplyr)
library(gtsummary)
library(gt)
library(flextable)

## ---- STEP 1: Build the process measures dataset ----
## Each row represents one patient
## We reconstruct binary columns from the known counts

## UPDATE these numerators if your exact counts differ:
## Format: met = number of patients where criterion was met
## n1 = 100 (Cycle 1 total), n2 = 109 (Cycle 2 total)

n1 <- 100
n2 <- 109

## Cycle 1 data
cycle1_process <- data.frame(
  Cycle             = "Cycle 1",
  LMWH_timing       = c(rep(1, 73),  rep(0, 27)),   # 73% met
  Correct_dose      = c(rep(1, 84),  rep(0, 16)),   # 84% met
  Correct_route     = c(rep(1, 100), rep(0, 0)),    # 100% met
  Risk_assessment   = c(rep(1, 100), rep(0, 0)),    # 100% met
  Mechanical_proph  = c(rep(1, 0),   rep(0, 100)),  # 0% met
  Patient_education_provided  = c(rep(1, 0),   rep(0, 100)),  # 0% met
  DVT_screening     = c(rep(1, 100), rep(0, 0)),    # 100% met
  DVT_incidence     = c(rep(1, 29),  rep(0, 71))    # 29% incidence
)

## Cycle 2 data
## UPDATE the mechanical prophylaxis numerator (x2) below
## to match your exact count (34% of 109 = ~37 patients)
cycle2_process <- data.frame(
  Cycle             = "Cycle 2",
  LMWH_timing       = c(rep(1, 109), rep(0, 0)),    # 100% met
  Correct_dose      = c(rep(1, 109), rep(0, 0)),    # 100% met
  Correct_route     = c(rep(1, 109), rep(0, 0)),    # 100% met
  Risk_assessment   = c(rep(1, 109), rep(0, 0)),    # 100% met
  Mechanical_proph  = c(rep(1, 37),  rep(0, 72)),   # 34% met (37/109)
  Patient_education_provided  = c(rep(1, 0),   rep(0, 109)),  # 0% met
  DVT_screening     = c(rep(1, 109), rep(0, 0)),    # 100% met
  DVT_incidence     = c(rep(1, 14),  rep(0, 95))    # 12.8% incidence
)

## Combine
process_data <- bind_rows(cycle1_process, cycle2_process) %>%
  mutate(Cycle = factor(Cycle, levels = c("Cycle 1", "Cycle 2")))

cat("=== Confirm row counts ===\n")
print(table(process_data$Cycle))

## ---- STEP 2: Define which variables need Fisher's exact ----
## Fisher's exact is needed when any proportion is 0% or 100%
## in either cycle, making expected cell counts < 5

## Variables at ceiling/floor in one or both cycles:
## Correct_route, Risk_assessment, DVT_screening → 100% in both → no test possible
## LMWH_timing   → 73% C1, 100% C2 → Fisher's (C2 at ceiling)
## Correct_dose  → 84% C1, 100% C2 → Fisher's (C2 at ceiling)
## Mechanical_proph → 0% C1, 34% C2 → Fisher's (C1 at floor)
## DVT_incidence → 29% C1, 12.8% C2 → chi-square or z-test (neither at boundary)

## ---- STEP 3: Build the table ----
process_table <- process_data %>%
  tbl_summary(
    by = Cycle,
    statistic = list(
      all_dichotomous() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_dichotomous() ~ c(0, 1)
    ),
    label = list(
      LMWH_timing      ~ "LMWH prophylaxis given within 12-24 hours",
      Correct_dose     ~ "Correct prophylactic dose given",
      Correct_route    ~ "Correct route of administration",
      Risk_assessment  ~ "Risk assessment correctly completed",
      Mechanical_proph ~ "Mechanical prophylaxis initiated",
      Patient_education_provided ~ "Patient education provided",
      DVT_screening    ~ "DVT screening by Doppler ultrasound",
      DVT_incidence    ~ "Pre-operative DVT confirmed"
    ),
    missing = "no",
    ## Show the "1" level only (i.e. "met" / "yes" / DVT present)
    ## so each row shows n (%) who met the criterion
    value = list(
      LMWH_timing      ~ 1,
      Correct_dose     ~ 1,
      Correct_route    ~ 1,
      Risk_assessment  ~ 1,
      Mechanical_proph ~ 1,
      Patient_education_provided ~ 1,
      DVT_screening    ~ 1,
      DVT_incidence    ~ 1
    )
  ) %>%
  add_p(
    test = list(
      ## Fisher's exact for variables at 0% or 100% in either cycle
      LMWH_timing      ~ "fisher.test",
      Correct_dose     ~ "fisher.test",
      Mechanical_proph ~ "fisher.test",
      ## Chi-square for DVT incidence (neither at boundary)
      DVT_incidence    ~ "chisq.test",
      ## No test for 100%/100% variables — handled via footnote
      Correct_route    ~ "fisher.test",
      Risk_assessment  ~ "fisher.test",
      DVT_screening    ~ "fisher.test"
    ),
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>%
  bold_labels() %>%
  modify_header(label ~ "**Audit Criterion**") %>%
  modify_footnote(
    p.value ~ paste(
      "Fisher's exact test used for variables where proportion",
      "reached 0% or 100% in either cycle;",
      "Pearson chi-squared test used for DVT incidence.",
      "No statistical test applied to criteria at 100% in both cycles",
      "(correct route of administration, risk assessment completion,",
      "DVT screening) as there is no variation to test."
    )
  ) %>%
  modify_caption(
    "**Table 2. Audit Criteria Outcomes: Cycle 1 vs Cycle 2**"
  )

## ---- STEP 4: Print and export ----
print(process_table)

process_table %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "Table2_Process_Measures.docx")

cat("\nTable saved as 'Table2_Process_Measures.docx'\n")
cat("Working directory:", getwd(), "\n")

## ============================================================
## END OF SCRIPT
## ============================================================


x1 <- sum(c1$DVT)  # number of DVT-positive cases in Cycle 1
x2 <- sum(c2$DVT)  # number of DVT-positive cases in Cycle 2

## ---- 5. Incidence + 95% CI (Wald / normal approximation) ----
## This is the simpler, more commonly reported CI in clinical audits
wald_ci <- function(x, n, conf.level = 0.95) {
  p <- x / n
  z <- qnorm(1 - (1 - conf.level) / 2)
  se <- sqrt(p * (1 - p) / n)
  lower <- max(0, p - z * se)
  upper <- min(1, p + z * se)
  return(c(estimate = p, lower = lower, upper = upper))
}

cat("\n--- Cycle 1 DVT Incidence (Wald 95% CI) ---\n")
print(round(wald_ci(x1, n1) * 100, 1))

cat("\n--- Cycle 2 DVT Incidence (Wald 95% CI) ---\n")
print(round(wald_ci(x2, n2) * 100, 1))

## ---- 6. Incidence + 95% CI (Wilson score interval) ----
## More robust for smaller samples / proportions near 0 or 1
## Uses base R's prop.test, which defaults to Wilson-type CI with continuity correction
wilson_ci_1 <- prop.test(x1, n1, correct = TRUE)
wilson_ci_2 <- prop.test(x2, n2, correct = TRUE)

cat("\n--- Cycle 1 DVT Incidence (Wilson 95% CI, continuity corrected) ---\n")
cat("Estimate:", round(wilson_ci_1$estimate * 100, 1), "%\n")
cat("95% CI:", round(wilson_ci_1$conf.int[1] * 100, 1), "% to",
    round(wilson_ci_1$conf.int[2] * 100, 1), "%\n")

cat("\n--- Cycle 2 DVT Incidence (Wilson 95% CI, continuity corrected) ---\n")
cat("Estimate:", round(wilson_ci_2$estimate * 100, 1), "%\n")
cat("95% CI:", round(wilson_ci_2$conf.int[1] * 100, 1), "% to",
    round(wilson_ci_2$conf.int[2] * 100, 1), "%\n")

## ---- 7. Two-proportion z-test: Cycle 1 vs Cycle 2 ----
## prop.test() performs a chi-square test which is mathematically equivalent
## to a two-proportion z-test for a 2x2 comparison (z^2 = chi-square statistic)
two_prop_test <- prop.test(
  x = c(x1, x2),
  n = c(n1, n2),
  correct = TRUE   # applies Yates' continuity correction (standard for 2x2 comparisons)
)

cat("\n--- Two-Proportion Z-Test: Cycle 1 vs Cycle 2 ---\n")
print(two_prop_test)

## Extract a clean summary
cat("\n--- Summary ---\n")
cat("Cycle 1 incidence:", round(x1/n1 * 100, 1), "%\n")
cat("Cycle 2 incidence:", round(x2/n2 * 100, 1), "%\n")
cat("Difference in proportions:", round((x1/n1 - x2/n2) * 100, 1), "percentage points\n")
cat("Chi-square statistic:", round(two_prop_test$statistic, 3), "\n")
cat("p-value:", format.pval(two_prop_test$p.value, digits = 3), "\n")
cat("95% CI for difference in proportions:",
    round(two_prop_test$conf.int[1] * 100, 1), "% to",
    round(two_prop_test$conf.int[2] * 100, 1), "%\n")

## WALD IS USED IN THE MANUSCRIPT


## ---- STEP 1: Calculate incidence and Wald 95% CIs ----

wald_ci <- function(x, n, conf.level = 0.95) {
  p     <- x / n
  z     <- qnorm(1 - (1 - conf.level) / 2)
  se    <- sqrt(p * (1 - p) / n)
  lower <- max(0, p - z * se)
  upper <- min(1, p + z * se)
  data.frame(estimate = p * 100,
             lower    = lower * 100,
             upper    = upper * 100)
}

dvt_data <- bind_rows(
  data.frame(Cycle = "Cycle 1", wald_ci(x = 29, n = 100)),
  data.frame(Cycle = "Cycle 2", wald_ci(x = 14, n = 109))
) %>%
  mutate(
    Cycle     = factor(Cycle, levels = c("Cycle 1", "Cycle 2")),
    bar_label = c("29.0%\n(29/100)", "12.8%\n(14/109)")
  )

cat("=== DVT Incidence with Wald 95% CI ===\n")
print(dvt_data %>%
        select(Cycle, estimate, lower, upper) %>%
        mutate(across(where(is.numeric), ~ round(., 1))),
      row.names = FALSE)

## ---- STEP 2: Build the bar chart ----

p_dvt_bar <- ggplot(dvt_data,
                    aes(x = Cycle, y = estimate, fill = Cycle)) +
  
  ## Bars
  geom_bar(stat   = "identity",
           width  = 0.5,
           colour = "white") +
  
  ## Error bars (Wald 95% CI)
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width     = 0.12,
    linewidth = 0.9,
    colour    = "grey25"
  ) +
  
  ## Percentage + count label inside bar
  geom_text(
    aes(label = bar_label),
    y          = 5,
    size       = 4,
    fontface   = "bold",
    colour     = "white",
    lineheight = 1.1
  ) +
  
  ## CI range label above each error bar
  geom_text(
    aes(y     = upper,
        label = paste0("95% CI:\n",
                       round(lower, 1), "–",
                       round(upper, 1), "%")),
    vjust      = -0.4,
    size       = 3.2,
    colour     = "grey30",
    lineheight = 1.0
  ) +
  
  scale_fill_manual(
    values = c("Cycle 1" = "#2E86AB",
               "Cycle 2" = "#A23B72")
  ) +
  
  scale_y_continuous(
    limits = c(0, 48),
    breaks = seq(0, 45, 5),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  
  labs(
    x    = NULL,
    y    = "DVT Incidence (%)"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position    = "none",
    plot.title         = element_blank(),
    plot.subtitle      = element_blank(),
    plot.caption       = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(size = 12, face = "bold"),
    axis.text.y        = element_text(size = 10),
    plot.margin        = margin(15, 20, 15, 15)
  )

print(p_dvt_bar)

ggsave("dvt_incidence_bar_chart.png", p_dvt_bar,
       width = 7, height = 7, dpi = 300, bg = "white")

cat("\nSaved: dvt_incidence_bar_chart.png\n")
cat("Working directory:", getwd(), "\n")

## ============================================================
## END OF SCRIPT
## ============================================================





## ---- STEP 1: Build fracture site distribution data ----
## Proportion of patients with each fracture site, by cycle

frac_dist <- bind_rows(
  cycle1_raw %>%
    mutate(Cycle = "Cycle 1") %>%
    count(Cycle, Fracture_site) %>%
    mutate(Pct = n / sum(n) * 100),
  cycle2_raw %>%
    mutate(Cycle = "Cycle 2") %>%
    count(Cycle, Fracture_site) %>%
    mutate(Pct = n / sum(n) * 100)
) %>%
  mutate(
    Cycle = factor(Cycle, levels = c("Cycle 1", "Cycle 2")),
    ## Order fracture sites by overall frequency for cleaner display
    Fracture_site = factor(Fracture_site,
                           levels = c("Hip", "Thigh", "Knee",
                                      "Leg", "Ankle", "Foot", "Heel"))
  )

## Quick check
cat("=== Fracture site distribution (%) ===\n")
print(frac_dist %>% select(Cycle, Fracture_site, n, Pct) %>%
        mutate(Pct = round(Pct, 1)), row.names = FALSE)

## ---- STEP 2: Build DVT rate by fracture site data ----

frac_dvt <- bind_rows(
  data.frame(
    Cycle         = "Cycle 1",
    Fracture_site = cycle1_raw$Fracture_site,
    DVT           = c1$DVT
  ),
  data.frame(
    Cycle         = "Cycle 2",
    Fracture_site = cycle2_raw$Fracture_site,
    DVT           = c2$DVT
  )
) %>%
  group_by(Cycle, Fracture_site) %>%
  summarise(
    n_total = n(),
    n_dvt   = sum(DVT, na.rm = TRUE),
    DVT_pct = n_dvt / n_total * 100,
    .groups = "drop"
  ) %>%
  mutate(
    Cycle = factor(Cycle, levels = c("Cycle 1", "Cycle 2")),
    Fracture_site = factor(Fracture_site,
                           levels = c("Hip", "Thigh", "Knee",
                                      "Leg", "Ankle", "Foot", "Heel")),
    ## Label showing DVT count / total for each bar
    bar_label = paste0(n_dvt, "/", n_total)
  )

## ============================================================
## CHART 1: Fracture site distribution (% of patients per cycle)
## ============================================================

p_dist <- ggplot(frac_dist,
                 aes(x = Fracture_site, y = Pct, fill = Cycle)) +
  
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.7),
           width = 0.65) +
  
  ## Add percentage labels on top of each bar
  geom_text(aes(label = paste0(round(Pct, 1), "%")),
            position = position_dodge(width = 0.7),
            vjust    = -0.4,
            size     = 3.2,
            colour   = "grey20",
            family   = "Tahoma") +
  
  scale_fill_manual(values = c("Cycle 1" = "#2E86AB",
                               "Cycle 2" = "#A23B72")) +
  
  scale_y_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, 5),
    labels = function(x) paste0(x, "%")
  ) +
  
  labs(
    x        = "Fracture Site",
    y        = "Percentage of Lower Limb Fractures (%)",
    fill     = NULL
  ) +
  
  theme_minimal(base_size = 30, base_family = "Tahoma") +
  theme(
    plot.title      = element_text(face = "bold", size = 12),
    plot.subtitle   = element_text(size = 10, colour = "grey40"),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x     = element_text(size = 30)
  )

print(p_dist)
ggsave("fracture_site_distribution.png", p_dist,
       width = 10, height = 6, dpi = 300, bg = "white")
cat("Saved: fracture_site_distribution.png\n")


cycle2_fresh <- Cycle_2