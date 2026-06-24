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