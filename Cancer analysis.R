# ###############################################################################################
# DSCI 724 — Cancer Risk Factors
# File: cancer-risk-factors.csv
#
# 
# #######################################
# installing the required Packages
# ###################################################
req_pkgs <- c("tidyverse", "corrplot", "broom")
to_install <- setdiff(req_pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(req_pkgs, library, character.only = TRUE))

# ###########################################
# 1) Import the cancer data set
# ##################################################
setwd("C:/Users/user/Documents/Data Mining for Managers DSCI 724/Project/Sharon")
getwd()

df_raw <- read.csv("cancer-risk-factors.csv", stringsAsFactors = FALSE, check.names = FALSE)
df_raw

names(df_raw)
table(df_raw$Cancer_Type, useNA = "ifany")
cat("Rows x Cols:", nrow(df_raw), "x", ncol(df_raw), "\n")


###############################################
# 2) Cleaning the data set to make it easier when doing the analysis and removing missing values
#    - Drop IDs
#    - Standardize key types
#    - Convert Yes/No  1/0 for binary risk factors
# #################################
#Using the %>% pipe to pass df_raw through a sequence of data-cleaning steps:
  # 1) Remove Patient_ID since it is an identifier, not a feature
  # 2) Convert selected columns to factors for categorical analysis

df <- df_raw %>%
  select(-any_of(c("Patient_ID"))) %>%
  mutate(
    Cancer_Type = as.factor(Cancer_Type),
    Gender      = as.factor(Gender),
    Risk_Level  = as.factor(Risk_Level),
    Physical_Activity_Level = as.factor(Physical_Activity_Level)
  )

#Defining the list of binary-like columns that should be converted to 0/1 values.

binary_cols <- c("Smoking","Alcohol_Use","Obesity","Family_History",
                 "Diet_Red_Meat","Diet_Salted_Processed","Fruit_Veg_Intake",
                 "Physical_Activity","Air_Pollution","Occupational_Hazards",
                 "BRCA_Mutation","H_Pylori_Infection")

# Loop through only the binary columns that are in the dataframe.
# Standardize each column by mapping various "yes" responses to 1 
# and various "no" responses to 0. Any unrecognized value becomes NA.
for (col in intersect(binary_cols, names(df))) {
  df[[col]] <- ifelse(tolower(df[[col]]) %in% c("yes","y","true","1"), 1,
                      ifelse(tolower(df[[col]]) %in% c("no","n","false","0"), 0, NA))
  df[[col]] <- as.numeric(df[[col]])
}


# Numeric coercions (safe)
# Convert numeric-like columns to numeric type safely.
# suppressWarnings() is used so that any non-numeric coercion warnings 
# (e.g., text values becoming NA) do not clutter the output.

num_like <- c("Age", "BMI", "Overall_Risk_Score")
for (col in intersect(num_like, names(df))) {
  df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
}



# ##################################################################
# 3) Select variables for EDA (<= 10 for correlation rubric)
#    Response + 9 predictors
# #######################################################

#Convert numeric-like columns to numeric values.
# This loop checks which of the expected numeric variables ("Age", "BMI",
# "Overall_Risk_Score") are actually present in the dataframe.
# 
# suppressWarnings() is used so the console doesn’t show warnings when
# a value cannot be converted (e.g., text becomes NA). This keeps output
# clean during preprocessing while still performing the conversion.

vars_eda <- c(
  "Overall_Risk_Score",           # response
  "Age", "BMI",
  "Smoking", "Alcohol_Use", "Obesity",
  "Family_History", "Air_Pollution", "Occupational_Hazards",
  "H_Pylori_Infection"
)

eda_df <- df %>% select(any_of(vars_eda))

# 4) Descriptive Statistics
#    Start with means/SD (continuous), then counts for categoricals/binaries

# ###################################################################
# Install flextable if not already installed
# Using flextable to create clean, professional-looking tables
if (!require(flextable)) install.packages("flextable")
library(flextable)

# Define continuous variables
cont_vars <- intersect(c("Overall_Risk_Score","Age","BMI"), names(eda_df))

cat("\n=== Means & Standard Deviations (Continuous) ===\n")

# Determine the continuous variables available for summary statistics.
# We start with a predefined list of expected numeric variables and use intersect()
# to keep only those that exist in eda_df.

if (length(cont_vars) > 0) {
  cont_summary <- eda_df %>%
    summarize(across(all_of(cont_vars),
                     list(mean = ~mean(.x, na.rm = TRUE),
                          sd   = ~sd(.x,   na.rm = TRUE)),
                     .names = "{.col}__{.fn}")) %>%
    pivot_longer(everything(),
                 names_to = c("Variable", ".value"),
                 names_sep = "__") %>%
    arrange(Variable)
  
  # Convert to flextable
  cont_table <- flextable(cont_summary)
  cont_table <- autofit(cont_table)
  
  print(cont_table)
} else {
  cat("No continuous variables found.\n")
}

# ###################################################################################
# Boxplots — Tukey five-number summaries
# Response + key numeric/binary predictors
# #########################################################################

# Choose variables to show 
box_vars <- c(
  "Overall_Risk_Score",  # response
  "Age", "BMI",
  "Smoking", "Alcohol_Use", "Obesity",
  "Family_History", "Air_Pollution", "Occupational_Hazards",
  "H_Pylori_Infection"
)

# Keep only those present and numeric
box_df <- eda_df %>%
  dplyr::select(dplyr::any_of(box_vars)) %>%
  dplyr::select(where(is.numeric))

# Simple title shortener for nicer plot headers
short_title <- function(x) {
  x <- gsub("Overall_Risk_Score", "Risk_Score", x, fixed = TRUE)
  x <- gsub("Alcohol_Use", "Alcohol", x, fixed = TRUE)
  x <- gsub("Family_History", "Fam_Hx", x, fixed = TRUE)
  x <- gsub("Air_Pollution", "AirPoll", x, fixed = TRUE)
  x <- gsub("Occupational_Hazards", "OccHaz", x, fixed = TRUE)
  x <- gsub("H_Pylori_Infection", "H.Pylori", x, fixed = TRUE)
  x
}

# Layout (2 rows, auto columns)
n <- ncol(box_df)
rows <- 2
cols <- max(1, ceiling(n / rows))

op <- par(mfrow = c(rows, cols), mar = c(3.5, 3.8, 2, 1), mgp = c(2.2, 0.7, 0), las = 1)

for (nm in names(box_df)) {
  boxplot(
    box_df[[nm]],
    main = short_title(nm),
    ylab = "",
    xlab = "",
    col  = "gray90",
    outline = TRUE,
    cex.main = 0.9,
    cex.axis = 0.85
  )
  grid(nx = NA, ny = NULL)
}

# Pad any empty slots to keep the grid shape nice
pad <- rows * cols - n
if (pad > 0) for (i in seq_len(pad)) { plot.new(); title("") }

par(op)
cat("\nBoxplots drawn for:", paste(names(box_df), collapse = ", "), "\n")


######################################
# Pretty Pearson correlation table with flextable
# ###########################################################
# Compute Pearson correlation matrix numeric variables only
# ##############################################################
corr_mat <- cor(eda_df %>% select(where(is.numeric)), 
                use = "pairwise.complete.obs", 
                method = "pearson")


# 1) Round and optionally keep only the upper triangle for compactness
corr_rounded <- round(corr_mat, 3)
##error

# Set lower triangle (excluding diagonal) to NA to reduce clutter (optional)
keep_upper_only <- TRUE
if (keep_upper_only) {
  corr_rounded[lower.tri(corr_rounded)] <- NA
}

# 2) Convert to data frame with rownames preserved
corr_df_print <- data.frame(Variable = rownames(corr_rounded), corr_rounded, check.names = FALSE)

# 3) Flextable (small font + autofit so it fits on screen)
if (!requireNamespace("flextable", quietly = TRUE)) install.packages("flextable")
library(flextable)

ft_corr <- flextable(corr_df_print)
ft_corr <- autofit(ft_corr)
ft_corr <- fontsize(ft_corr, size = 9, part = "all")   # shrink to fit
ft_corr <- align(ft_corr, align = "center", part = "all")
ft_corr <- bold(ft_corr, part = "header")

# freeze the first column (row labels) visually
ft_corr <- bg(ft_corr, j = "Variable", bg = "#F5F5F5")
ft_corr <- align(ft_corr, j = "Variable", align = "left")

ft_corr   # <- prints a single, scrollable table that fits nicely


# ##################################################################################
# Correlation matrix for selected cancer risk variables numeric only
# ######################################################################################

num_df <- eda_df %>% dplyr::select(
  Overall_Risk_Score, Age, BMI, Smoking, Alcohol_Use,
  Obesity, Family_History, Air_Pollution,
  Occupational_Hazards, H_Pylori_Infection
)

# Compute Pearson correlation matrix
corr_mat <- cor(num_df, use = "pairwise.complete.obs", method = "pearson")

######################################
# Pretty Pearson correlation table with flextable 

# Round and  keep only the upper triangle for compactness
corr_rounded <- round(corr_mat, 3)

# Set lower triangle (excluding diagonal) to NA to reduce clutter 
keep_upper_only <- TRUE
if (keep_upper_only) {
  corr_rounded[lower.tri(corr_rounded)] <- NA
}

# Convert to data frame with rownames preserved
corr_df_print <- data.frame(Variable = rownames(corr_rounded),
                            corr_rounded, check.names = FALSE)

#  Flextable (small font + autofit so it fits on screen)
#if (!requireNamespace("flextable", quietly = TRUE)) install.packages("flextable")
library(flextable)

ft_corr <- flextable(corr_df_print) |>
  autofit() |>
  fontsize(size = 9, part = "all") |>
  align(align = "center", part = "all") |>
  bold(part = "header") |>
  bg(j = "Variable", bg = "#F5F5F5") |>
  align(j = "Variable", align = "left")

ft_corr  # <- prints the correlation matrix as a tidy table

###############################################################################
# Now showing the graphical representation 
# Graphical Pearson Correlation Matrix (heatmap)
#############################################################################


# Readable, boxed correlation heatmap (ggcorrplot)
#
if (!require(ggcorrplot)) install.packages("ggcorrplot")
library(ggcorrplot)
library(dplyr)

# Use exactly the variables in our regression
vars <- c("Overall_Risk_Score","Age","BMI","Smoking","Alcohol_Use",
          "Obesity","Family_History","Air_Pollution",
          "Occupational_Hazards","H_Pylori_Infection")

num_df <- eda_df %>% select(any_of(vars)) %>% select(where(is.numeric))

# Compute Pearson matrix
cor_mat <- cor(num_df, use = "pairwise.complete.obs", method = "pearson")

# Short display labels
disp_names <- c(
  Overall_Risk_Score   = "Risk_Score",
  Age                  = "Age",
  BMI                  = "BMI",
  Smoking              = "Smoking",
  Alcohol_Use          = "Alcohol",
  Obesity              = "Obesity",
  Family_History       = "Fam_Hx",
  Air_Pollution        = "AirPoll",
  Occupational_Hazards = "OccHaz",
  H_Pylori_Infection   = "H.Pylori"
)
colnames(cor_mat) <- disp_names[colnames(cor_mat)]
rownames(cor_mat) <- disp_names[rownames(cor_mat)]

# Draw heatmap (upper triangle) with borders and tight text
p <- ggcorrplot(
  cor_mat,
  type = "upper",
  method = "square",
  lab = TRUE,
  lab_size = 3.2,        # numeric labels size
  tl.cex = 9.5,          # axis label size
  tl.srt = 45,           # rotate x labels
  tl.col = "black",
  outline.color = "grey40",
  colors = c("#B2182B", "white", "#2166AC") # neg -> 0 -> pos
) +
  ggtitle("Pearson Correlation — Cancer Risk Predictors") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold"),
    panel.grid   = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.4),
    plot.margin  = margin(8, 16, 28, 16),     # more bottom space for angled labels
    axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1)
  )

print(p)

# ################################################################################
# Now the above heatmap is not as clean and visible 
# the next code makes the heatmap look better and the numbers to be visible 
#######################################################################################



########################Good map#####################
# Adjusted Code for Cleaner Heatmap
# 5) Pretty Pearson Correlation Heatmap 

if (!require(ggcorrplot)) install.packages("ggcorrplot")
library(ggcorrplot)

# Use the same variables selected for the EDA
vars_for_cor <- vars_eda

# 1. Select numeric variables from the EDA data frame using explicit package calls
num_df <- eda_df %>%
  dplyr::select(dplyr::any_of(vars_for_cor)) %>%
  dplyr::select(where(is.numeric))

# Check for empty data frame before computing correlation
if (ncol(num_df) < 2) {
  stop("Error: Less than two numeric variables found for correlation. Check 'eda_df' and 'vars_eda'.")
}

# 2. Compute Pearson matrix
cor_mat <- cor(num_df, use = "pairwise.complete.obs", method = "pearson")

# 3. Short display labels (CLEANED)
disp_names <- c(
  # CLEANED LINE BELOW
  Overall_Risk_Score = "Risk Score", 
  Age = "Age",
  BMI = "BMI",
  Smoking = "Smoking",
  Alcohol_Use = "Alcohol",
  Obesity = "Obesity",
  Family_History = "Fam. History",
  Air_Pollution = "Air Poll.",
  Occupational_Hazards = "Occ. Hazards",
  H_Pylori_Infection = "H. Pylori"
)
colnames(cor_mat) <- disp_names[colnames(cor_mat)]
rownames(cor_mat) <- disp_names[rownames(cor_mat)]

# #################################################################
# 4. Draw heatmap (upper triangle) with READABLE LABELS
# ###################################################################
p <- ggcorrplot(
  cor_mat,
  type = "upper",
  method = "square",
  lab = TRUE,
  lab_size = 4,         # INCREASED: Makes the numbers inside the boxes larger
  #lab_col = "black", 
  tl.cex = 10,          # INCREASED: Makes the axis labels larger
  tl.srt = 45,
  tl.col = "black",
  outline.color = "grey40",
  #colors = c("#F4A582", "white", "#92C5DE")
  colors = c("#B2182B", "white", "#2166AC")
) +
  ggtitle("Pearson Correlation — Cancer Risk Predictors") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold"),
    panel.grid   = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.4),
    plot.margin  = margin(8, 16, 28, 16),
    axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1)
  )

print(p)

# Note, need to save with higher dimensions for clarity

#############################################################################################
# ============================================================
# Step 6: Predictive Results — Multiple Linear Regression Model
# ============================================================

# Fit the multiple linear regression model
fit <- lm(Overall_Risk_Score ~ Age + BMI + Smoking + Alcohol_Use +
            Obesity + Family_History + Air_Pollution +
            Occupational_Hazards + H_Pylori_Infection,
          data = df)

# View model summary
summary(fit)

### Output I'm getting NA
##Running this code
colSums(is.na(df))


##check how many rows you can keep if you only use the variables with no missing values
model_data <- df %>%
  select(Overall_Risk_Score, Age, BMI, Family_History, H_Pylori_Infection) %>%
  drop_na()

nrow(model_data)

## Re-run the Model Using the Clean Variables

fit <- lm(Overall_Risk_Score ~ Age + BMI + Family_History + H_Pylori_Infection,
          data = model_data)
summary(fit)

##test whether any interaction terms (combinations of two predictors) 
##meaningfully improve your model


## we need to clean the dataset for us to fit in the anova
model_data2 <- df %>%
  select(Overall_Risk_Score, Age, BMI, Obesity,
         Family_History, H_Pylori_Infection) %>%
  drop_na()

nrow(model_data2)

##Fit base and interaction models on model_data2
# Base model (no interactions)
fit_base2 <- lm(Overall_Risk_Score ~ Age + BMI + Obesity +
                  Family_History + H_Pylori_Infection,
                data = model_data2)

summary(fit_base2)

# Model with interaction terms
fit_interaction <- lm(Overall_Risk_Score ~ Age * BMI + BMI * Obesity +
                        Family_History + H_Pylori_Infection,
                      data = model_data2)

summary(fit_interaction)

# Compare model fits (same dataset, so ANOVA is valid)
anova(fit_base2, fit_interaction)


## The non linear terms
# Model with quadratic (non-linear) terms
fit_nonlinear <- lm(Overall_Risk_Score ~ Age + I(Age^2) +
                      BMI + I(BMI^2) +
                      Obesity + Family_History + H_Pylori_Infection,
                    data = model_data2)

summary(fit_nonlinear)

# Compare to base model (linear)
anova(fit_base2, fit_nonlinear)

#######################################################################################
###The Evaluation Metrics######
##########################################################################################

# Install and load required packages
if (!require(Metrics)) install.packages("Metrics")
library(Metrics)

names(model_data)
## obesity is not in the column so create it again
# Create dataset with all predictors used in the model (no missing values)
model_data <- df %>%
  select(Overall_Risk_Score, Age, BMI, Obesity, Family_History, H_Pylori_Infection) %>%
  na.omit()

##re-fit your model and predict again:
# Refit the model using the clean dataset
fit_base2 <- lm(Overall_Risk_Score ~ Age + BMI + Obesity + Family_History + H_Pylori_Infection,
                data = model_data)

# Generate predictions safely
pred <- predict(fit_base2, newdata = model_data)

##computing the metrics
r2 <- summary(fit_base2)$r.squared
adj_r2 <- summary(fit_base2)$adj.r.squared
rmse_val <- rmse(model_data$Overall_Risk_Score, pred)
mae_val <- mae(model_data$Overall_Risk_Score, pred)

cat("R²:", round(r2, 4), "\n")
cat("Adjusted R²:", round(adj_r2, 4), "\n")
cat("RMSE:", round(rmse_val, 4), "\n")
cat("MAE:", round(mae_val, 4), "\n")

## presenting them in a flextable
# ===============================
# Create a flextable for Evaluation Metrics
# ===============================
#if (!require(flextable)) install.packages("flextable")
library(flextable)
library(dplyr)

# Combine results into a data frame
metrics_df <- data.frame(
  Metric = c("R² (Coefficient of Determination)",
             "Adjusted R²",
             "RMSE (Root Mean Squared Error)",
             "MAE (Mean Absolute Error)"),
  Value = c(round(r2, 4),
            round(adj_r2, 4),
            round(rmse_val, 4),
            round(mae_val, 4)),
  Interpretation = c(
    "Proportion of variation in cancer risk explained by predictors.",
    "Adjusted for number of predictors; measures generalization ability.",
    "Average magnitude of prediction error (lower is better).",
    "Average absolute difference between predicted and actual scores."
  )
)

# Create and format flextable
metrics_table <- flextable(metrics_df)
metrics_table <- autofit(metrics_table)
metrics_table <- fontsize(metrics_table, size = 10, part = "all")
metrics_table <- align(metrics_table, align = "center", part = "all")
metrics_table <- bold(metrics_table, part = "header")
metrics_table <- bg(metrics_table, bg = "#F8F8F8", part = "header")

# Print the table
metrics_table

#############################################################################################
#Plots of Individual Relationships
# ##############################################################################################
# Individual Relationship Plots (Response vs Each Predictor)
# ===========================================
#if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# List of predictors in the model
predictors <- c("Age", "BMI", "Obesity", "Family_History", "H_Pylori_Infection")

# Loop through each predictor and create scatter or box plots
for (var in predictors) {
  p <- ggplot(model_data, aes_string(x = var, y = "Overall_Risk_Score")) +
    geom_point(alpha = 0.4, color = "#0073C2FF") +
    geom_smooth(method = "lm", color = "red", se = TRUE) +
    labs(
      title = paste("Relationship between", var, "and Overall Risk Score"),
      x = var,
      y = "Overall Risk Score"
    ) +
    theme_minimal(base_size = 11)
  
  print(p)
}

#################################################################################################
##Generating diagnostic plots
# #####################################################################################################
# Diagnostic Plots for Multiple Linear Regression

par(mfrow = c(2, 2))   # 2x2 layout
plot(fit_base2)
par(mfrow = c(1, 1))   # reset layout



######################################################################################################
##Compute VIF (Without Interaction Terms)
#################################################################################################

##I have my main model (fit_base2), which does not include interaction terms perfect for running VIF
# #############################################
# Variance Inflation Factor (VIF) Check
# ####################################################
if (!require(car)) install.packages("car")
library(car)

# Compute VIF for base model (no interaction terms)
vif_values <- vif(fit_base2)

# Display results in a clear table
vif_table <- data.frame(
  Predictor = names(vif_values),
  VIF = round(vif_values, 3)
)
print(vif_table)

#pretty flextable for report
if (!require(flextable)) install.packages("flextable")
library(flextable)
ft_vif <- flextable(vif_table)
ft_vif <- autofit(ft_vif)
ft_vif

##########################################################################################
# VIF Bar Chart Visualization
# ########################################################################################

if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Convert to data frame if needed
vif_table <- data.frame(
  Predictor = names(vif_values),
  VIF = as.numeric(vif_values)
)

# Create the VIF bar chart
p_vif <- ggplot(vif_table, aes(x = reorder(Predictor, VIF), y = VIF)) +
  geom_col(fill = "#0073C2FF") +
  geom_text(aes(label = round(VIF, 3)), vjust = -0.5, size = 3.5) +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red", linewidth = 0.8) +
  labs(
    title = "Variance Inflation Factor (VIF) for Predictors",
    x = "Predictor Variables",
    y = "VIF Value"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_vif)







