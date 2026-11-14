## STS4407 Tutorial 2
# Name: Sametha Sivalingam
# Matrix No: SA00280

## ------------------------------------------------------------------------------------------------------

## create "output" directory if necessary
dir.create("output", showWarnings = FALSE)

## load libraries
library(tidyverse) # dplyr, ggplot2

## -------------------------------------------------------------------------------------------------------

## Data Description

# tree_id: Tree Identifier
# girth_in: Trunk diameter (inches)
# height_ft: Tree height (feet)
# volume_ft3: Timber volume (cubic feet) ; the response variable

#---------------------------------------------------------------------------------------------------------

## QUESTION 1(a)

trees_raw <- read.csv("data/STS4407_Tutorial2_Trees.csv")

## check number of trees
nrow(trees_raw) # 31 rows
length(unique(trees_raw$tree_id)) # 31 unique tree id's
# Since there are 31 rows and 31 unique tree id's, therefore there are 31 trees in this dataset

## check missing values
colSums(is.na(trees_raw))
# tree_id: 0
# girth_in: 0
# height_ft: 0
# volume_ft3: 0
# hence, no missing values in any of the columns

## identify diameter (girth_in) range
# min & max of trees_raw$girth_in
range(trees_raw$girth_in, na.rm = TRUE)
# min value = 8.3
# max value = 20.6
# range = [8.3, 20.6]

## Identify the response, predictor and their units
# reponse = volume_ft3 (timber volume, cubic feet)
# predictor = girth_in (trunk diameter, inches)


## volume-vs-diameter scatterplot
vol_diameter_plot <- ggplot(trees_raw,
    aes(x = girth_in, y = volume_ft3)) +
    geom_point(colour = "#b82b21") +
    labs(title = "Volume-vs-Diameter Scatterplot",
        x = "Trunk Diameter (inches)",
        y = "Timber Volume (cubic feet)") + theme_minimal()

vol_diameter_plot

ggsave("output/volume_diameter_scatterplot.png",
    plot = vol_diameter_plot,
    width = 7, 
    height = 5, 
    units = "in",
    dpi = 300,
    bg = "white")

# There is a strong, positive, slightly curved relationship between trunk diameter and timber volume.
# As trunk diameter increase, timber volume generally increases, with volume tending to rise more rapidly at
# larger diameters. The points follow the overall pattern fairly closely, with no obvious outliers.

## ---------------------------------------------------------------------------------------------------------

## QUESTION 1(b)

## fit M1
M1 <- lm(volume_ft3 ~ girth_in, data = trees_raw)

summary(M1)
# Fitted equation: Volume = -36.9435 + 5.0659(Diameter)
# R-squared = 0.9353
# Residual std error = 4.252

# Interpretation: For each additional 1 inch in trunk diameter, the predicted timber volume increase by
# approximately 5.07 cubic feet.
# The intercept should not be given a physical interpretation because it predicts a timber volume of
# approximate;y -36.94 when trunk diameter is zero. This is not physically meaningful because a negative
# timber volume is impossible.
# A zero timber volume is outside the observed data range, so applying the fitted line there is 
# extrapolation.

## ---------------------------------------------------------------------------------------------------------

## QUESTION 1(c)

## Tree with diameter 15 inches
new_tree_data <- data.frame(girth_in = 15.00)

## Fitted value and 95% confidence interval for mean volume
predict(M1, newdata = new_tree_data,
    interval = "confidence", level = 0.95)
# fitted value = 39.04439
# 95% C.I. = (37.24858, 40.84019) cubic feet

## Fitted value and 95% prediction interval for one tree
predict(M1, newdata = new_tree_data,
    interval = "prediction", level = 0.95)
# fitted value = 39.04439
# 95% Prediction Interval = (30.16461, 47.92416) cubic feet

## Why the intervals differ
# The confidence interval estimates the mean volume of all trees with a diameter of 15 inches. It reflects
# uncertainty in estimating the mean response. 
# The prediction interval predicts the volume of one individual tree with a diameter of 15 inches
# The prediction interval is wider because it includes uncertainty in the estimated mean and the additional
# variation between individual trees.
# This is interpolation since 15 inches lies within the observed diameter range of 8.3 - 20.6 inches.

## --------------------------------------------------------------------------------------------------------

## QUESTION 1(d)

tree_fit_df <- trees_raw %>%
    mutate(fitted_volume = fitted(M1),
        residual = residuals(M1))


tree_b05 <- tree_fit_df %>%
    filter(tree_id == "B05")
tree_b05

print(tree_b05["volume_ft3"] - tree_b05["fitted_volume"]) # 1.538795

# Observed Volume = 18.8 cubic feet
# Fitted Volume = 17.2612 cubic feet
# Residual = 1.538795 cubic feet
# The residual is positive which means Tree B05's observed volume is approximately 1.54 cubic feet greater
# than its fitted volume. M1 underestimates this tree's volume by approximately 1.54 cubic feet.
# The residual has the same unit as the volume ; cubic feet
# The residual is observable because it is calculated using the observed volume and the fitted regression line.
# It estimates the true error because the true population mean relationship is unknown.

## -------------------------------------------------------------------------------------------------------------

## QUESTION 2(a)

## Standardised Residual-vs-fitted plot
tree_fit_df <- tree_fit_df %>%
    mutate(std_residual = rstandard(M1))

stdr_fitted_plot <- ggplot(tree_fit_df,
    aes(x = fitted_volume, y = std_residual)) +
    geom_point(colour = "#7E0024", size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(method = "loess", formula = y ~ x, se = TRUE) + 
    labs(title = "Standardised Residual vs Fitted Volume",
        x = "Fitted Volume", y = "Standardised Residual") + theme_minimal()

ggsave("output/stdresid_vs_fitted_plot.png",
    plot = stdr_fitted_plot,
    width = 7, 
    height = 5, 
    units = "in",
    dpi = 300,
    bg = "white")

# The smooth trend is U-shaped: above zero at low-fitted values, below zero in the middle, and rising again
# at the high end. This suggests M1's linear mean is missing curvature, such as a quadratic term. The rise at
# the far right rests heavily on one point, so the evidence is suggestive not conclusive.
# The spread looks broadly similar across fitted values so changing variance is not obviously a 
# concern.  However, a plot with only 31 points cannot prove constant variance, it can only give some evidence for it.
# The shaded band (se = TRUE) shows the uncertainty in the smooth trend, which is wide at the extremes where
# data is sparse.

## -------------------------------------------------------------------------------------------------------------

## QUESTION 2(b)

## Normal Q-Q Plot of M1 standardised residuals
m1_qq_plot <- ggplot(tree_fit_df,
    aes(sample = std_residual)) +
    stat_qq(colour = "#7E0024") +
    stat_qq_line(colour = "#24778A") +
    labs(title = "Normal Q-Q Plot of M1 Standardised Residuals",
        x = "Theoretical normal quantile",
        y = "Ordered standardised residual")

ggsave("output/m1_norm_qq_stdresid_plot.png",
    plot = m1_qq_plot,
    width = 7, 
    height = 5, 
    units = "in",
    dpi = 300,
    bg = "white")

# The points in the middle follow the reference line closely. The centre looks consistent with normality.
# The tails depart slightly. At the lower end, the lowest points sit above the line, so the lower tail looks
# lighter than normal. At the upper end, the points from about 1 to 1.7 fall below the line, then the single
# largest point (about 2.5) lands on or just above it. This gives a mild right-skew pattern. 

# The errors are assumed normal.
# A Q-Q plot cant show independence because it ignores the order and structure of the data.
# The plot only shows the marginal distribution of the residuals.
## ------------------------------------------------------------------------------------------------------------

## QUESTION 2(c)

## Fit M2
M2 <- lm(volume_ft3 ~ girth_in + I(girth_in^2), data = trees_raw)
summary(M2)
# Fitted equation: Timber Volume = 10.78627 - 2.09214(Trunk Diameter) + 0.25454(Trunk Diameter^2)

tree_compare_fit <- trees_raw %>% 
    mutate(fit_M1 = fitted(M1),
        fit_M2 = fitted(M2))

m1_vs_m2_fits <- ggplot(tree_compare_fit, aes(x = girth_in, y = volume_ft3)) +
    geom_point(colour = "#7E0024", size = 2) +
    geom_line(aes(y = fit_M1, colour = "M1")) +
    geom_line(aes(y = fit_M2, colour = "M2")) +
    labs(title = "Volume vs Diameter with M1 and M2 Fits",
        x = "Trunk Diameter (in)", y = "Timber Volume (cubic feet)", colour = "Model") +
    theme_minimal()

ggsave("output/m1_vs_m2_fits.png",
    plot = m1_vs_m2_fits,
    width = 7, 
    height = 5, 
    units = "in",
    dpi = 300,
    bg = "white")

# M1 is a straight line (its slope is constant) whereas M2 is a curve that bends upward, getting steeper as
# diameter increases. 
# In the middle (about 12 to 17 in): M1 sits above many of the points, giving negative residuals,
# while M2 dips below M1 and tracks the data better.
# At large diameters (about 20 in): M1 falls short of the highest point (about 77), giving a positive residual,
# while M2 rises to meet it.
# However, M2's curve is fairly gentle (the two lines are not far apart)
# THe gap is largest at the two ends, so there is evidence of an improvement from the quadratic term (girth_in^2)
# but it is modest. 

# I(girth_in^2): `^` means interaction crossing, so I() makes R treat it as arithmetic squaring
# M2 is linear is its coefficients because the model is linear in b0, b1, b2 and only the predictor is transformed.
## ------------------------------------------------------------------------------------------------------------------

# -- QUESTION 2(d)

## Compare adjusted R-squared of M1 and M2
summary(M1)$adj.r.squared # 0.9330895
summary(M2)$adj.r.squared # 0.9588428

## Compare residual standard errors
summary(M1)$sigma # 4.251988
summary(M2)$sigma # 3.334785

## Add M2 fitted values and standardised residuals
tree_fit_df <- tree_fit_df %>%
    mutate(fitted_M2 = fitted(M2),
        std_resid_M2 = rstandard(M2))

## Standardised residual-vs-fitted plot for M2
stdr_fitted_M2_plot <- ggplot(tree_fit_df,
    aes(x = fitted_M2, y = std_resid_M2)) +
    geom_point(colour = "#7E0024", size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(method = "loess", formula = y ~ x, se = TRUE) +
    labs(title = "M2: Standardised Residual vs Fitted Volume",
        x = "Fitted Volume (cubic feet)", y = "Standardised Residual") +
    theme_minimal()

ggsave("output/stdresid_vs_fitted_M2.png",
    plot = stdr_fitted_M2_plot,
    width = 7, height = 5, units = "in", dpi = 300, bg = "white")

## Side-by-side comparison of M1 and M2 residual plots
resid_compare <- bind_rows(
    tree_fit_df %>% transmute(model = "M1", fitted = fitted_volume, std_resid = std_residual),
    tree_fit_df %>% transmute(model = "M2", fitted = fitted_M2, std_resid = std_resid_M2))

resid_compare_plot <- ggplot(resid_compare, aes(x = fitted, y = std_resid)) +
    geom_point(colour = "#7E0024", size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(method = "loess", formula = y ~ x, se = TRUE) +
    facet_wrap(~ model) +
    labs(title = "Standardised Residuals vs Fitted: M1 vs M2",
        x = "Fitted Volume (cubic feet)", y = "Standardised Residual") +
    theme_minimal()

ggsave("output/stdresid_M1_vs_M2.png",
    plot = resid_compare_plot,
    width = 9, height = 5, units = "in", dpi = 300, bg = "white")

## Comparison table (M1 vs M2)
m1_m2_table <- data.frame(
    Model = c("M1", "M2"),
    Adj_R2 = c(summary(M1)$adj.r.squared, summary(M2)$adj.r.squared),
    RSE = c(summary(M1)$sigma, summary(M2)$sigma))
m1_m2_table
write.csv(m1_m2_table, "output/m1_vs_m2_table.csv", row.names = FALSE)

# - M2 has the higher adjusted R-squared and the smaller RSE.
# - M1's smooth shows a U-shape; slightly flatter for M2.
# - Verdict: M2 gives the more adequate mean shape if the U-shape is reduced.
# - Limit: in-sample measures from only 31 trees; residual plots are subjective;
#   no hold-out data to check prediction.


## -------------------------------------------------------------------------------------------------------------

# -- QUESTION 3(a)

## Fit M3
M3 <- lm(sqrt(volume_ft3) ~ girth_in, data = trees_raw)
summary(M3)

# fitted equation: Sqrt(Volume) = -0.55183 + 0.44262(Diameter)


tree_fit_df <- tree_fit_df %>%
    mutate(fitted_M3 = fitted(M3),
        std_resid_M3 = rstandard(M3))

## Standardised residual-vs-fitted plot for M3
m3_resid_plot <- ggplot(tree_fit_df,
    aes(x = fitted_M3, y = std_resid_M3)) +
    geom_point(colour = "#7E0024", size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(method = "loess", formula = y ~ x, se = TRUE) +
    labs(title = "M3: Standardised Residual vs Fitted",
        x = "Fitted sqrt(Volume) (sqrt cubic feet)", y = "Standardised Residual") +
    theme_minimal()

ggsave("output/m3_stdresid_vs_fitted.png",
    plot = m3_resid_plot,
    width = 7, height = 5, units = "in", dpi = 300, bg = "white")

## Normal Q-Q plot for M3
m3_qq_plot <- ggplot(tree_fit_df, aes(sample = std_resid_M3)) +
    stat_qq(colour = "#7E0024") +
    stat_qq_line(colour = "#24778A") +
    labs(title = "Normal Q-Q Plot of M3 Standardised Residuals",
        x = "Theoretical normal quantile",
        y = "Ordered standardised residual") +
    theme_minimal()

ggsave("output/m3_norm_qq_stdresid_plot.png",
    plot = m3_qq_plot,
    width = 7, height = 5, units = "in", dpi = 300, bg = "white")

# Interpretation (write after viewing your plots):
# - Compare M3's residual smooth with M1's U-shape: is the curvature reduced?
# - Remember the response is sqrt(volume) in sqrt(cubic feet), not cubic feet,
#   so M3's slope is the change in sqrt(volume) per inch of diameter.
# - Comment on the Q-Q plot centre and tails.

## -------------------------------------------------------------------------------------------------------------
## QUESTION 3(b)

## Check that the log transformation is defined (volume must be > 0)
any(trees_raw$volume_ft3 <= 0)   # FALSE = all volumes positive, so log is defined
min(trees_raw$volume_ft3) # 10.2

## Fit M4  (natural log; response scale = log(cubic feet))
M4 <- lm(log(volume_ft3) ~ girth_in, data = trees_raw)
summary(M4)
# Fitted equation: log(Volume) = 1.118997 + 0.162566(Diameter)  

tree_fit_df <- tree_fit_df %>%
    mutate(fitted_M4 = fitted(M4),
        std_resid_M4 = rstandard(M4))

## Standardised residual-vs-fitted plot for M4
m4_resid_plot <- ggplot(tree_fit_df,
    aes(x = fitted_M4, y = std_resid_M4)) +
    geom_point(colour = "#7E0024", size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(method = "loess", formula = y ~ x, se = TRUE) +
    labs(title = "M4: Standardised Residual vs Fitted",
        x = "Fitted log(Volume) (log cubic feet)", y = "Standardised Residual") +
    theme_minimal()

ggsave("output/m4_stdresid_vs_fitted.png",
    plot = m4_resid_plot,
    width = 7, height = 5, units = "in", dpi = 300, bg = "white")

## Normal Q-Q plot for M4
m4_qq_plot <- ggplot(tree_fit_df, aes(sample = std_resid_M4)) +
    stat_qq(colour = "#7E0024") +
    stat_qq_line(colour = "#24778A") +
    labs(title = "Normal Q-Q Plot of M4 Standardised Residuals",
        x = "Theoretical normal quantile",
        y = "Ordered standardised residual") +
    theme_minimal()

ggsave("output/m4_norm_qq_stdresid_plot.png",
    plot = m4_qq_plot,
    width = 7, height = 5, units = "in", dpi = 300, bg = "white")

## Side-by-side M3 vs M4 residual plots
resid_m3_m4 <- bind_rows(
    tree_fit_df %>% transmute(model = "M3 (sqrt scale)", fitted = fitted_M3, std_resid = std_resid_M3),
    tree_fit_df %>% transmute(model = "M4 (log scale)", fitted = fitted_M4, std_resid = std_resid_M4))

resid_m3_m4_plot <- ggplot(resid_m3_m4, aes(x = fitted, y = std_resid)) +
    geom_point(colour = "#7E0024", size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(method = "loess", formula = y ~ x, se = TRUE) +
    facet_wrap(~ model, scales = "free_x") +
    labs(title = "Standardised Residuals vs Fitted: M3 vs M4",
        x = "Fitted value (model scale)", y = "Standardised Residual") +
    theme_minimal()

ggsave("output/stdresid_M3_vs_M4.png",
    plot = resid_m3_m4_plot,
    width = 9, height = 5, units = "in", dpi = 300, bg = "white")

# Interpretation

## -------------------------------------------------------------------------------------------------------------
## QUESTION 3(c)

## Predictions at diameter = 15 inches (new_tree_data from Q1(c))
pred_M1 <- predict(M1, newdata = new_tree_data)   # cubic feet
pred_M3 <- predict(M3, newdata = new_tree_data)   # sqrt(cubic feet)
pred_M4 <- predict(M4, newdata = new_tree_data)   # log(cubic feet)

## Direct inverses back to the volume scale
pred_M3_vol <- pred_M3^2      # square the M3 prediction
pred_M4_vol <- exp(pred_M4)   # exponentiate the M4 prediction

pred_table <- data.frame(
    Model = c("M1", "M3", "M4"),
    Model_scale_prediction = c(pred_M1, pred_M3, pred_M4),
    Model_scale = c("cubic feet", "sqrt(cubic feet)", "log(cubic feet)"),
    Volume_scale_prediction = c(pred_M1, pred_M3_vol, pred_M4_vol))
pred_table
write.csv(pred_table, "output/predictions_at_15in.csv", row.names = FALSE)

# Compare M3 and M4 volume-scale predictions with M1 (39.04 cubic feet).
# Why direct inverses are not automatically arithmetic conditional mean volumes:
# - The models estimate E[g(Y) | x], and in general E[g^-1(g(Y))] != g^-1(E[g(Y)])
#   because g^-1 (square or exp) is a non-linear function (Jensen's inequality).
# - For M4, exp(prediction) is closer to a conditional median than a mean.
# - Getting the mean back needs a bias adjustment (not required here).

## -------------------------------------------------------------------------------------------------------------
## QUESTION 3(d)

## Summary table for M1-M4 (supports the recommendation; do NOT rank on these alone)
model_summary <- data.frame(
    Model = c("M1", "M2", "M3", "M4"),
    Response = c("volume (ft3)", "volume (ft3)", "sqrt(volume) (sqrt ft3)", "log(volume) (log ft3)"),
    Adj_R2 = c(summary(M1)$adj.r.squared, summary(M2)$adj.r.squared,
        summary(M3)$adj.r.squared, summary(M4)$adj.r.squared),
    RSE = c(summary(M1)$sigma, summary(M2)$sigma,
        summary(M3)$sigma, summary(M4)$sigma))
model_summary
write.csv(model_summary, "output/model_summary_M1_M4.csv", row.names = FALSE)

# Recommendation (120-180 words) goes in the PDF report. Include:
# - Preferred model, justified by residual and Q-Q diagnostics
# - Response scales: M1/M2 ft3, M3 sqrt(ft3), M4 log(ft3)
# - At least two limitations (n = 31, one predictor, back-transformation bias, in-sample only)
# - Why R2 / RSE cannot rank models across response scales (different units and scales)
# - Extra evidence: cross-validation or hold-out set, errors compared on the original volume scale