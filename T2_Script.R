## STS4407 Tutorial 2
# Name: Sametha Sivalingam
# Matrix No: SA00280

## ------------------------------------------------------------------------------------------------------

## create "output" directory if necessary
# dir.create("output", showWarnings = FALSE)

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
unique(trees_raw$tree_id) # 31 unique tree id's
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
# concern. 
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

## ------------------------------------------------------------------------------------------------------------

## QUESTION 2(c)

## Fit M2
M2 <- lm(volume_ft3 ~ girth_in + I(girth_in^2), data = trees_raw)
summary(M2)

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

## ------------------------------------------------------------------------------------------------------------------

# -- QUESTION 2(d)

## Compare adjusted R-squared of M1 and M2
summary(M1)$adj.r.squared # 0.9330895
summary(M2)$adj.r.squared # 0.9588428

## Compare residual standard errors
summary(M1)$sigma # 4.251988
summary(M2)$sigma # 3.334785

## Compare Standardised Residual Plots










## -------------------------------------------------------------------------------------------------------------

# -- QUESTION 3(a)

## Fit M3
M3 <- lm(sqrt(volume_ft3) ~ girth_in, data = trees_raw)
summary(M3)

# fitted equation: Sqrt(Volume) = -0.55183 + 0.44262(Diameter)