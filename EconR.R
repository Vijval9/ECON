library(ggplot2)
library(dplyr)
library(fastDummies)

# DISCLOSURE: Some help was taken from AI for the R Syntax and the beautification of graphs
# Since I am not familiar with R, I first wrote down whatever code I could, however, the graphs weren't readable,
# and the dummy variables were causing an issue, due to which I took some help from the Claude Chatbot

finalcsv <- read.csv("C:/Users/vijva/Documents/ECON/Project/FinalData.csv")    # loading data

# Q1: Regressing on SDP

finalcsv <- finalcsv[complete.cases(finalcsv),]           # removing missing values
finalcsv <- na.exclude(finalcsv)

regress_linear <- lm(chloride ~ sdp, data = finalcsv)         # regressing on sdp
summary(regress_linear)

regress_on_gini <- lm(chloride~gini, data=finalcsv)
summary(regress_on_gini)

#=============================================================================================================

# Q2: Plotting 

#Boxplot of GWQ
library(ggplot2)

ggplot(finalcsv, aes(x = factor(year), y = chloride)) +
  geom_boxplot() +  
  labs(x = "Year", y = "Chloride Concentration",title = "District level GWQ for 2000-2018") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), 
    plot.title = element_text(hjust = 0.5) 
  )


# Plot residuals vs SDP

ggplot(data = finalcsv, aes(x = sdp, y = regress_linear$residuals)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "loess", color = "darkred") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "SDP in Rupees Crore",
       y = "Residuals",
       title = "Plotting Residuals vs the SDP") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Plot GWQ vs SDP
ggplot(data = finalcsv, aes(x = sdp, y = chloride)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "loess", color = "darkred") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "SDP in Rupees Crore",
       y = "GWQ In terms of Chloride in mg/L",
       title = "Plotting GWQ vs the SDP") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# Plotting Estimates from the Regression vs the SDP
ggplot(data = finalcsv, aes(x = sdp, y = regress_linear$fitted.values)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "loess", color = "darkred") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "SDP",
       y = "Estimates from The Regression",
       title = "Plotting Estimates vs the SDP") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


#==========================================================================================================


# Q3: Plotting Histogram of Residuals and Summing them

# Histogram of residuals
hist(regress_linear$residuals,
     main = "Histogram of Residuals",  # Main title
     xlab = "Residuals",                # X-axis label
     ylab = "Frequency",                # Y-axis label
     col = "blue",                 # Color of the bars
     border = "white",                  # Border color of the bars
     las = 1,                           # Rotate X-axis labels
     cex.lab = 1.2,                     # Increase size of axis labels
     cex.main = 1.4)                    # Increase size of main title

# Add a vertical line at zero
abline(v = 0, col = "white", lwd = 2)     # Vertical line at zero
sum(regress_linear$residuals)   # nearly 0, as expected

#============================================================================================================

# Q4: Enhanced Model:

regress_non_linear <- lm(chloride ~ sdp + I(sdp**2) + I(sdp**3) + gini, data = finalcsv)
summary(regress_non_linear)


# Plotting Residuals against SDP
ggplot(data = finalcsv, aes(x = sdp, y = regress_non_linear$residuals)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "loess", color = "darkred") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "SDP in Rupees Crore",
       y = "Residuals",
       title = "Plotting Residuals vs the SDP") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#plotting histogram of residuals
hist(regress_non_linear$residuals,
     main = "Histogram of Residuals",  # Main title
     xlab = "Residuals",                # X-axis label
     ylab = "Frequency",                # Y-axis label
     col = "blue",                 # Color of the bars
     border = "white",                  # Border color of the bars
     las = 1,                           # Rotate X-axis labels
     cex.lab = 1.2,                     # Increase size of axis labels
     cex.main = 1.4)                    # Increase size of main title

# Add a vertical line at zero
abline(v = 0, col = "white", lwd = 2)     # Vertical line at zero
sum(regress_non_linear$residuals)   # nearly 0, as expected

# Identifying and plotting influential observations using DFFITS and DFBETAs

dff <- dffits(regress_non_linear)
k <- length(regress_non_linear$coefficients)
n <- nrow(finalcsv)
threshold <- 2*sqrt(k/n)

influential <- dff[dff < -threshold | dff > threshold]
plot(dff,type = 'h', x = finalcsv$sdp, xlab = 'SDP', ylab = 'DFFITS', main = 'Plotting DFFITS against SDP')
abline(h = threshold, lty = 2)
abline(h = -threshold, lty = 2)

plot(dff,type = 'h', xlab = 'Observation', ylab = 'DFFITS', main = 'Plotting DFFITS for each observation')
abline(h = threshold, lty = 2)
abline(h = -threshold, lty = 2)

(length(influential)/length(dff))*100 # percentage of influential obs on the basis on dffits


influential_indices <- as.numeric(names(influential))
finalcsv$is_influential <- rownames(finalcsv) %in% influential_indices

# Highlighting Influential Obs on the basis of DFFITS
ggplot(data = finalcsv, aes(x = sdp, y = chloride, color = is_influential)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "darkred") +
  scale_color_manual(values = c("steelblue", "orange"), labels = c("Regular", "Influential")) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "SDP in Rupees Crore",
       y = "GWQ In terms of Chloride in mg/L",
       title = "Plotting GWQ vs the SDP",
       color = "Point Type") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data = finalcsv, aes(x = X, y = chloride, color = is_influential)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "darkred") +
  scale_color_manual(values = c("steelblue", "orange"), labels = c("Regular", "Influential")) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Index",
       y = "GWQ In terms of Chloride in mg/L",
       title = "Plotting GWQ vs the Index",
       color = "Point Type") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

library(ggplot2)
ggplot(data = finalcsv, aes(x = sdp, y = chloride)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "loess", color = "darkred") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "SDP in Rupees Crore",
       y = "GWQ In terms of Chloride in mg/L",
       title = "Plotting GWQ vs the SDP") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



dfb <- dfbetas(regress_non_linear)
threshold2 <- 2/sqrt(n)
influential_betas <- dfb[dfb < -threshold2 | dfb > threshold2]
plot(dfb,type = 'h')
abline(h = threshold2, lty = 2)
abline(h = -threshold2, lty = 2)

install.packages("reshape")
library(reshape)

install.packages('car')
library('car')

dfbetasPlots(regress_non_linear, terms= ~ ., intercept=TRUE, layout=NULL, 
             labels=rownames(dfbeta), 
             id.method = "identify",
             id.n=if(id.method[1]=="identify") Inf else 0, id.cex=1,
             id.col=carPalette()[1], id.location="lr", col=carPalette()[1], grid=TRUE)





library(ggplot2)

# Plotting the Estimates from this Regression vs the SDP
ggplot(data = finalcsv, aes(x = sdp, y = regress_non_linear$fitted.values)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "loess", color = "darkred") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "SDP",
       y = "Estimates from The Regression",
       title = "Plotting Estimates vs the SDP") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Plotting the Residuals against SDP
ggplot(data = finalcsv, aes(x = gini, y = regress_non_linear$residuals)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "loess", color = "darkred") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Gini Value",
       y = "Estimates from The Regression",
       title = "Plotting Estimates vs the Gini value") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# identification of outliers
sdr <- rstudent(regress_non_linear)
outliers <- sdr[abs(sdr)>3]
plot(sdr,type = 'h',main = 'RSTUDENT vs index', ylab = 'RSTUDENT')
abline(h = 3, lty = 2)
abline(h = -3, lty = 2)

outlier_indices <- as.numeric(names(outliers))
finalcsv$is_outlier <- rownames(finalcsv) %in% outlier_indices

# Plotting outliers
ggplot(data = finalcsv, aes(x = X, y = chloride, color = is_outlier)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "darkred") +
  scale_color_manual(values = c("steelblue", "orange"), labels = c("Regular", "Outlier")) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Index",
       y = "GWQ In terms of Chloride in mg/L",
       title = "Plotting GWQ vs the Index",
       color = "Point Type") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data = finalcsv, aes(x = sdp, y = chloride, color = is_outlier)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "darkred") +
  scale_color_manual(values = c("steelblue", "orange"), labels = c("Regular", "Outlier")) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "SDP",
       y = "GWQ In terms of Chloride in mg/L",
       title = "Plotting GWQ vs the SDP",
       color = "Point Type") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))












#============================================================================================================


# Q6: Adding dummy variables for years

newcsv <- dummy_cols(finalcsv,select_columns = "year",remove_first_dummy = TRUE,remove_selected_columns = TRUE)
View(newcsv)

library(dplyr)
year_vars <- paste0("year_",2001:2018)
newcsv <- newcsv %>%
  mutate(across(year_vars, ~sdp * .x, .names = "sdp_{col}")) %>%
  mutate(across(year_vars, ~sdp^2 * .x, .names = "sdp2_{col}")) %>%
  mutate(across(year_vars, ~sdp^3 * .x, .names = "sdp3_{col}")) %>%
  mutate(across(year_vars, ~gini * .x, .names = "gini_{col}"))

excluded_columns <- c("X","chloride","state", "district", "intercept")
formula_str <- paste("chloride ~ I(sdp**2) + I(sdp**3) + ", paste(setdiff(names(newcsv), excluded_columns), collapse = " + "))
regress_yearwise <- lm(as.formula(formula_str), data = newcsv)
summary(regress_yearwise)

library(ggplot2)

# Extract gini coefficients from model summary
coef_summary <- summary(regress_yearwise)$coefficients
gini_coefs <- coef_summary[grep("^gini_", rownames(coef_summary)), ]
gini_coefs <- data.frame(
  year = as.numeric(gsub("^gini_", "", rownames(gini_coefs))),
  estimate = gini_coefs[, "Estimate"],
  std.error = gini_coefs[, "Std. Error"]
)


ggplot(gini_coefs, aes(x = substr(rownames(gini_coefs), nchar(rownames(gini_coefs)) - 1, nchar(rownames(gini_coefs))), y = estimate)) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "Coefficient Estimate", title = "Gini Coefficient Estimates") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  theme_bw()

ggplot(data = finalcsv, aes(x = sdp, y = regress_non_linear$fitted.values, group = year, color = factor(year))) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "SDP",
       y = "Estimated GWQ",
       title = "Plotting Estimated GWQ vs the SDP",
       color = "Year") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")




#============================================================================================================


# Q7: Adding Dummy variables for regions

finalcsvreg <- read.csv("C:/Users/vijva/Documents/ECON/Project/finalwithregions.csv")
finalcsvreg <- finalcsvreg[complete.cases(finalcsvreg),]
newcsv <- dummy_cols(finalcsvreg,select_columns = "region",remove_first_dummy = TRUE,remove_selected_columns = TRUE)
View(newcsv)
newcsv <- na.exclude(newcsv)


regions <- c("eastern_region", "north_eastern_region", "northern_region", "southern_region", "western_region")
region_vars <- paste0("region_", regions)
library(dplyr)
newcsv <- newcsv %>%
  mutate(across(region_vars, ~sdp * .x, .names = "sdp_{col}")) %>%
  mutate(across(region_vars, ~sdp^2 * .x, .names = "sdp2_{col}")) %>%
  mutate(across(region_vars, ~sdp^3 * .x, .names = "sdp3_{col}")) %>%
  mutate(across(region_vars, ~gini * .x, .names = "gini_{col}"))

excluded_columns <- c("X","chloride","state","year", "district", "intercept")
formula_str <- paste("chloride ~ I(sdp**2) + I(sdp**3) + ", paste(setdiff(names(newcsv), excluded_columns), collapse = " + "))
regress_regionwise <- lm(as.formula(formula_str), data = newcsv)
summary(regress_regionwise)


library(ggplot2)
library(dplyr)

# Plotting Chloride conc regionwise vs sdp


ggplot(data = finalcsvreg, aes(x = sdp, y = chloride, group = region, color = factor(region))) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) + 
  labs(x = "SDP",
       y = "Chloride",
       title = "Plotting Chloride vs the SDP",
       color = "Region") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")

regress_reg <- lm(chloride ~ sdp + I(sdp**2) + I(sdp**3) + gini, data = finalcsvreg)

ggplot(data = finalcsvreg, aes(x = sdp, y = regress_reg$fitted.values, group = region, color = factor(region))) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "SDP",
       y = "Estimated GWQ",
       title = "Plotting Estimated GWQ vs the SDP",
       color = "Region") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom")


