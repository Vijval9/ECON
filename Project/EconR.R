library(ggplot2)
finalcsv <- read.csv("C:/Users/vijva/Documents/ECON/Project/finalfinal.csv")
regress_on_sdp <- lm(chloride~sdp,data=finalcsv)
regress_on_sdp23 <- lm(chloride ~ sdp + I(sdp^2) + I(sdp^3) + gini,data=finalcsv)
summary(regress_on_sdp)

n <- nrow(finalcsv)
threshold <- 2/sqrt(n)

dfbetas <- as.data.frame(dfbetas(regress_on_sdp))
dfbetas

finalcsv <- read.csv("C:/Users/vijva/Documents/ECON/Project/finalfinal.csv")
finalcsv <- finalcsv[complete.cases(finalcsv),]
newcsv <- dummy_cols(finalcsv,select_columns = "year",remove_first_dummy = TRUE,remove_selected_columns = TRUE)
View(newcsv)
year_vars <- grep("^year",names(newcsv),value = TRUE)
regress_yearwise <- lm(chloride ~ sdp + I(sdp**2) + I(sdp**3) + gini + sdp:year_vars + I(sdp**2):year_vars + I(sdp**3):year_vars + gini:year_vars,data = newcsv)

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


finalcsvreg <- read.csv("C:/Users/vijva/Documents/ECON/Project/finalwithregions.csv")
finalcsvreg <- finalcsvreg[complete.cases(finalcsv),]
newcsv <- dummy_cols(finalcsvreg,select_columns = "region",remove_first_dummy = TRUE,remove_selected_columns = TRUE)
View(newcsv)
year_vars <- grep("^year",names(newcsv),value = TRUE)
newcsv <- na.exclude(newcsv)
library(dplyr)
regions <- c("eastern_region", "north_eastern_region", "northern_region", "southern_region", "western_region")
region_vars <- paste0("region_", regions)
newcsv <- newcsv %>%
 mutate(across(region_vars, ~sdp * .x, .names = "sdp_{col}")) %>%
 mutate(across(region_vars, ~sdp^2 * .x, .names = "sdp2_{col}")) %>%
 mutate(across(region_vars, ~sdp^3 * .x, .names = "sdp3_{col}")) %>%
 mutate(across(region_vars, ~gini * .x, .names = "gini_{col}"))

excluded_columns <- c("X","chloride","state","year", "district", "intercept")
formula_str <- paste("chloride ~ I(sdp**2) + I(sdp**3) + ", paste(setdiff(names(newcsv), excluded_columns), collapse = " + "))
regress_yearwise <- lm(as.formula(formula_str), data = newcsv)
summary(regress_yearwise)
