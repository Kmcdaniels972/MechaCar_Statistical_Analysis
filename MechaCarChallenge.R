library(dplyr)

## Deliverable 1: Linear Regression of Prototype Mecha Car MPG Data
# Load the Data
mecha_car <- read.csv(file='MechaCar_mpg.csv')

# Create the model
model = lm(formula = mpg ~ vehicle_length + vehicle_weight + ground_clearance + spoiler_angle + AWD, mecha_car)
summary(model)


## Deliverable 2: Descriptive statistics or production level supspension ciol PSI
# Load the data
susp_coil <- read.csv(file='Suspension_Coil.csv')

# Summarize the data
total_summary <- susp_coil %>% summarize(Total_Lots=3, Min_PSI=min(PSI), Mean_PSI=mean(PSI), Median_PSI=median(PSI), Max_PSI=max(PSI), Production_Variance=sd(PSI)**2, St_Dev=sd(PSI), .groups = 'keep')

lot_summary <- susp_coil %>% group_by(Manufacturing_Lot) %>% summarize(Min_PSI=min(PSI), Mean_PSI=mean(PSI), Median_PSI=median(PSI), Max_PSI=max(PSI), Production_Variance=sd(PSI)**2, St_Dev=sd(PSI), .groups = 'keep')


## Deliverable 3: T-Test of suspension coil production lots
# All current production vs Population mean of 1500 PSI
total_production_test <- t.test(susp_coil$PSI, mu=1500)

# Each lot production vs population mean of 1500 PSI
lot_1 <- subset(susp_coil, Manufacturing_Lot == "Lot1")
ttest_lot1 <- t.test(lot_1$PSI, mu=1500)

lot_2 <- subset(susp_coil, Manufacturing_Lot == "Lot2")
ttest_lot2 <- t.test(lot_2$PSI, mu=1500)

lot_3 <- subset(susp_coil, Manufacturing_Lot == "Lot3")
ttest_lot3 <- t.test(lot_3$PSI, mu=1500)

# Create a Summary table of the test results
T.Value <- c(ttest_lot1$statistic, ttest_lot2$statistic, ttest_lot3$statistic, total_production_test$statistic)
P.Value <- c(ttest_lot1$p.value, ttest_lot2$p.value, ttest_lot3$p.value, total_production_test$p.value)
DF <- c(ttest_lot1$parameter, ttest_lot2$parameter, ttest_lot3$parameter, total_production_test$parameter)
Mean <- c(ttest_lot1$estimate, ttest_lot2$estimate, ttest_lot3$estimate, total_production_test$estimate)
Manufacturing_Lot <- c("Lot 1", "Lot 2", "Lot 3", "Total Production")


ttest_summaries = tibble(Manufacturing_Lot, T.Value, DF, P.Value, Mean)