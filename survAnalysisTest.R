##HOUSEKEEPING##

library(survival)
library(dplyr)
library(tidyr)
library(ggplot2)
library(survminer)

read.csv("C:/Users/bluri/Downloads/MFG10YearTerminationData.csv") -> data

################

# Detect whether termination was due to retirements. 

# Create cases of when termination didn't happen (0), termination happened due to target event (1),
# and termination occured due to some other reason, (censored data, 2)

data %>% mutate(termEvent = (termreason_desc == "Retirement") * 1) -> data

data$termEvent[data$termreason_desc == "Layoff" | data$termreason_desc == "Resignation"] <- 2

# Inspect Length of Service #
hist(data$length_of_service)
hist(subset(data, termEvent == 0)$length_of_service)
hist(subset(data, termEvent == 1)$length_of_service)

min(subset(data, termEvent == 1)$length_of_service)

# Retirees observed have served for at least 8 years in this fictious company.
# Length of service measured in years.

# Kaplan-Meier Analysis #

# Create Censored Record #
data$censored[data$termEvent == 2] <- 1

# Create Model terms #
kmWeight1 <- survfit(Surv(length_of_service, censored) ~ 1, data = data, type = 'kaplan-meier')

print(kmWeight1)

summary(kmWeight1)
install.packages('survminer')

# plot #
plot(kmWeight1)