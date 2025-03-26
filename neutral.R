###############################
# Beta regression analysis
# Programmer: Miguel Acevedo
##################################

# Load the necessary packages
library(readxl)

# Load the data
neutral_data <- read_excel("Neutral Landscape with defit def50.xlsx")

# Look at the data
hist(neutral_data$defit)
hist(neutral_data$def50)

# Plot deforestation inside IT vs deforestation at 50km buffer
plot(neutral_data$def50, neutral_data$defit)
lines(lowess(neutral_data$def50, neutral_data$defit), col = "blue")

# Fit a linear model
mod <- lm(defit ~ def50, data = neutral_data)
summary(mod)


