library(readxl)

neutral_data <- read_excel("Neutral Landscape with defit def50.xlsx")

hist(neutral_data$defit)
hist(neutral_data$def50)

plot(neutral_data$def50,neutral_data$defit)
lines(lowess(neutral_data$def50,neutral_data$defit),col="blue")

mod <- lm(defit~def50,data=neutral_data)
summary(mod)


