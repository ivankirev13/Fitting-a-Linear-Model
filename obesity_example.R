
## Country-level data obtained from Our World in Data
deaths <- read.csv("total-deaths-covid-19.csv")
cases <- read.csv("total-cases-covid-19.csv")
obesity <- read.csv("share-of-adults-defined-as-obese.csv")

## Subset to cases/deaths as of 30 Apr 2020
sub.cases <- subset(cases, Date == "Apr 30, 2020")[,c("Entity", "Code", "Total.confirmed.cases.of.COVID.19..cases.")]
names(sub.cases)[3] <- "Cases"

sub.deaths <- subset(deaths, Date == "Apr 30, 2020")[,c("Entity", "Total.confirmed.deaths.due.to.COVID.19..deaths.")]
names(sub.deaths)[2] <- "Deaths"

## Subset to obesity as of 2016 (most recent year available)
sub.obesity <- subset(obesity, Year == 2016)[,c("Entity", "Share.of.adults.who.are.obese....")]
names(sub.obesity)[2] <- "Obese"

## Merge data so each Entity has columns for Cases, Deaths and Obese in a single object
data.tmp <- merge(x=sub.cases, y=sub.deaths, by="Entity") # merging cases and deaths
data <- merge(x=data.tmp, y=sub.obesity, by="Entity")

## Subset to countries (Europe and Africa also appear)
data <- subset(data, !(Entity %in% c("Europe", "Africa")))

# Calculate case-fatality ratio for each country
data$DCR <- with(data, (Deaths / Cases)*100)

# Create a scatterplot of CFR and Obesity
plot(DCR ~ Obese, data = data, xlab = "Obesity rate x 100%", ylab = "DCR x 100%")

# Fit the linear regression model
fit <- lm(DCR ~ Obese, data = data)

# Add the fitted line to the scatterplot
abline(fit, col = "blue", lwd=2)

## For countries differing by 1% in obesity rate, we expect the case-fatality ratio to be
## 0.03% greater in the country with higher obesity rate.
summary(fit)

## Confidence interval
confint(fit)
