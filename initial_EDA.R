
# Gillian Haggerty initial analysis --------------------------------------

# Load packages -----------------------------------------------------------

library(readr)
library(tidyverse)
library(dplyr)
library(psych)
library(modelr)
library(dataMaid)

# Analysis of each variable -----------------------------------------------

describe(LED_clean)

# Relationship between each variable and target (life expectancy) ---------

# the goal of these plots is to get an initial idea what variables may be worth looking at (based on their 
# correlation with life expectancy)

ggplot(LED_clean) +
  geom_point(mapping = aes(x = adult_mortality, y = life_expectancy))
#of note
ggplot(LED_clean) +
  geom_point(mapping = aes(x = infant_deaths, y = life_expectancy)) +
  geom_smooth(mapping = aes(x = infant_deaths, y = life_expectancy))
ggplot(LED_clean) +
  geom_point(mapping = aes(x = alcohol, y = life_expectancy))
ggplot(LED_clean) +
  geom_point(mapping = aes(x = percentage_expenditure, y = life_expectancy))
ggplot(LED_clean) +
  geom_point(mapping = aes(x = hepatitis_b, y = life_expectancy))
ggplot(LED_clean) +
  geom_point(mapping = aes(x = bmi, y = life_expectancy))
ggplot(LED_clean) +
  geom_point(mapping = aes(x = under_five_deaths, y = life_expectancy))
ggplot(LED_clean) +
  geom_point(mapping = aes(x = polio, y = life_expectancy))
ggplot(LED_clean) +
  geom_point(mapping = aes(x = total_expenditure, y = life_expectancy))
ggplot(LED_clean) +
  geom_point(mapping = aes(x = diphtheria, y = life_expectancy))
ggplot(LED_clean) +
  geom_point(mapping = aes(x = hiv_aids, y = life_expectancy))
ggplot(LED_clean) +
  geom_point(mapping = aes(x = gdp, y = life_expectancy))
ggplot(LED_clean) +
  geom_point(mapping = aes(x = population, y = life_expectancy))
ggplot(LED_clean) +
  geom_point(mapping = aes(x = thinness_10_to_19_years, y = life_expectancy))
ggplot(LED_clean) +
  geom_point(mapping = aes(x = thinness_5_to_9_years, y = life_expectancy))
ggplot(LED_clean) +
  geom_point(mapping = aes(x = income_composition_of_resources, y = life_expectancy)) +
  geom_smooth(mapping = aes(x = income_composition_of_resources, y = life_expectancy))
#of note
ggplot(LED_clean) +
  geom_point(mapping = aes(x = schooling, y = life_expectancy)) + 
  geom_smooth(mapping = aes(x = schooling, y = life_expectancy))
#of note


# Inspecting covariation --------------------------------------------------

# looking at variables that both had similarly oriented correlations to target var (life expec)

ggplot(LED_clean) +
  geom_bin2d(mapping = aes(x = schooling, y = income_composition_of_resources))
#demonstrates a positive correlation

ggplot(LED_clean) +
  geom_bin2d(mapping = aes(x = adult_mortality, y = income_composition_of_resources))

ggplot(LED_clean) +
  geom_bin2d(mapping = aes(x = schooling, y = adult_mortality))

x <- LED_clean %>%
  filter(!is.na(LED_clean$schooling)) %>%
  select(schooling)
y <- LED_clean %>%
  filter(!is.na(LED_clean$income_composition_of_resources)) %>%
  select(income_composition_of_resources)
cor(x, y)

b <- !is.na(LED_clean$adult_mortality)
c <- !is.na(LED_clean$life_expectancy)

sum(is.na(LED_clean$adult_mortality))
sum(is.na(LED_clean$life_expectancy))
cor(b,y)

ggplot(LED_clean) +
  geom_bin2d(mapping = aes(x = gdp, y = percentage_expenditure))
ggplot(LED_clean) +
  geom_point(mapping = aes(x = gdp, y = percentage_expenditure)) +
  geom_smooth(mapping = aes(x = gdp, y = percentage_expenditure))

ggplot(LED_clean) +
  geom_histogram(mapping = aes(x = population), binwidth = 100000)
ggplot(LED_clean) + 
  geom_histogram(mapping = aes(x = population), binwidth = 100000) +
  coord_cartesian(xlim = c(0, 5000000))
LED_clean %>% 
  count(cut_width(population, 5000000)) %>%
  arrange(n)
#understandig how the data is heavily weighted to represent low populations

ggplot(LED_clean) +
  geom_bin2d(mapping = aes(x = under_five_deaths, y = infant_deaths))

# Creating specialized (restricted) datasets ------------------------------

LED2015 <- LED_clean %>%
  filter(year == 2015)

# Comparing developed versus developing leading into economic focus-----------------------------------

qplot(life_expectancy, data = LED_clean, geom = "density",
      color = status, linetype = status)
# can see a clear pattern of higher life expectancy in developed countries (unsurprising)
LED_clean %>%
  count(status == "Developed")
#Far more countries classified as developing which is important in understanding analyses

ggplot(LED_clean, mapping = aes(x = percentage_expenditure, y = life_expectancy)) +
  geom_boxplot()
ggplot(LED_clean, mapping = aes(x = GDP, y = life_expectancy)) +
  geom_boxplot()
gdp_core <- LED_clean %>%
  filter(gdp < 50000)
ggplot(gdp_core, mapping = aes(x = gdp, y = life_expectancy)) + 
  geom_boxplot(mapping = aes(group = cut_width(gdp, 5000)), varwidth = TRUE) 
#using boxplot to see quartiles 
ggplot(LED_clean, mapping = aes(x = GDP, y = percentage_expenditure)) +
  geom_point() +
  geom_smooth()
# as expected there is a pretty linear relationship between GDP and percentage expenditure 
# explains the similarity between the two plots 

ggplot(LED_clean, mapping = aes(x = GDP, y = income_composition_of_resources)) +
  geom_point() +
  geom_smooth()
ggplot(LED_clean, mapping = aes(x = GDP, y = income_composition_of_resources)) +
  geom_point() +
  geom_smooth() +
  coord_cartesian(xlim = c(0, 5000))
#not a significant relationship between GDP and income composition of resources 

ggplot(LED_clean, aes(x = schooling, y = life_expectancy)) +
  geom_point(aes(color = status)) 

ggplot(LED2015, aes(x = schooling, y = life_expectancy)) +
  geom_text(aes(label = country, color = status)) +
  coord_cartesian(xlim = c(15, 20)) +
  coord_cartesian(ylim = c(80, 85))
#interesting outliers of Israel, Canada, and Chile as developing countries with both high levels of schooling and 
# life expectancy 

ggplot(LED2015, aes(x = gdp, y = life_expectancy)) +
  geom_text(aes(label = country, color = status)) +
  coord_cartesian(xlim = c(30000, 70000)) +
  coord_cartesian(ylim = c(80, 90))
#show the same outlying countries as developing but with a high GDP as well--could indicate a mistake in classification
            
ggplot(LED2015, aes(x = gdp, y = life_expectancy)) +
  geom_text(aes(label = country, color = status)) +
  coord_cartesian(ylim = c(70, 90))

# Effect of alcohol -------------------------------------------------------

qplot(alcohol, data = LED_clean, geom = "density",
      color = status, linetype = status)
# AN interesting plot--far higher recorded alcohol consumption in developed countries which may be counterintuitive
# as alcohol generally has health detriments associated. Going to look into it with other metrics

ggplot(LED_clean, mapping = aes(x = alcohol, y = life_expectancy)) +
  geom_point() +
  geom_smooth()

# Does not reveal any meaningful trend when compared to life expectancy so suggests that it's an irrelevant 
# correlation but let's test out some other relationships

mod <- lm(log(alcohol) ~ log(life_expectancy), data = LED_clean)

LED_mod <- LED_clean %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))

ggplot(data = LED_mod) + 
  geom_point(mapping = aes(x = alcohol, y = resid))

View(LED_clean)


# Make codebook -----------------------------------------------------------

makeCodebook(replace = TRUE, LED_clean)
attr(LED_clean$Schooling, "labels") <- "Average number of years of schooling"
attr(LED_clean$income_composition_of_resources, "labels") <- "Human Development Index in terms of income composition of resources (index ranging from 0 to 1)"
attr(LED_clean$GDP, "labels") <- "Gross Domestic Product per capita (in USD)"
attr(LED_clean$Alcohol, "labels") <- "Alcohol, recorded per capita (15+) consumption (in litres of pure alcohol)"



