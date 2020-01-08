# installing the required packages

if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")

# importing the wide dataset and creating the tidy dataset

wide <- read_excel("wide_1-7-20.xlsx", col_types = "numeric")
wide <- round(wide,2)
tidy <- gather(wide, "Category", "Rate", -1, na.rm = TRUE)
tidy$Category <- as.character(tidy$Category)
tidy$Rate <- as.numeric(tidy$Rate)
str(tidy)
str(wide)

# the tidy dataset

head(tidy, n = 10)
colnames(tidy)
dim(tidy)

# the wide dataset

head(wide, n = 10)
colnames(wide)
dim(wide)
# Plotting suicide rates to begin analysis

a_plot <- tidy %>% 
  filter(Category %in% c("15-19_rate", "20-24_rate", "15-24", "25-44", "45-64", ">65")) %>%
  ggplot(aes(x = Year, y = Rate, color = Category)) +
  geom_line(size = 1) + 
  ggtitle("Suicide Rates per 100,000 from 1970-2017") + 
  xlim(1970,2018)
a_plot
# plotting just the 10-14 rate

a_1plot <- tidy %>% 
  filter(Category %in% c("10-14_rate")) %>%
  ggplot(., aes(x = Year, y = Rate, color = Category)) +
  geom_line(size = 1) + 
  ggtitle("Age 10-14 suicide rates from 1999 to 2017") + 
  xlim(1999,2018)
a_1plot

# plotting the 10-14, 15-19, and 20-24 suicide rates post 1999

a_2plot <- tidy %>% 
  filter(Category %in% c("10-14_rate", "15-19_rate", "20-24_rate")) %>%
  ggplot(., aes(x = Year, y = Rate, color = Category)) +
  geom_line(size = 1) + 
  ggtitle("Age 10-14 suicide rates from 1999 to 2017") + 
  xlim(1999,2018)
a_2plot

# plotting usage rates for all electronic media types

b_plot <- tidy %>% 
  filter(Category %in% c("Owns_Cellphone", "Owns_Smartphone", "Avg_SM_Use", "Comp_athome", "Internet_athome")) %>%
  ggplot(aes(x = Year, y = Rate, color = Category)) +
  geom_line(size = 1) +
  ggtitle("Usage Rates for PC to Smartphone Era") +
  xlim(1970, 2018)
b_plot

# arranging plots for comparison

grid.arrange(a_plot,b_plot)

# combining the 2 15-24 vectors to one

ratepost99 <- (wide$`15-19_rate` + wide$`20-24_rate`)/2
wide <- cbind(wide, ratepost99)

a <- wide$`15-24`
a
b <- wide$`ratepost99`
b
c <- data.frame(a,b)
c[['d']] <- rowMeans(c,na.rm=TRUE)
c
c <- as.tibble(c[,3])
colnames(c) <- c("All_15_24_Rates")
wide <- cbind(wide, c)
dim(wide)
wide$All_15_24_Rates

# adding the new column to the tidy dataset

tidy <- gather(wide, "Category","Rate", -1, na.rm=TRUE)
colnames(wide)

# plotting the new column with the 2 original 15-24 columns

d_plot <- tidy %>% 
  filter(Category %in% c("15-24", "ratepost99", "All_15_24_Rates")) %>%
  ggplot(aes(x = Year, y = Rate, color = Category)) +
  geom_line(size = 1) +
  ggtitle("Ages 15-24 Suicide Rates 1970-2017") +
  xlim(1970, 2018)
d_plot

# calculating correlation for home computer ownership and 15-24 suicide rate

cor(wide$All_15_24_Rates,wide$Comp_athome, use = "pairwise.complete.obs")

# plotting just 15-24 S.R. 

e_plot <- tidy %>% 
  filter(Category %in% c("All_15_24_Rates")) %>%
  ggplot(aes(x = Year, y = Rate, color = Category)) +
  geom_line(size = 1) + 
  ggtitle("15-24 Age Group Suicide Rate") + 
  xlim(1980,2010)
e_plot

# plotting just computer ownership

f_plot <- tidy %>% 
  filter(Category %in% c("Comp_athome")) %>%
  ggplot(aes(x = Year, y = Rate, color = Category)) +
  geom_line(size = 1) + 
  ggtitle("Computer Ownership Rate in U.S. Households")
f_plot

# arranging plots for comparison

grid.arrange(e_plot, f_plot)

# computing linear regression statistics for these variables

fit1 <- wide %>% lm(All_15_24_Rates ~ Comp_athome, data = .)
summary(fit1)

# calculating correlation for household internet access rate and 15-24 suicide rate

cor(wide$All_15_24_Rates,wide$Internet_athome, use = "pairwise.complete.obs")

# plotting just 15-24 S.R. 

g_plot <- tidy %>% 
  filter(Category %in% c("All_15_24_Rates")) %>%
  ggplot(aes(x = Year, y = Rate, color = Category)) +
  geom_line(size = 1) + 
  ggtitle("15-24 Age Group Suicide Rate") + 
  xlim(1995,2010)
g_plot

# plotting just household internet access rate

h_plot <- tidy %>% 
  filter(Category %in% c("Internet_athome")) %>%
  ggplot(aes(x = Year, y = Rate, color = Category)) +
  geom_line(size = 1) + 
  ggtitle("Internet Access Rate in U.S. Households") +
  xlim(1995,2010)
h_plot

# arranging plots for comparison

grid.arrange(g_plot, h_plot)

# computing linear regression statistics for these variables

fit2 <- wide %>% lm(All_15_24_Rates ~ Internet_athome, data = .)
summary(fit2)

# calculating correlation for cell phone ownership and 15-24 suicide rate

cor(wide$`All_15_24_Rates`,wide$Owns_Cellphone, use = "pairwise.complete.obs")

# plotting just 15-24 S.R. 

i_plot <- tidy %>% 
  filter(Category %in% c("All_15_24_Rates")) %>%
  ggplot(aes(x = Year, y = Rate, color = Category)) +
  geom_line(size = 1) + 
  ggtitle("15-24 Age Group Suicide Rate") + 
  xlim(2000,2020)
i_plot

# plotting just adult average cell phone ownership

j_plot <- tidy %>% 
  filter(Category %in% c("Owns_Cellphone")) %>%
  ggplot(aes(x = Year, y = Rate, color = Category)) +
  geom_line(size = 1) + 
  ggtitle("Average U.S. Cell Phone Ownership") +
  xlim(2000,2020)
j_plot

# arranging plots for comparison

grid.arrange(i_plot, j_plot)

# computing linear regression statistics for these variables

fit3 <- wide %>% lm(All_15_24_Rates ~ Owns_Cellphone, data = .)
summary(fit3)

# calculating correlation for smart phone ownership and 15-24 suicide rate

cor(wide$All_15_24_Rates,wide$Owns_Smartphone, use = "pairwise.complete.obs")

# plotting just 15-24 S.R. 

k_plot <- tidy %>% 
  filter(Category %in% c("All_15_24_Rates")) %>%
  ggplot(aes(x = Year, y = Rate, color = Category)) +
  geom_line(size = 1) + 
  ggtitle("15-24 Age Group Suicide Rate") + 
  xlim(2010,2017)
k_plot

# plotting just adult average smart phone ownership

l_plot <- tidy %>% 
  filter(Category %in% c("Owns_Smartphone")) %>%
  ggplot(aes(x = Year, y = Rate, color = Category)) +
  geom_line(size = 1) + 
  ggtitle("Average U.S. Smart Phone Ownership") +
  xlim(2010,2017)
l_plot

# arranging plots for comparison

grid.arrange(k_plot, l_plot)

# computing linear regression statistics for these variables

fit4 <- wide %>% lm(`All_15_24_Rates` ~ Owns_Smartphone, data = .)
summary(fit4)

# calculating correlation for social media use and 15-24 suicide rate

cor(wide$All_15_24_Rates,wide$Avg_SM_Use, use = "pairwise.complete.obs")

# plotting just 15-24 S.R. 

m_plot <- tidy %>% 
  filter(Category %in% c("All_15_24_Rates")) %>%
  ggplot(aes(x = Year, y = Rate, color = Category)) +
  geom_line(size = 1) + 
  ggtitle("15-24 Age Group Suicide Rate") + 
  xlim(2005,2020)
m_plot

# plotting just social media use

n_plot <- tidy %>% 
  filter(Category %in% c("Avg_SM_Use")) %>%
  ggplot(aes(x = Year, y = Rate, color = Category)) +
  geom_line(size = 1) + 
  ggtitle("Average Use of >= 1 Social Media Site") +
  xlim(2005,2020)
n_plot

# arranging plots for comparison

grid.arrange(m_plot, n_plot)

# computing linear regression statistics for these variables

fit5 <- wide %>% lm(All_15_24_Rates ~ Avg_SM_Use, data = .)
summary(fit5)

# calculating correlation for economic variation and 15-24 suicide rate

wide2 <- wide %>% filter(Year >= 1985)
cor(wide2$All_15_24_Rates,wide2$Markets, use = "pairwise.complete.obs")

# plotting just 15-24 S.R. 

tidy2 <- gather(wide2, Category, Rate, -1, na.rm = TRUE)

q_plot <- tidy2 %>% 
  filter(Category %in% c("All_15_24_Rates")) %>%
  ggplot(aes(x = Year, y = Rate, color = Category)) +
  geom_line(size = 1) + 
  ggtitle("15-24 Age Group Suicide Rate") + 
  xlim(1985,2020)
q_plot

# plotting just market variation

r_plot <- tidy2 %>% 
  filter(Category %in% c("Markets")) %>%
  ggplot(aes(x = Year, y = Rate, color = Category)) +
  geom_line(size = 1) + 
  ggtitle("Average Annual Market Close") +
  xlim(1985,2020)
r_plot

# arranging plots for comparison

grid.arrange(q_plot, r_plot)

# computing linear regression statistics for these variables

fit6 <- wide2 %>% lm(All_15_24_Rates ~ Markets, data = .)
summary(fit6)

# running linear regression analysis for the two predictor variables

fit1 <- wide2 %>% filter(Year >= 2005) %>% lm(All_15_24_Rates ~ Owns_Cellphone + Markets, data = .)
summary(fit1)

# Create data frame with just the columns needed

data_glm <- wide[,c(1,12,16)] %>% drop_na()

dim(data_glm)

# Create data partitions with 75% of data as train set and 25% of data as test set

set.seed(1)
test_index <- createDataPartition(data_glm$Year, times = 1, p = .75, list = FALSE)
train_set <- data_glm[test_index,]
test_set <- data_glm[-test_index,]
train_set
test_set

# run GLM machine learning algorithm

glm_fit <- train_set %>%
  glm(All_15_24_Rates ~ Markets, data = ., family = "gaussian")
glm_fit
y_hat <- predict(glm_fit, newdata = test_set, type = "response")
y_hat

# round y hat to nearest integer

y_hat <- round(y_hat, 0)
y_hat

# round the test set to the nearest integer

actual_rates <- round(test_set$All_15_24_Rates, 0)
actual_rates

# calculate the number predicted accurately by the GLM model

mean(y_hat == actual_rates)