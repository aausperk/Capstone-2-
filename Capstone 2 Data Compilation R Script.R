# installing the R packages needed
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
```{r message=FALSE, error=FALSE}
# importing data
Study_1970_2002 <- read_excel("Study 1970-2002 suicide rate by age group.xlsx")
n15_19 <- read_excel("CDC suicide rate ages 15-19 1999 to 2017.xlsx")
n10_14 <- read_excel("CDC Suicide rate ages 10-14 1999 to 2017.xlsx")
n10_24 <- read_excel("CDC suicide rate ages 10-24 1999 to 2017.xlsx")
n20_24 <- read_excel("CDC Suicide rate ages 20-24 1999 to 2017.xlsx")
Census_compint <- read_excel("computer and internet use 1984-2009.xlsx")
Pew_SM <- read_excel("Pew Social Media Use 2.xlsx")
all_suicides <- read_excel("CDC all age suicide rate 1999 to 2017.xlsx")
cell_own <- read_excel("Cell smart phone ownership.xlsx")

# importing stock market data

Dow <- read_excel("Stock Market Index Dow Data.xlsx")
SP <- read_excel("Stock Market Index SP 500 Data.xlsx")
nasdaq <- read_excel("Stock Market Index nasdaq data.xlsx")
```{r message=FALSE, error=FALSE}
# examining the imported stock market data

head(Dow)
head(S_P)
head(nasdaq)

# keeping only the necessary columns for the stock market data

Dow <- Dow[,c(1,6)]
nasdaq <- nasdaq[,c(1,6)]
SP <- S_P[,c(1,6)]

# examining the new data frames

head(Dow)
head(SP)
head(nasdaq)

# creating a column for the year of the Date column

Dow <- Dow %>% mutate(Year = year(Date))
SP <- SP %>% mutate(Year = year(Date))
nasdaq <- nasdaq %>% mutate(Year = year(Date))
# examining the new market data frames

Dow
SP
nasdaq

# keeping only the year and adj close columns

Dow <- Dow[,c(2,3)]
SP <- SP[,c(2,3)]
nasdaq <- nasdaq[,c(2,3)]

Dow
SP
nasdaq
# making these data frames all numeric

SP$'Adj Close' <- as.numeric(SP$'Adj Close')
Dow$'Adj Close' <- as.numeric(Dow$'Adj Close')
nasdaq$'Adj Close' <- as.numeric(nasdaq$'Adj Close')

SP
Dow
nasdaq
# grouping by year and summarizing the close amounts

Dow1 <- Dow %>% 
  group_by(Year) %>% 
  summarize(Avg_close = mean(`Adj Close`))
Dow1 

SP1 <- SP %>% 
  group_by(Year) %>% 
  summarize(Avg_close = mean(`Adj Close`))
SP1 

nasdaq1 <- nasdaq %>% 
  group_by(Year) %>% 
  summarize(Avg_close = mean(`Adj Close`))
nasdaq1 
# combining markets datasets

colnames(Dow1) <- c("Year", "Dow_close")
colnames(SP1) <- c("Year", "SP_Close")
colnames(nasdaq1) <- c("Year", "nasdaq_Close")

Dow1
SP1
nasdaq1

# joining the data frames to one named "markets"

markets <- inner_join(Dow1, SP1, by = "Year")
markets <- inner_join(markets, nasdaq1, by = "Year")
markets
# creating new column with mean of three market columns

markets1 <- markets %>% mutate(Mkt_close = (Dow_close + SP_Close + nasdaq_Close)/3)
markets1

# selecting just the year and average close in a new dataframe

markets <- markets1[,c(1,5)]
markets

# rounding the summary column

markets$Mkt_close <- round(markets$Mkt_close, 0)
markets

# renaming the second column to "Markets"

colnames(markets) <- c("Year", "Markets")
markets
# combine 1999-2017 15-19 and 20-24 suicide rate data

wide <- full_join(n15_19, n20_24, by = "Year")
colnames(wide) <- c("Year", "15-19_rate", "20-24_rate")
wide

# add in 10-14 rate

wide <- full_join(wide, n10_14, by = "Year")
colnames(wide) <- c("Year", "15-19_rate", "20-24_rate", "10-14_rate")

# add in cell and smart phone ownership

wide <- full_join(wide, cell_own)
wide

# add in 1970-2002 study

wide <- full_join(wide, Study_1970_2002)
wide

# arrange wide by year

wide <- arrange(wide, Year)
wide
# adjusting Pew social media to be usable

Pew_SM
Pew_SM <- Pew_SM[,c(1,6)]
colnames(Pew_SM) <- c("Survey_Date", "Avg_SM_Use")
Pew_SM$Survey_Date <- as_date(Pew_SM$Survey_Date)
class(Pew_SM$Survey_Date)

Pew_SM

Pew_SM <- Pew_SM %>% mutate(Year = year(Survey_Date))
Pew_SM

Pew_SM1 <- Pew_SM[,c(2,3)]
Pew_SM1

# group by year and average rate

Pew_SM2 <- Pew_SM1 %>% group_by(Year) %>%
  summarize(Avg_SM_Use = mean(Avg_SM_Use))

Pew_SM2

# add social media data to the wide dataset 

wide <- full_join(wide, Pew_SM2)
wide
# add in market data

wide <- full_join(wide, markets)
wide

# add in computer and home internet use

colnames(Census_compint) <- c("Year", "Comp_athome", "Internet_athome")

wide <- full_join(wide, Census_compint)
wide
# creating column names

colnames(wide) <- c("Year", "15-19_rate", "20-24_rate", "10-14_rate", "Owns_Cellphone", "Owns_Smartphone", "15-24", "25-44", "45-64", ">65", "Avg_SM_Use", "Markets", "Comp_athome", "Internet_athome")



# export completed wide dataset

write.csv(wide, "wide.csv")
