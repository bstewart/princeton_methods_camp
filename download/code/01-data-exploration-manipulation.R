
## ----message = FALSE, warning = FALSE-------------------------------------------------------------------------------------------------
# install.packages(tidyverse)
library("tidyverse")

## check working directory
getwd()

## read in from where .Rproj file is
addh <- read_csv("data/addhealthfake.csv")


## ----message = FALSE, warning = FALSE-------------------------------------------------------------------------------------------------
# install.packages("here")
library("tidyverse")
library("here")

## usual way
addh <- read_csv("data/addhealthfake.csv")

## using here()
addh <- read_csv(here("data", "addhealthfake.csv"))

## using here() + creating a separate var called "path"
## in case you need to read it in multiple places in your code
path <- here("data", "addhealthfake.csv") 
addh <- read_csv(path)


## -------------------------------------------------------------------------------------------------------------------------------------
class(addh$age)
class(addh$gender)


## -------------------------------------------------------------------------------------------------------------------------------------
class(addh)


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
str(addh)


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
head(addh, n = 5)


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
slice_sample(addh, n = 5) # from dplyr


## -------------------------------------------------------------------------------------------------------------------------------------
table(addh$gender) # number of respondents in each category


## -------------------------------------------------------------------------------------------------------------------------------------
unique(addh$age)
sort(unique(addh$age))
sort(unique(addh$gender))


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
# get first row
addh[1, ]

# get first column, rows 1 through 3 (colon means "through")
addh[1:3, 1]


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
# get everything besides first row
addh[-1, ]


## ----explore-data---------------------------------------------------------------------------------------------------------------------
median(addh$income)
mean(addh$age)


## ----explore-data-2-------------------------------------------------------------------------------------------------------------------
# using dplyr
summarize(addh,
          mean_money = mean(money),
          mean_nocheating = mean(nocheating),
          mean_love = mean(love))


## ----explore-data-3-------------------------------------------------------------------------------------------------------------------
unique(addh$debt)


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
paycold <- select(addh, contains("pay"))
head(paycold, 3)


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
nodebtd <- filter(addh, debt == "nodebt" &
                    income < 20000) 
nrow(nodebtd)



## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
nomissinginc <- filter(addh, !is.na(income)) # only keep obs that are NOT (!) na

nomissinginc <- drop_na(addh, income) # alternate function from tidyr

nrow(nomissinginc)


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
addh %>% 
  filter(money == 10) %>% 
  arrange(paypercent) %>% 
  head(2)


## ---- -------------------------------------------------------------------
addhd <- mutate(addh,
               rateavg = (love + money + nocheating)/3,
               rateavglog = log(rateavg))

# look at the first 3 rows and some columns
addhd %>%
  select(love, money, nocheating, rateavg, rateavglog) %>%
  head(3)


## ---- -------------------------------------------------------------------
# New column, new dataframe
addhnew <- mutate(addh, 
                  loglove = log(love))

# New column, same dataframe
addh <- mutate(addh,
                loglove = log(love))

# Overwrite old column, same dataframe
addh <- mutate(addh,
               love = log(love))
               
# Overwrite old column, new dataframe
addhnew <- mutate(addh,
                  love = log(love))


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
gender_group <- group_by(addh, gender) 
summarise(gender_group,
          meannocheat = mean(nocheating))


## ---- -------------------------------------------------------------------
genderdebt <- group_by(addh, gender, debt)
summarise(genderdebt,
          count = n(),
          percent = n()/nrow(addh),
          distinctlove = n_distinct(love))


## ---- -------------------------------------------------------------------
# let's use one of the built in R datasets, mtcars
head(mtcars, 3)
# what if we want the names to be more informative
mtcars2 <- rename(mtcars, 
                    c("displacement" = "disp", 
                      "milespergal" = "mpg"))
head(mtcars2, 3)


## ---- -------------------------------------------------------------------
arrange(summarise(group_by(addh, gender, debt), 
                       nocheatavg = mean(nocheating),
                       loveavg = mean(love),
                       moneyavg = mean(money)), desc(moneyavg))


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
addh %>%
  group_by(gender, debt) %>%
  summarise(nocheatavg = mean(nocheating),
            loveavg = mean(love),
            moneyavg = mean(money)) %>%
  arrange(desc(moneyavg)) %>% 
  rename(c("No cheating average" = "nocheatavg",
           "Love average" = "loveavg",
           "Money average" = "moneyavg"))

