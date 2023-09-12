
library(tidyverse)
library(here)

## using here() + creating a separate var called "path"
## in case you need to read it in multiple places in your code
path <- here("data", "addhealthfake.csv") 
addh <- read_csv(path)


## -------------------------------------------------------------------------------------------------------------------------------------
class(addh$age)

addh2 <- addh %>%
  mutate(agechar = as.character(age))

class(addh2$agechar)


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
agevec <- c(18, 21, 23, 20)
agevec
class(agevec)

gendervec <- c("male", "female", "other", "female")
gendervec
class(gendervec)


## -------------------------------------------------------------------------------------------------------------------------------------
# What happened here?
c(18, "20", FALSE)

# How about here?
c(1, 2, 3, TRUE, FALSE)


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
agevec
as.character(agevec)

gendervec
as.numeric(gendervec) # why doesn't this work?


## -------------------------------------------------------------------------------------------------------------------------------------
genderfactorvec <- factor(gendervec, 
                          levels = c("male", "female", "other"))
genderfactorvec
class(genderfactorvec)

as.numeric(genderfactorvec) # what happened here?


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
agevec
gendervec

bind_cols(age = agevec, gender = gendervec)


## ----factors, mysize = TRUE, size = '\\scriptsize'------------------------------------------------------------------------------------
addh2 <- addh %>% # Ctrl-Shift-M
  mutate(gender = factor(gender,
                         levels = c("male", "female"),
                         labels = c("male", "female")))

# Check new variable against old variable
str(addh$gender)
str(addh2$gender)


## ----convert-types, eval=F------------------------------------------------------------------------------------------------------------
## vec1 <- as.character(addh2$gender)
## vec2 <- as.numeric(addh2$gender)
## 
## class(addh2$gender)
## class(vec1)
## class(vec2)


## ----convert-types-2, eval=T----------------------------------------------------------------------------------------------------------
vec1 <- as.character(addh2$gender)
vec2 <- as.numeric(addh2$gender)

head(vec1)
head(vec2)


## ----convert-types-3, eval=T----------------------------------------------------------------------------------------------------------
class(addh2$gender)
class(vec1)
class(vec2)


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
addh2 <- mutate(addh,
                moneymoreimport = ifelse(money > love, 1, 0),
                .before = id) # put this new column *before* the id column - notice the period
                
head(addh2)


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
addh3 <- addh %>%
  mutate(loveormoney = ifelse(love == money,
                              "same",
                              ifelse(love > money,
                                     "lovegreater", "moneygreater")))


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
addh3 <- addh %>%
  mutate(loveormoney = case_when(love == money ~ "same",
                                 love > money ~ "lovegreater",
                                 love < money ~ "moneygreater",
                                 .default = NA))

addh3 %>%
  select(id, love, money, loveormoney) %>%
  head(3)


## ----subset-high-inc------------------------------------------------------------------------------------------------------------------
addh3 <- filter(addh2, inclevel == "high")
nrow(addh3)


## ----sample-one-iteration-------------------------------------------------------------------------------------------------------------
set.seed(08540)
samp <- sample(addh3$money, size = 1000, replace = TRUE)
samp_mean <- mean(samp)


## ----for-loop-scratch-----------------------------------------------------------------------------------------------------------------
for (i in 1:1000) {
  samp <- sample(addh3$money, size = 1000, replace = TRUE)
  samp_mean <- mean(samp)
}


## ----for-loop-final-------------------------------------------------------------------------------------------------------------------
sample_means <- numeric(length = 1000)
for (i in seq_along(sample_means)) {
  samp <- sample(addh3$money, size = 1000, replace = TRUE)
  samp_mean <- mean(samp)
  sample_means[i] <- samp_mean
}

mean(sample_means)


## ----vis-for-loop, eval = F, mysize = TRUE, size = '\\scriptsize'---------------------------------------------------------------------
## # plot it!
## ggplot(as.data.frame(sample_means), aes(x = sample_means)) +
##   geom_density() +
##   geom_vline(xintercept = mean(addh3$money),
##              col = "red", linetype = "dashed") +
##   theme_bw()


## ----vis-for-loop-2, echo = F, mysize = TRUE, size = '\\scriptsize'-------------------------------------------------------------------
# plot it!
ggplot(as.data.frame(sample_means), aes(x = sample_means)) +
  geom_density() +
  geom_vline(xintercept = mean(addh3$money),
             col = "red", linetype = "dashed") +
  theme_bw() 


## -------------------------------------------------------------------------------------------------------------------------------------
rep(1000, 5)

rep("Wave 3", 5)


## -------------------------------------------------------------------------------------------------------------------------------------
##create a sequence of each decade (1900, 1910, 1920...)
decades <- seq(from = 1900, to = 2000, by = 10)
decades


## -------------------------------------------------------------------------------------------------------------------------------------
##create names for decades spanning from 1900 to 2000 by 10
decadenames <- paste(c("decade", "birthday"), 
                     seq(from = 1900, to = 2000, by = 10), 
                     sep = "_") 
decadenames


## -------------------------------------------------------------------------------------------------------------------------------------
##set a seed so we sample same ids each time
set.seed(123)

##create a vector with three randomly sampled id's
sampids <- sample(addh$id, size = 3)
sampids


## ----recoding-2, mysize = TRUE, size = '\\scriptsize'---------------------------------------------------------------------------------
summary(addh$income)

# get the 75th percentile of this group to define "high income"
inc75 <- quantile(addh$income)[4]
quantile(addh$income)[4]
inc75

# make new binary variable for if respondent has high income
addh2 <- addh %>%
  mutate(highinc = ifelse(income > inc75, 1, 0))


## ----grouped-means-inc-buckets, mysize = TRUE, size = '\\scriptsize'------------------------------------------------------------------
inc25 <- quantile(addh$income)[2]

addh2 <- addh %>%
  mutate(inclevel = ifelse(income <= inc25,
                                 "low",
                                 ifelse(income >= inc75,
                                        "high", "medium")))


## ----grouped-means-inc-buckets-case-when, mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------
addh2 <- addh %>%
  mutate(inclevel = case_when(income <= inc25 ~ "low",
                                    income >= inc75 ~ "high", 
                                    .default = "medium"))


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
# initialize empty matrix, good to preallocate space
set.seed(1234)
sampmat <- matrix(NA, nrow = 1000, ncol = 3050)
# iterate through each row of the matrix
for(i in 2:nrow(sampmat)){
  # and fill it with a sample of size 10 from the data
  draws <- sample(addh$money, size = 3050, replace= TRUE)
  # note that because each i-th sample is filling a row,
  # we add that sample to the matrix by indexing the i-th row
  sampmat[i, ] <- draws
}

# this is basically bootstrapping!
# check to make sure the for loop properly populated the matrix
sampmat[1:2, 1:10] 
# find mean of each 1000 samples
samplemeans <- rowMeans(sampmat)


## ---- mysize = TRUE, size = '\\scriptsize', fig.height = 2, fig.width = 2-------------------------------------------------------------
# plot distribution of mean ratings
# adding a vertical line for observed mean
ggplot(as.data.frame(samplemeans), aes(x = samplemeans)) +
  geom_density() +
  geom_vline(xintercept = mean(addh$money),
             col = "red", linetype = "dashed") +
  theme_bw() 

