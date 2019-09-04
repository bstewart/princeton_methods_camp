## ----setup, echo = FALSE, results = "hide"-------------------------------
##allows to knit with errors for pedagogical purposes
knitr::opts_chunk$set(error = TRUE)

## ------------------------------------------------------------------------
# Create a vector filled with random values
# we want to use a loop to get the square of this vector
vector1 <- c(10, 20, 30, 40, 50)

# how to write a loop

vector.sq <- c()
# step 1, workshop the loop with a single data point
#for(i in 1:length(vector1)) {
  # 1st element of `vector11` squared into `1st`-th position of `vector.sq`
  vector.sq[1] <- vector1[1] * vector1[1]
#}

vector.sq

# then try again with another data point, imitating how the loop would cycle through the index
# step 2, workshop the loop with another data point
#for(i in 1:length(vector1)) {
  # 2nd element of `vector11` squared into `2nd' position of `vector.sq`
  vector.sq[2] <- vector1[2] * vector1[2]
#}

vector.sq

# notice how all the number that index where the number is is changing in a consistent fashion in the loop
# this ia a key characteristic of for loops

#putting it together with i in place of actual index

# Initialize results
vector.sq <- c()
for(i in 1:length(vector1)) {
  # i-th element of `vector11` squared into `i`-th position of `vector.sq`
  vector.sq[i] <- vector1[i] * vector1[i]
}

vector.sq

## ---- results = "hide", echo = FALSE, warning = FALSE, message = FALSE----
##set size-related parameters and load ANES data
##load anes data
library(magrittr)
library(dplyr)
library(purrr)
library(tibble)
setwd("~/Dropbox/MethodsCamp/2018/Programming Lectures/Day2Programming")
addh <- read.csv("addhealthlec1.csv")

##size
knitr::knit_hooks$set(mysize = function(before, options, envir) {
  if (before) 
    return(options$size)
})

## ---- mysize = TRUE, size = '\\scriptsize'-------------------------------
##how we would do manually
femdebt <-  length(unique(addh$love[addh$gender == "female" & 
                                      addh$debt == "yesdebt"]))
femdebt
femnodebt <- length(unique(addh$love[addh$gender == "female" &
                                       addh$debt == "nodebt"]))
femnodebt
maledebt <- length(unique(addh$love[addh$gender == "male" & 
                                      addh$debt == "yesdebt"]))
maledebt
malenodebt <- length(unique(addh$love[addh$gender == "male" &
                                        addh$debt == "nodebt"]))
malenodebt

## ---- mysize = TRUE, size = '\\scriptsize'-------------------------------
#focusing on two cases
femdebt <-  length(unique(addh$love[addh$gender == "female" & 
                                      addh$debt == "yesdebt"]))
maledebt <- length(unique(addh$love[addh$gender == "male" & 
                                      addh$debt == "yesdebt"]))

## ---- mysize = TRUE, size = '\\scriptsize'-------------------------------
# Our custom function!
# x here is our placeholder for things we want to function to take in
nunique <- function(x){
  length(unique(x))
  }

## ---- mysize = TRUE, size = '\\scriptsize'-------------------------------
# less efficient way to feed the function the appropriate vectors
nunique(x = addh$love[addh$gender == "female" & addh$debt == "nodebt"])
nunique(x = addh$love[addh$gender == "male" & addh$debt == "nodebt"])
# etc...

## ---- mysize = TRUE, size = '\\scriptsize'-------------------------------
# more efficient way to apply, specify name of function
Tapply.output <- tapply(addh$love, list(addh$gender, addh$debt),
                  mean) 

class(Tapply.output)
# include function directly in command
tapply(addh$love, list(addh$gender, addh$debt),
                  function(x){length(unique(x))}) 
# how we would do this in Tidyverse?
#tidyverse.output <- 
  
addh %>%
  split(list(.$gender, .$debt)) %>%
  map(~nunique(.$love))

# alternatively 
addh %>%
  plyr::dlply(c("gender", "debt")) %>%
  map(~nunique(.$love))

# or my favorite way 
addh %>%
  group_by(gender, debt) %>%
  summarise(unique.love.responses = nunique(love))


## ---- mysize = TRUE, size = '\\tiny'-------------------------------------
set.seed(1234)
sampmat <- matrix(NA, nrow = 15, ncol = 10)
# iterate through each row of the matrix
for(i in 1:nrow(sampmat)){
  # and fill it with a sample of size 10 from the data
  drawof10 <- sample(addh$money, size = 10, replace= FALSE)
  # note that because each i-th sample is filling a row,
  # we add that sample to the matrix by indexing the i-th row
  sampmat[i, ] <- drawof10
}
head(sampmat, 2) #object we want stored
head(drawof10, 2) #useless intermediate objects, could use remove() to clean

#remove(paycol)
## ---- mysize = TRUE, size = '\\scriptsize'-------------------------------
##extract relevant columns
addh2 <- addh[, c("age", "paypercent")]

##find the mean of the columns using apply
apply(addh2, MARGIN = 2, FUN = function(x){mean(x)})

mean(addh2$age)
mean(addh2$paypercent)
##could also subset directly inside the apply
##to do in one step
apply(addh[, c("age", "paypercent")], 2, function(x){mean(x)})

addh %>% 
  summarise(mean.age = mean(age), mean.paypercent = mean(paypercent))

##find the mean of the columns using colmeans
# short built in base R function for using tapply for means in array
colMeans(addh2)

## ---- mysize = TRUE, size = '\\scriptsize'-------------------------------
# create a logged pay percent and log income
loginclogpay <- apply(addh[, c("income", "paypercent")], c(1, 2), log)
# view the output of apply and check its class
head(loginclogpay, 3)
# Do this in Tidyverse
TV.loginclogpay <- addh %>% 
  select(income, paypercent) %>% 
  log 
head(TV.loginclogpay, 3)
# can append to your original dataset by cbind 
addh <- cbind(addh, TV.loginclogpay)

str(addh)
mean(addh$income)

## ---- echo = FALSE-------------------------------------------------------
##restrict data to with numeric variables
# sapply allows easy restriction of functions and result in a 1-d array 
addh$id <- as.character(addh$id)
addhnumeric <- addh[, sapply(addh, is.numeric)]

## ---- mysize = TRUE, size = '\\scriptsize'-------------------------------
##create normalize function
normalizefunc <- function(x){(x - mean(x))/sd(x)}

##apply normalize function to columns of dataframe restricted to
##numeric variables 
addhnormalized <- apply(addhnumeric, 2, normalizefunc)

##check that it worked by manually normalizing the age variable and comparing
addh$normage <- (addh$age - mean(addh$age))/sd(addh$age)
head(addh[, "normage"], 3)
head(addhnormalized[, "age"], 3)


## ---- mysize = TRUE, size = '\\scriptsize'-------------------------------
##could also do in one step
addhnormalized2 <- apply(addhnumeric, 2, function(x){x- mean(x)/sd(x)})
head(addhnormalized2)

# Now let's try in Tidyverse! With Purrr
TV.addhnorm <- addh %>% 
  map_if(is.numeric, ~normalizefunc(.)) %>% 
  as.data.frame 

head(TV.addhnorm[, "age"], 3)

## ---- mysize = TRUE, size = '\\scriptsize'-------------------------------
##use tapply to construct the matrix, index is like group_by
payresults <- tapply(addh$paypercent, 
                     INDEX = list(addh$money, addh$gender), 
                     FUN = mean)
head(payresults, 3)
# dyplr
addh<- addh[, c(-12, -13)] 
addh %>% 
  group_by(money, gender) %>% 
  summarize(pay.mean = mean(paypercent))

## ---- echo = FALSE, warning = FALSE, message = FALSE---------------------
library(tidyr)
library(ggplot2)
##convert to a data.frame to help with plotting
payresultsdf <- as.data.frame(payresults)

##add a variable to pay results indicating that the
##row index corresponds to the rating of money's importance
payresultsdf$moneyrate <- as.numeric(rownames(payresults))

# step one: reshuffle data so that we have a variable indicating gender
# will learn in data cleaning day more about conversion between 
# long and wide form data on day 3 using tidyr
library(tidyr)
payresultslong <- gather(payresultsdf, variable, value, -moneyrate)

## ---- mysize = TRUE, size = '\\tiny', fig.width = 4.5, fig.height = 2.5, echo = FALSE----
##plot (intermediate steps in .rmd file and will be covered more on 
## data cleaning day)
ggplot(payresultslong, aes(x = moneyrate, y = value, 
  group = variable, color = variable)) +
  geom_line() +
  ylim(0, max(payresultslong$value)+5)  +
  theme_bw(base_size = 10) +
  theme(legend.position = c(0.75, 0.25)) +
  xlab("Rating of money's importance") +
  ylab("Mean percentage of dates paid for") +
  labs(color = "")

## ---- mysize = TRUE, size = '\\scriptsize'-------------------------------
# initialize empty vector, though generally preallocate memory if possible
agecoef <- c()

agecoef.good <- rep(NA, 3050)

# alternative vector creation 
# agecoef <- rep(NA, nrow(addh))
# iterate through each observation in the data, 
for(i in 1:nrow(addh)){
  # removing one observation at a time
  dataminus1 <- addh[-i, ]
  # feed this modified data into a regression func
  regminus1 <- lm(money ~ age,
                  data = dataminus1)
  # save the regression output coefficient for age
  agecoefsingle <- coef(regminus1)["age"]
  # append the output coef into our empty vector at the end 
  # agecoef <- c(agecoef, agecoefsingle)
  # alternative way to save loop output
  agecoef.good[i] <- agecoefsingle
}


## ---- mysize = TRUE, size = '\\scriptsize'-------------------------------
# make a function called leaveoneout.moregeneral that takes i, 
# data, formula, coeftoextract, and vectofill parameters
leaveoneout.moregeneral <- function(i, data, formula,
                                    coeftoextract,
                                    vectofill){
  # remove data points 1 at a time, indexed by i 
  dataminus1 <- data[-i, ]
  # run regression with this data 
  regminus1 <- lm(formula = formula,
                  data = dataminus1) 
  coefsingle <- coef(regminus1)[coeftoextract] 
  vectofill <- c(vectofill, coefsingle) 
  return(vectofill)
}

## ---- mysize = TRUE, size = '\\scriptsize'-------------------------------
# define i vector to iterate over
i <- 1:nrow(addh)
# use sapply to apply the function to i, and also feed it the other inputs
agecoefs.func.output <- sapply(i, 
        FUN = leaveoneout.moregeneral, 
      # define data as the data we want to feed it
        data = addh,
      # define what formula we want to feed into lm
       formula = money ~ age, coeftoextract = "age",
      # and store result in a vector;
       vectofill = c()) 


str(agecoefs.func.output)


## ---- echo = FALSE, results = "hide"-------------------------------------
#getting name of factor variable coefficients
lm(money ~ gender, data = addh)
lm(money ~ debt, data = addh)
table(addh$debt) #see it removed first level as reference 
#category and pasted variable name onto second level

## ---- mysize = TRUE, size = '\\scriptsize'-------------------------------
#look at gender controlling for age and debt status
coefsgender <- sapply(i, 
        leaveoneout.moregeneral, 
        data = addh,
       formula = money ~ gender + age + debt, 
       coeftoextract = "gendermale",
       vectofill = c())
head(coefsgender)

#look at debt status controlling for income
coefsdebt <- sapply(i, 
        leaveoneout.moregeneral, 
        data = addh,
       formula = money ~ debt + income, 
       coeftoextract = "debtyesdebt",
       vectofill = c())
head(coefsdebt)

## ---- eval = T, mysize = TRUE, size = '\\scriptsize'---------------------
# to do with the for loop, we would have had to
# rewrite the entire loop each time we changed
# what variables we wanted to look at
for(i in 1:nrow(addh)){
  dataminus1 <- addh[-i, ]
  # involves changing formula and coef to extract inside coefficient
  regminus1 <- lm(money ~ gender,
                  data = dataminus1)
  agecoefsingle <- coef(regminus1)["gendermale"]
  agecoef <- c(agecoef, agecoefsingle)
}



# repeat again
for(i in 1:nrow(addh)){
  dataminus1 <- addh[-i, ]
  regminus1 <- lm(money ~ gender,
                  data = dataminus1)
  agecoefsingle <- coef(regminus1)["debtyesdebt"]
  agecoef <- c(agecoef, agecoefsingle)
}

## ---- echo = FALSE, message = FALSE, warning = FALSE, results = "hide"----
anesdf <- read.csv("anespilot16.csv")

## ---- mysize = TRUE, size= '\\scriptsize'--------------------------------
i <- 1:nrow(anesdf)
freetradecoefs <- sapply(i, leaveoneout.moregeneral, 
        data = anesdf,
       formula = fttrump ~ freetrade, 
       coeftoextract = "freetrade",
       vectofill = c()) 
head(freetradecoefs)

## ---- mysize = TRUE, size = '\\scriptsize', results = "hide"-------------
# example of mapply to use t.tests to compare responses
# about what's important for relationship for two different factor variables
grouping <- rep(c("debt", "gender"), each = 3)
outcome <- c("love", "nocheating", "money")

custom.t <- function(x, y){
  formula <- as.formula(paste(y, x, sep = "~"))
  lm(formula, data = addh)
}


mapply(custom.t, x = grouping, y = outcome)["coefficients",]

## ---- mysize = TRUE, size = '\\scriptsize', echo = FALSE-----------------
##example of mapply to use t.tests to compare responses
##about what's important for relationship between two sets of groups
grouping <- rep(c("debt", "gender"), each = 3)
outcome <- c("love", "nocheating", "money")
custom.t <- function(x, y){
  formula <- as.formula(paste(y, x, sep = "~"))
  summary(lm(formula, data = addh))
}
# if we don't loop with mapply, default is the first elements
custom.t(x=grouping, y=outcome)
mapply(custom.t, x = grouping, y = outcome)

## ---- mysize = TRUE, size = '\\scriptsize'-------------------------------
##bind into a list
listacs <- list(acs1, acs2, acs3, acs4)

##use do.call with the list to bind into a data.frame
acsallyears <- do.call(rbind.data.frame, listacs)
head(acsallyears, 3); tail(acsallyears, 3)
dim(acsallyears)

## ---- mysize = TRUE, size= '\\scriptsize'--------------------------------
# code a function for vector cross products
# Compute generalized cross product by taking the determinant of sub-matricies
xprod <- function(x, y) {
  args <- list(x, y)
  len <- unique(sapply(args, FUN=length))
  m <- do.call(rbind, args)
  
  sapply(seq(len),
         FUN=function(i) {
           det(m[,-i,drop=FALSE]) * (-1)^(i+1)
         })
}
x <- c(2,3, 5)
y <- c(4, 10, 10)
xprod(x, y)

#check that it's done correctly b/c [sum of (cross product * vector) should be 0]
crossprod <- xprod(x, y)
sum(x * crossprod) # should be 0 or close to 0 due to rounding
sum(y * crossprod)

# a quick example for transforming skewed data like income
IHS <- function(x) {
  log(x + (x^2 + 1)^(0.5))}

IHS.income <- IHS(addh$income)
hist(IHS.income)
hist(addh$income)
hist(addh$logincome)

