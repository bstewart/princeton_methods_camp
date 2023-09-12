
library(tidyverse)


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
sleep_wide <- tibble(name = c("Angela", "Varun"), 
                        year1 = c(6,7),
                        year2 = c(6,6),
                        year3 = c(5,5))
sleep_wide


## ---- message = F, warning = F, mysize = TRUE, size = '\\scriptsize'------------------------------------------------------------------
library(tidyverse)
sleep_long <- sleep_wide %>%
  pivot_longer(-name,
               names_to = "year",
               values_to = "avgsleep")
sleep_long


## ---- eval=F--------------------------------------------------------------------------------------------------------------------------
## pivot_longer(cols = c(year1, year2, year3),
##              names_to = year,
##              values_to = avgsleep)


## ---- message = F, warning = F, mysize = TRUE, size = '\\scriptsize'------------------------------------------------------------------
sleep_long <- sleep_wide %>%
  pivot_longer(-name,
               names_to = "year",
               values_to = "avgsleep")
sleep_long


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
sleep_wide2 <- sleep_long %>%
  pivot_wider(names_from = year, values_from = avgsleep)

sleep_wide2


## ---- echo = F, mysize = TRUE, size = '\\scriptsize'----------------------------------------------------------------------------------
sleep_p2 <- tibble(year = c(1,2,3),
                   W1_GH = c(6,6,5),
                   W2_OC = c(7,6,5))
sleep_p2


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
sleep_p2_tidy <- sleep_p2 %>%
  pivot_longer(cols = c(W1_GH, W2_OC), 
               names_to = "office_housing", 
               values_to = "avgsleep")

sleep_p2_tidy


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
sleep_p2_tidy <- sleep_p2 %>%
  pivot_longer(cols = c(W1_GH, W2_OC), 
               names_to = "office_housing", 
               values_to = "avgsleep") %>%
  separate(col = office_housing, into = c("office", "housing"), sep = "_")
  
sleep_p2_tidy


## ---- eval = F------------------------------------------------------------------------------------------------------------------------
## separate(col = office_housing,
##          into = c("office", "housing"),
##          sep = "_")


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
library(stringr)
sleep_pls_unite <- sleep_p2_tidy %>% 
  mutate(building = stringr::str_sub(office, 1, 1), 
         floor = stringr::str_sub(office, -1, -1)) %>%
  select(year, building, floor, housing, avgsleep)

sleep_pls_unite


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
sleep_united <- sleep_pls_unite %>%
  unite(col = "office", building, floor, sep = "")

sleep_united


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
sleep_p3 <- tibble(name = c(rep("Angela",2), rep("Varun", 2)),
                       activity = rep(c("sleep", "exercise"), 2), 
                       year1 = c(6,1,7,2),
                       year2 = c(6, 0.5, 6, 0),
                       year3 = c(5,0,5,0))
sleep_p3


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
sleep_p3_tidy <- sleep_p3 %>%
  pivot_longer(cols = c(year1, year2, year3), names_to = "year", values_to = "avgtime") 

sleep_p3_tidy


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
sleep_p3_tidy <- sleep_p3 %>%
  pivot_longer(cols = c(year1, year2, year3),
               names_to = "year", 
               values_to = "avgtime") %>%
  pivot_wider(names_from = "activity", 
              values_from = "avgtime")

sleep_p3_tidy


## ---- eval = F, mysize = TRUE, size = '\\scriptsize'----------------------------------------------------------------------------------
## #Example: saving csv file to "output" folder in my current working directory
## write_csv(sleep_p3_tidy, "output/sleep_p3_tidy.csv")
## 
## #Example, saving Stata file to my Downloads folder
## library(haven)
## write_dta(sleep_long, "~/Downloads/gss_long.dta")


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
coursework <- tibble(name = c(rep("Angela",4), rep("Varun", 4), rep("Matt",4)),
                       year = rep(c("year1", "year1", "year2", "year2"), 3),
                       semester = rep(c(1,2), 6),                      
                       classes = c(4,4.5,3,3,4,4,4,3,4,3.5,4,3))


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
coursework


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
leftjoin <- left_join(sleep_p3_tidy, coursework, by = c("name", "year"))
leftjoin


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
onlycommon <- inner_join(sleep_p3_tidy, coursework, by = c("name", "year"))

onlycommon


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
keepallobs <- full_join(sleep_p3_tidy, coursework, by = c("name", "year"))

keepallobs


## ---- mysize = TRUE, size = '\\scriptsize'--------------------------------------------------------------------------------------------
keeprightrows <- right_join(sleep_p3_tidy, coursework, by = c("name", "year"))

keeprightrows


## ---- results = 'asis', message = FALSE, warning = FALSE,mysize = TRUE, size = '\\scriptsize'-----------------------------------------
library(stargazer)
#print summary table of age for coursework data 
stargazer((leftjoin %>% select(exercise, sleep)), header = FALSE, title = " Summary table",
          font.size = "tiny")


## ---- results = 'asis', message = FALSE, warning = FALSE,mysize = TRUE, size = '\\scriptsize'-----------------------------------------
#not a sensical regression in this example but used to illustrate
reg <- glm(sleep ~ exercise + classes, data  = leftjoin)
stargazer(reg, header = FALSE, title = " Regression results",
          font.size = "tiny")


## ---- results = 'asis', message = FALSE, warning = FALSE, mysize = TRUE, size = '\\scriptsize', options(xtable.comment = FALSE)-------
library(xtable)
xtable(sleep_long)


## ---- results = 'asis', message = FALSE, warning = FALSE, mysize = TRUE, size = '\\scriptsize'----------------------------------------
library(knitr)
kable(sleep_long)

