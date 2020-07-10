rm(list=ls())
df <- data.frame(gender = c("M", "F", NA, "M", "F"),
                 score = c(5, 4, 3, 4, NA))
df
is.na(df)
table(is.na(df))
table(is.na(df$gender))
table(is.na(df$score))
mean(df$score)
sum(df$score)

library(ggplot2)
library(dplyr)

ggplot(data=mpg, aes(x=displ, y=hwy)) + geom_point()

ggplot(data=economics, aes(x=date, y=unemploy)) + geom_line()
install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)
raw_welfare <- read.spss(file="Koweps_hpc10_2015_beta1.sav", to.data.frame=T)
welfare <- raw_welfare
head(welfare)
welfare <- rename(welfare,
                  gender=h10_g3,
                  birth=h10_g4,
                  marriage=h10_g10,
                  religion=h10_g11,
                  income=p1002_8aq1,
                  code_job=h10_eco9,
                  code_region=h10_reg7)

R.Version()

class(welfare$code_region)
table(welfare$code_region)
list_region <- data.frame(code_region = c(1:7),
                          region = c("서울",
                                     "수도권(인천/경기)",
                                     "부산/경남/울산",
                                     "대구/경북",
                                     "대전/충남",
                                     "강원/충북",
                                     "광주/전남/전북/제주도"))
list_region
welfare <- left_join(welfare, list_region)
library(ggplot2)
library(dplyr)
welfare <- left_join(welfare, list_region)
welfare %>% 
  select(code_region, region) %>% 
  head
