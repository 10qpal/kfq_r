ls()
rm(list=ls())

english <- c(90, 80, 60, 70)
math <- c(50, 60, 100, 20)
name <- c("김지훈", "이유진", "박동현", "김민지")
df_midterm <- data.frame(name, english, math)
df_midterm
class <- c(1, 1, 2, 2)
df_midterm <- data.frame(name, class, english, math)
df_midterm
mean(df_midterm$english)
mean(df_midterm$math)
df_sales = data.frame(제품 = c("사과", "딸기", "수박"),
                        가격 = c(1800, 1500, 3000),
                        판매량 = c(24, 38, 13))
df_sales
install.packages("readxl")
library(readxl)
df_exam <- read_excel("excel_exam.xlsx")
df_exam

df_test <- read_excel("선별진료소목록.xlsx")
df_test

df_csv_exam <- read.csv("csv_exam.csv")
df_csv_exam

write.csv(df_midterm, file="df_midterm.csv")

yangsan <- read.csv("경상남도 양산시_읍면동별강수량_20171128.csv")
yangsan

exam <- read.csv("csv_exam.csv")
head(exam)
View(exam)
dim(exam)
str(exam)
summary(exam)
library(ggplot2)
mpg <- as.data.frame(ggplot2::mpg)
summary(mpg)
mean(mpg$hwy)

install.packages("dplyr")
library(dplyr)
df_mpg <- rename(mpg, highway = hwy, city = cty)
df_mpg
head(df_mpg)

df <- data.frame(var1 = c(4, 3, 8),
                 var2 = c(2, 6, 1))
df
df$var_sum <- df$var1 + df$var2
df$var_avg <- df$var_sum/2
df

x <- c(1, 2, 3)
class(x)
mode(x)
y <- c("a", "b")
class(y)
mode(y)
is.data.frame(df)
length(x)
names(x) <- c("kim", "lee", "park")
x
x[2]
x[-2]
x
x1 <- x[-2]
x1
x[c(1,3)]

mx <- matrix(c(1, 2, "3", 4, 5, 6), ncol = 2)
mx2 <- matrix(c(1, 2, "3", 4, 5, 6), ncol = 2, byrow = T)
mx
mx2
class(mx)
mode(mx)

list <- list(name="kim", age=c(23, 21))
list
list[2]
list$age
list[[2]]
list[[2]][2]

ls()
rm(list=ls())
library(ggplot2)
mpg <- as.data.frame(ggplot2::mpg)
mpg
mpg$total = (mpg$cty+mpg$hwy)/2
summary(mpg$total)
hist(mpg$total)
ifelse(mpg$total>=20, "pass", "fail")
mpg$test <- ifelse(mpg$total>=20, "pass", "fail")
table(mpg$test)
qplot(mpg$test)
mpg$grade <- ifelse(mpg$total>=30, "A",
                    ifelse(mpg$total>=25, "B",
                           ifelse(mpg$total>=20, "C", "D")))
table(mpg$grade)
qplot(mpg$grade)

library(ggplot2)
midwest <- as.data.frame(ggplot2::midwest)
head(midwest)
tail(midwest)
View(midwest)
dim(midwest)
str(midwest)
summary(midwest)

library(dplyr)
midwest <- rename(midwest, total=poptotal)
midwest <- rename(midwest, asian=popasian)

midwest$asianpertotal <- (midwest$asian/midwest$total)*100
hist(midwest$asianpertotal)

midwest$proportion <- ifelse(midwest$asianpertotal>mean(midwest$asianpertotal), "large", "small")

table(midwest$proportion)
qplot(midwest$proportion)

library(dplyr)
exam <- read.csv("csv_exam.csv")
class(exam)
mode(exam)
?class
?mode
exam %>% filter(class != 1) %>% select(math)
ex_filter <- exam %>% filter(class %in% c(1,3,5))
class(ex_filter)
mode(ex_filter)

mpg <- as.data.frame(ggplot2::mpg)
mpg1 <- mpg %>% filter(mpg$displ<=4)
mpg2 <- mpg %>% filter(mpg$displ>=5)
mean(mpg1$hwy)
mean(mpg2$hwy)

mpg3 <- mpg %>% filter(mpg$manufacturer=="audi")
mpg4 <- mpg %>% filter(mpg$manufacturer=="toyota")
mean(mpg3$cty)
mean(mpg4$cty)

mpg5 <- mpg %>% filter(mpg$manufacturer %in% c("chevrolet", "ford", "honda"))
mean(mpg5$hwy)

library(ggplot2)
exam <- read.csv("csv_exam.csv")
exam %>% select(-math)
exam %>% filter(class == 1) %>% select(math)
exam %>% select(id, math) %>% 
  head
mpg <- as.data.frame(ggplot2::mpg)
mpg1 <- mpg %>% select(class, cty)
head(mpg1)
mpg2 <- mpg %>% filter(class=="suv") %>% select(cty)
mean(mpg2$cty)
mpg3 <- mpg %>% filter(class=="compact") %>% select(cty)
mean(mpg3$cty)

exam %>% arrange(math)
exam %>% arrange(desc(math))

mpg %>% filter(manufacturer=="audi") %>% arrange(desc(hwy)) %>% head(5)

exam %>% mutate(total = math + english + science) %>% head
exam
exam %>% mutate(total = math + english + science) %>%
  arrange(total) %>% 
  head
exam %>% mutate(test=ifelse(science>=60,"pass","fail")) %>% 
  select(class,science,test) %>% 
  filter(test=="pass") %>% 
  arrange(desc(science)) %>% 
  head(3)

mpg_copy <- mpg
mpg_copy <- mpg %>% mutate(hap = cty + hwy)
mpg_copy <- mpg_copy %>% mutate(avg = hap / 2)
mpg_copy %>% arrange(desc(avg)) %>% head(3)
mpg %>% mutate(hap=cty+hwy,avg=hap/2) %>%
  arrange(desc(avg)) %>%
  head(3)

exam %>%
  group_by(class) %>%
  summarise(mean_math=mean(math),
            sum_math=sum(math),
            median_math=median(math),
            n_math=n())
mpg %>% 
  group_by(manufacturer, drv) %>% 
  summarise(mean_cty = mean(cty)) %>%
  head(10)
mpg %>% 
  group_by(manufacturer) %>% 
  filter(class == "suv") %>% 
  mutate(avg = (cty + hwy) / 2) %>%
  summarise(manuracturer_avg = mean(avg)) %>% 
  arrange(desc(manuracturer_avg)) %>% 
  head(5)
  
mpg %>%
  group_by(class) %>% 
  summarise(mean_cty = mean(cty))
mpg %>%
  group_by(class) %>% 
  summarise(mean_cty = mean(cty)) %>% 
  arrange(desc(mean_cty))
mpg %>% 
  group_by(manufacturer) %>% 
  summarise(mean_hwy = mean(hwy)) %>% 
  arrange(desc(mean_hwy)) %>% 
  head(3)
mpg %>% 
  group_by(manufacturer) %>% 
  filter(class == "compact") %>% 
  summarise(n_compact = n()) %>% 
  arrange(desc(n_compact))

test1 <- data.frame(id = c(1, 2, 3, 4, 5),
                    midterm = c(60, 80, 70, 90, 85)) 
test2 <- data.frame(id = c(1, 2, 3, 4, 5, 6),
                    final = c(70, 83, 65, 95, 80, 10))
test1
test2
total <-left_join(test1, test2, by="id")
total
total <-left_join(test2, test1, by="id")
total
name <- data.frame(class=c(1,2,3,4,5),
                   teacher=c("kim","lee","park","choi","jung"))
name
exam_new <- left_join(exam, name, by="class")
exam_new
exam_new <- left_join(name, exam, by="class")
exam_new

group_a <- data.frame(id=c(1,2,3,4,5),
                      test=c(60,80,70,90,85))
group_b <- data.frame(id=c(6,7,8,9,10),
                      test=c(70,83,65,95,80))
group_all <- bind_rows(group_a, group_b)
group_all

midwest <- as.data.frame(ggplot2::midwest)
midwest <- midwest %>% mutate(midwest, ratio_child=(poptotal-popadults)/poptotal*100)
midwest %>% 
  arrange(desc(ratio_child)) %>% 
  select(county,ratio_child) %>% 
  head(5)
midwest <- midwest %>% mutate(midwest, grade=ifelse(ratio_child>=40, "large", ifelse(ratio_child>=30, "middle", "small")))
table(midwest$grade)
midwest %>% 
  mutate(ratio_asian=(popasian/poptotal)*100) %>% 
  arrange(ratio_asian) %>% 
  select(state, county, ratio_asian) %>% 
  head(10)
