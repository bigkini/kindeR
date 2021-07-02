print("Hello, World!")

"Hello, World!"

1 + 2

1+2

object <- 1 + 2

print(object) #object를 출력하라

object 

Object #O가 대문자

ls()

rm(object) #object 지우기

object

rm(list = ls())

z <- y <- x <- 1 + 2

x

y

z

c(x, y, z)

c(1, 2, 3)

1:10

seq(1, 10, 2)

add_one <- function(x) { x + 1 }

add_one(2)


df1 <- data.frame(x = c(1, 2, 3),
                  y = c(4, 5, 6))

df1

class(df1)

df1$x

df1$z <- c(7, 8, 9)

df1[1, ] #첫 행을 출력하라

df1[ , 1] #첫 열을 출력하라

df1[2, 2] #두 번째 행, 두 번째 열을 출력하라

1 == 1

1 == 2

df1$x == 1

df1[df1$x == 1,]

df1[, df1$x == 2]

class(df1)

batting <- read.csv('kbo_batting_qualified.csv')

getwd()

setwd('위치')

batting <- read.csv(file.choose())

batting <- read.csv('https://github.com/bigkini/kindeR/blob/main/kbo_batting_qualified.csv')

class(batting)

str(batting)

head(batting)

tail(batting, 5)

head(batting$avg, 5)

mean(batting$avg)

sd(batting$avg)

summary(batting)

summary(0:100)

length(ls('package:base'))

?ls

args(rm)