print("Hello, World!")

"Hello, World!"

1 + 2

1+2

object <- 1 + 2

print(object) #object�� ����϶�

object 

Object #O�� �빮��

ls()

rm(object) #object �����

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

df1[1, ] #ù ���� ����϶�

df1[ , 1] #ù ���� ����϶�

df1[2, 2] #�� ��° ��, �� ��° ���� ����϶�

1 == 1

1 == 2

df1$x == 1

df1[df1$x == 1,]

df1[, df1$x == 2]

class(df1)

batting <- read.csv('kbo_batting_qualified.csv')

getwd()

setwd('��ġ')

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