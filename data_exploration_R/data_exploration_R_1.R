#Data exploration and statistics with R
library (tidyverse)
#conflicts (if not specified, the function from the last loaded package is used)
getwd()
dir()
setwd("~/Documents/Other/AsigmoDS/")
sessionInfo()
lm #to check source code of a function
   #control + click function to open on a tab

# assign a variable 
# <- alt + dash shortcut

# Filter a data frame 
a <- filter(.data=iris, Species=="setosa")

# Comment a block
# ctrl+shift+C

# Data types, dynamic assignment
alphabet<-letters
str(alphabet)

# Missing values
is.nan(0/0)
is.infinite(log(0))
is.na(as.numeric(alphabet))
a<-NULL
is.null(a)

# Vectors, named vectors, matrices, and ways to populate them
vect<-c(first="1",second="2",third="3")
vect[1]
vect["first"]

c(1, "s")
letters[seq( from = 1, to = 10 )]
seq(10,1,-2)
seq(1,100,pi)

set.seed(1)
vector<-sample(letters,100,replace=T)
vector[seq(1,10,2)]

mm<-matrix(1:5,10,5,byrow=T)#matrix is 2D array, to get n-dimensional use array
dim(mm)
mm[1:10,2]

rnorm(10) #gaussian


my_list<-list("A",1,LETTERS[1:10],matrix(1,3,3));my_list
my_list[[1]]
str(my_list)
str(my_list[4])
str(my_list[[4]])
names(my_list)<-c("first","second","vector","matrix")

str(my_list["second"])
my_list$matrix #using a cash, no need to extract with [[]]

#exercises
v<-c(1, "a","b","c")
str(v) #all become strings

f<-paste("fellow ",seq(1,15))

f<-replace(f,f=="fellow 5","best fellow")
f[5]<-"best fellow"
f

matrix(rnorm(9),3,3)
a<-matrix(1,2,3);b<-matrix(1,3,2)

l<-a%*%b

my_list <- list("A",1,LETTERS[1:10],matrix(1,3,3))
my_list[[2]]*10


new_list<-list(my_list,matrix(1,5,5),"Hi, we are done!")
new_list
##

reg <- lm(Petal.Length~Sepal.Length,data=iris)
summary(reg)
#parse named vector (which is the output of many functions)
names(reg)
plot(reg)

library(tidyverse)
m<-dplyr::select(iris,Petal.Length,Sepal.Length)
cl<-kmeans(m,5)
names(cl)
cl$cluster
cl$centers
cl$totss
plot(m,col=cl$cluster)
points(cl$centers, col ="yellow", pch = 1)




