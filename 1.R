install.packages(c('ggplot2', 'gganimate', 'transformr', 'gifski'))
library(ggplot2)
library(gganimate)
# source will evaluate very single line from internet, dont use it, it will create random variables
source('http://bit.ly/CEU-R-shoes')
ls()
str(students)
plot(students$shoe, students$math)
abline(lm(math~shoe, students), col='red')
install.packages('GGally')
library(GGally)
plot(students)

ggpairs(students)
rm(list=ls())

# read the file, not download
readLines('http://bit.ly/CEU-R-shoes')


## city distance

download.file("https://bit.ly/hun-cities-distance", 'cities.xls')
library(readxl)
library(GGally)
cities <- read_excel('cities.xls')

cities <- cities[, -1]
cities <- cities[-nrow(cities), ]
plot(cities)
ggpairs(cities)

# use mds to make the 40 dimentional to 2 dimentional

mds <- data.frame(cmdscale(as.dist(cities)))
mds[, 1] <- -1* mds[, 1]
plot(mds)
text(mds[, 1], mds[, 2], names(cities))

# TODO redo with ggplot

mds$city <- names(cities)
ggplot(mds, aes(X1, X2, label= city)) +
  geom_text()

library(ggrepel)
ggplot(mds, aes(X1, X2, label= city)) + geom_text_repel()

?eurodist


str(eurodist)

mds <- data.frame(cmdscale(eurodist))
mds$city <- row.names(mds)
ggplot(mds, aes(X1, -X2, label= city)) + geom_text_repel()


# mtcars

str(mtcars)

mds <- data.frame(cmdscale(dist(mtcars)))
mds$car <- row.names(mds)
ggplot(mds, aes(X1, X2, label= car)) + geom_text_repel()

dist(mtcars)

























