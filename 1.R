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

mds <- data.frame(cmdscale(dist(scale(mtcars))))
library(ggplot2)
library(ggrepel)
mds$car <- row.names(mds)
ggplot(mds, aes(X1, X2, label= car)) + geom_text_repel()

str(mtcars)

# make sure the dist matrix takes all variables with the same scale
scale(mtcars)

# 
str(UCBAdmissions)
ucb <- as.data.frame(UCBAdmissions)

# admitted and rejected
# position= 'fill' shows the percentage
ggplot(ucb, aes(Gender, Freq, fill=Admit)) + geom_col(position = 'fill')

# add colors
ggplot(ucb, aes(Gender, Freq, fill=Admit)) + geom_col(position = 'fill') + 
  scale_fill_manual(values = c("Admitted"="darkgreen", "Rejected"="darkred"))

ggplot(ucb, aes(Gender, Freq, fill=Admit)) + geom_col(position = 'fill') + 
  scale_fill_manual(values = c("Admitted"="darkgreen", "Rejected"="darkred")) + facet_wrap(~Dept)

# look at numbers rather than frequency
ggplot(ucb, aes(Gender, Freq, fill=Admit)) + geom_col() + 
  scale_fill_manual(values = c("Admitted"="darkgreen", "Rejected"="darkred")) + facet_wrap(~Dept)

# iris
?iris
ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point() +
  geom_smooth(method = lm)

ggplot(iris, aes(Sepal.Length, Sepal.Width)) + geom_point() +
  geom_smooth(method = lm)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width))  +
  scale_fill_manual(values = c("setosa" = "green",
                               "versicolor" = "red",
                               "virginica" = 'blue')) +
  geom_point(aes(color = Species)) + 
  geom_smooth(method = 'lm', color='black')+
  geom_smooth(aes(color = Species), method = 'lm') +
  theme_minimal()


# new topic
library(data.table)
bookings <- fread('http://bit.ly/CEU-R-hotels-2018-prices')
bookings[1:5]

bookings[price<100][holiday==1][1:5]

bookings[price<100 & holiday==1, .N]
bookings[price<100 & holiday==1, mean(price)]
bookings[price<100 & holiday==1, summary(price)]
bookings[price<100 & holiday==1, hist(price)]


# average price

bookings[weekend==1, mean(price)]
bookings[weekend==0, mean(price)]

bookings[, mean(price), by=weekend]

bookings[, price_per_night:= price/nnights]
bookings

# grouping multiple columns
bookings[, .(price=mean(price), min=min(price), max=max(price)), by=.(weekend, nnights, holiday)]


# hotel features
features <- fread('http://bit.ly/CEU-R-hotels-2018-features')

merge(bookings, features, all.x = T)[is.na(city)]

# country level aggregated data on avg rating of hotels

countries <- features[, .(rating=mean(rating, na.rm=T)), by=country][!is.na(country)]
setorder(countries, rating)

countries[order(country)]
countries[order(rating)]

# create a map
library(ggmap)
library(tidygeocoder)
?geocode

countries <- data.table(tidygeocoder::geocode(countries, 'country'))
library(maps)
world <- map_data('world')
str(world)

map <- ggplot() + geom_map(data=world, map = world, aes(long, lat, map_id=region)) +
  theme_void() + coord_fixed(1.3)

map + geom_point(data= countries, aes(long, lat, size=rating), color='orange' )

# get_stamenmap
bbox = c(left = min(countries$long), bottom = min(countries$lat),
         right = max(countries$long), top = max(countries$lat))
get_stamenmap(bbox = bbox, zoom = 4) %>% ggmap() +
  geom_point(aes(x = long, y = lat, size=rating), data = countries, colour = "red") 
+ theme_void() + coord_fixed(1.3)


# anscombe
anscombe
ggplot(data=anscombe, aes(x=x1, y=y1)) + geom_point(color='Orange')

plot(anscombe[,  c(1, 5)])

plot(anscombe[,  c(2, 6)])
cor(anscombe[,  c(2, 6)])
plot(anscombe[,  c(3, 7)])
plot(anscombe[,  c(4, 8)])


mean(anscombe[, c(1)])
lapply(1:4, function(i) mean(anscombe[, c(i)]))
computemean <- function(i) { 
  mean(anscombe[, c(i)])
}

lapply(1:4, computemean)
lapply(1:4, function(i) cor(anscombe[, c(i)], anscombe[, c(i+4)]))
anscombe

# create dataset
anscombe_df <- rbindlist(lapply(1:4, function(i) {
  data.frame(
    x = anscombe[, c(i)],
    y = anscombe[, c(i+4)],
    dataset = i)
}
))

## data.frame with 4x11 rows, 3 columns: x, y, dataset id
ggplot(anscombe_df, aes(x, y)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_wrap(~dataset) +
  theme_bw()


# dinasaur dataset
df <-datasauRus::datasaurus_dozen_wide
df

# or use (1:13)*2
dino_df <- rbindlist(lapply(seq(1, 26, by = 2), function(i) {
  data.frame(
    x = df[, c(i), drop = TRUE], 
    y = df[, c(i+1), drop = TRUE],
    # sub('_x$', '', names(df)[i])
    dataset = substr(names(df)[i], 1, nchar(names(df)[i])-2))
}
))

ggplot(dino_df, aes(x, y)) + geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_wrap(~dataset) +
  theme_bw()








