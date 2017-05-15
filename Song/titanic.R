# Get Data
url <- "http://stats.math.uni-augsburg.de/Mondrian/Data/Titanic.txt"
data <- read.csv(url,T,sep="\t")

library(ggplot2)
p <- ggplot(data,aes(x=Class,fill=Survived))
p + geom_bar(position="stack")+ coord_flip() + facet_wrap(~Sex)

library(vcd)
plot(1)
mosaic(Survived~ Class+Sex, data = data,shade=T, highlighting_direction = "right")
# Survival
fx <- function(x) length(x[x=="Yes"])
table1 <- data.frame(with(data,aggregate(x=Survived,by=list(Class,Sex,Age),FUN=length)))
table2 <- data.frame(with(data,aggregate(x=Survived,by=list(Class,Sex,Age),FUN=fx)))
table1y<−table2y <- table2x
table1survived<−round(table1survived <- round(table1y /table1$x,digits=2)
                      table1[order(table1$survived,decreasing=T),]
                      # Decision Tree Model
                      library(rpart)
                      formula <- Survived~ Class+Sex+Age
                      fit <- rpart(formula,data)
                      library(maptree)
                      draw.tree(fit)