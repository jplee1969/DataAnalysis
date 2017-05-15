getcontent <- function(s,g){
  substring(s,g,g+attr(g,'match.length')-1)
}
# getcontent(word[1],gregout[[1]])


# 获取网页原代码，以行的形式存放在web变量中(每页显示25条记录，共10页)
url<-'http://movie.douban.com/top250?format=text'
web <- readLines(url,encoding="UTF-8")
for(i in 1:9){
  url1<-paste('http://movie.douban.com/top250?start=',25*i,'&filter=&type=',sep="")
  web1 <- readLines(url1,encoding="UTF-8")
  web<-c(web,web1)  
}


# 找到包含电影名称的行编号
name <- web[grep(' <div class="hd">',web)+2]
# 用正则表达式来提取电影名
gregout <- gregexpr('>\\w+',name)
movie.names = 0
for(i in 1:length(gregout)){
  movie.names[i]<-getcontent(name[i],gregout[[i]])
}
movie.names <- sub('>','',movie.names)


# 找到包含电影发行年份的行编号并进行提取
# year <- web[grep('<br>',web)+1]
# movie.year <- substr(year,28,32)
year <- web[grep('<p class="">',web)+2]
movie.year <- substr(year,28,32)


# 找到包含电影评分的行编号并进行提取
score <- web[grep('<div class="star">',web)+2]
movie.score <- substr(score,79,81)

# 找到包含电影评价数量的行编号并进行提取
rating <- web[grep('<span property="v:best" content="10.0">',web)+1]
movie.rating <- NULL
for(i in 1:length(rating)){
  rating1<-regexpr('<span>',rating[i])[1] + 6
  rating2<-regexpr('人',rating[i])[1] - 1
  movie.rating[i] <- substr(rating[i],rating1,rating2)
}

# 合成为数据框
movie <- data.frame(names=movie.names,year=as.numeric(movie.year),
                    score=as.numeric(movie.score),rate=as.numeric(movie.rating))
print(head(movie))
# 绘散点图
library(ggplot2)
p <- ggplot(data=movie,aes(x=year,y=score))
p+geom_point(aes(size=rate),colour='blue',
             position="jitter",alpha=0.8)

# 绘制词云
movie["freqRate"] = movie$rate/sum(movie$rate) #添加评价人数的占比列
cloudmovie=movie[,c("names","freqRate")] # 取出电影名称和评价比两列作为词云的数据
wordcloud2(cloudmovie, size = 1, shape='star',color = 'random-dark',
                      backgroundColor = "white",fontFamily = "微软雅黑")