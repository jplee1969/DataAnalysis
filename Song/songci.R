#用文件选择的方式获取数据
songpoem <- read.csv("F:\\Code\\R\\Song\\SongPoem.csv");

#获取词人以及对每一位词人的作品数
author <- unique(songpoem$Author);
num <- 0;
for (i in 1:length(author)) {
  num[i] <- length(which(songpoem$Author == author[i]));
}

#构造数据，并挑选出作品数大于100的记录
author.poemnum <- data.frame(author = author, num = num);
author.poemnum.max <- author.poemnum[which(author.poemnum$num > 100),];

#绘制柱状图，设置标题、图例和坐标轴，以及在柱状图内绘制相应的作品数
col <- rainbow(length(author.poemnum.max$author));
barplot(as.numeric(author.poemnum.max$num), col = col, ylim = c(0,1600));
title(main = "宋词数大于100的词人及作品数", cex = 2); 
legend("topleft", legend = author.poemnum.max$author, pch = 15, col = col, ncol = 4, cex = 0.8);
axis(side = 1, labels = author.poemnum.max$author, las = 2, cex = 0.4, at = seq(from = 0.7, length.out = length(author.poemnum.max$author), by = 1.2), col.lab = "blue");
text(labels = author.poemnum.max$num, cex = 0.7, x = c(seq(from = 0.7, length.out = length(author.poemnum.max$author), by = 1.2)), y = c(author.poemnum.max$num +20))

#词云
library(RColorBrewer)
library(wordcloud)
mycolors <- brewer.pal(8,"Dark2");
windowsFonts(myFont=windowsFont("华文彩云"));
wordcloud(songpoem$Author,min.freq=3,random.order=FALSE,random.color=FALSE,colors=mycolors,family="myFont")

