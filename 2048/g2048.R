source(file="./game.r") #加载游戏框架

# G2048类，继承Game类
G2048<-setRefClass("G2048",contains="Game",
                   
                   methods=list(
                     
                     # 构造函数
                     initialize = function(name,debug) {
                       callSuper(name,debug) # 调父类
                       
                       name<<-"2048 Game"
                       width<<-height<<-4
                     },
                     
                     # 初始化变量
                     init = function(){
                       callSuper()  # 调父类
                       
                       e$max<<-4 # 最大数字
                       e$step<<-1/width #步长
                       e$dir<<-'up'
                       e$colors<<-rainbow(14) #颜色
                       e$stop<<-FALSE #不满足移动条件
                       
                       create()
                     },
                     
                     # 随机产生一个新数字
                     create=function(){
                       if(length(index(0))>0 & !e$stop){
                         e$stop<<-TRUE         
                         one<-sample(c(2,4),1)
                         idx<-ifelse(length(index(0))==1,index(0),sample(index(0),1))
                         m[idx]<<-one
                       }      
                     },
                     
                     #失败条件
                     lose=function(){
                       
                       # 判断是否有相邻的有重复值
                       near<-function(x){
                         length(which(diff(x)==0))
                       }
                       
                       # 无空格子
                       if(length(index(0))==0){
                         h<-apply(m,1,near)  # 水平方向
                         v<-apply(m,2,near) # 垂直方向
                         
                         if(length(which(h>0))==0 & length(which(v>0))==0){
                           fail("No free grid.")
                           return(NULL)
                         }
                       }
                     },
                     
                     # 方向移动
                     move=function(){
                       
                       # 方向移动函数
                       moveFun=function(x){
                         if(e$dir %in% c('right','down')) x<-rev(x)
                         
                         len0<-length(which(x==0)) # 0长度
                         x1<-x[which(x>0)] #去掉0
                         pos1<-which(diff(x1)==0) # 找到挨着相等的元素的位置
                         
                         if(length(pos1)==3){ #3个索引
                           pos1<-pos1[c(1,3)]
                         }else if(length(pos1)==2 && diff(pos1)==1){ #2个索引
                           pos1<-pos1[1]
                         }
                         
                         x1[pos1]<-x1[pos1]*2
                         x1[pos1+1]<-0
                         
                         x1<-x1[which(x1>0)] #去掉0
                         x1<-c(x1,rep(0,4))[1:4] #补0，取4个
                         
                         if(e$dir %in% c('right','down')) x1<-rev(x1)
                         return(x1)
                       }
                       
                       last_m<-m
                       if(e$dir=='left')  m<<-t(apply(m,1,moveFun))
                       if(e$dir=='right') m<<-t(apply(m,1,moveFun))
                       if(e$dir=='up')    m<<-apply(m,2,moveFun)
                       if(e$dir=='down')  m<<-apply(m,2,moveFun)
                       
                       e$stop<<-ifelse(length(which(m != last_m))==0,TRUE,FALSE)
                     },
                     
                     # 画布背景
                     drawTable=function(){
                       if(isFail) return(NULL)
                       plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
                       abline(h=seq(0,1,e$step),col="gray60") # 水平线
                       abline(v=seq(0,1,e$step),col="gray60") # 垂直线
                     },
                     
                     # 根据矩阵画数据
                     drawMatrix=function(){
                       if(isFail) return(NULL)
                       a<-c(t(m))
                       lab<-c(a[13:16],a[9:12],a[5:8],a[1:4])
                       
                       d<-data.frame(x=rep(seq(0,0.95,e$step),width),y=rep(seq(0,0.95,e$step),each=height),lab=lab)
                       df<-d[which(d$lab>0),]
                       points(df$x+e$step/2,df$y+e$step/2,col=e$colors[log(df$lab,2)],pch=15,cex=23)
                       text(df$x+e$step/2,df$y+e$step/2,label=df$lab,cex=2)
                     },
                     
                     # 游戏场景
                     stage1=function(){
                       callSuper()
                       
                       move()
                       lose()
                       create()
                       
                       drawTable()
                       drawMatrix()  
                     },
                     
                     # 开机画图
                     stage0=function(){
                       callSuper()
                       plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
                       text(0.5,0.7,label=name,cex=5)
                       text(0.5,0.4,label="Any keyboard to start",cex=2,col=4)
                       text(0.5,0.3,label="Up,Down,Left,Rigth to control direction",cex=2,col=2)
                       text(0.2,0.05,label="Author:DanZhang",cex=1)
                       text(0.5,0.05,label="http://blog.fens.me",cex=1)
                     },
                     
                     # 结束画图
                     stage2=function(){
                       callSuper()
                       info<-paste("Congratulations! You have max number",max(m),"!")
                       print(info)
                       
                       plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
                       text(0.5,0.7,label="Game Over",cex=5)
                       text(0.5,0.4,label="Space to restart, q to quit.",cex=2,col=4)
                       text(0.5,0.3,label=info,cex=2,col=2)
                       text(0.2,0.05,label="Author:DanZhang",cex=1)
                       text(0.5,0.05,label="http://blog.fens.me",cex=1)
                     },
                     
                     # 键盘事件，控制场景切换
                     keydown=function(K){
                       callSuper(K)
                       
                       if(stage==1){ #游戏中
                         if(K == "q") stage2()
                         else {
                           if(tolower(K) %in% c("up","down","left","right")){
                             e$dir<<-tolower(K)
                             stage1()  
                           }
                         }
                         return(NULL)
                       }
                       return(NULL)
                     }
                     
                   )
)

# 封装启动函数
g2048<-function(){
  game<-G2048$new()
  game$initFields(debug=TRUE)
  game$run()
}

# 启动游戏
#g2048()
