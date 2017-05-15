Game<-setRefClass('Game',
                  
                  fields=list(
                    # 系统变量
                    name="character", # 名字
                    debug='logical',  # 调试状态
                    width='numeric',  # 矩阵宽
                    height='numeric', # 矩阵高
                    
                    # 应用变量
                    stage='numeric',  # 场景
                    e='environment',  # 环境空间变量
                    m='matrix',       # 数据矩阵
                    isFail='logical'  # 游戏失败
                  ),
                  
                  methods=list(
                    
                    # 构造函数
                    initialize = function(name,width,height,debug) {
                      name<<-"R Game Framework"
                      debug<<-FALSE
                      width<<-height<<-20   #矩阵宽高
                    },
                    
                    # 初始化变量
                    init = function(){
                      e<<-new.env()   #环境空间
                      m<<-matrix(rep(0,width*height),nrow=width)  #数据矩阵
                      isFail<<-FALSE
                    },
                    
                    # 开机画图
                    stage0=function(){
                      stage<<-0
                      init()
                    },
                    
                    # 结束画图
                    stage2=function(){
                      stage<<-2
                    },
                    
                    # 游戏中
                    stage1=function(default=FALSE){
                      stage<<-1
                      if(FALSE){  # 默认游戏中界面
                        plot(0,0,xlim=c(0,1),ylim=c(0,1),type='n',xaxs="i", yaxs="i")
                        text(0.5,0.7,label="Playing",cex=5)  
                      }
                    },
                    
                    # 矩阵工具
                    index = function(col) {
                      return(which(m==col))
                    },
                    
                    # 失败操作
                    fail=function(msg){
                      print(paste("Game Over",msg))
                      isFail<<-TRUE
                      keydown('q')
                      return(NULL)
                    },
                    
                    # 键盘事件，控制场景切换
                    keydown=function(K){
                      if(stage==0){ #开机画面
                        stage1()
                        return(NULL)
                      }  
                      
                      if(stage==2){ #结束画面
                        if(K=="q") q()
                        else if(K==' ') stage0()  
                        return(NULL)
                      } 
                    },
                    
                    # 启动程序
                    run=function(){
                      par(mai=rep(0,4),oma=rep(0,4))
                      stage0()
                      getGraphicsEvent(prompt="Snake Game",onKeybd=function(K){
                        if(debug) print(paste("keydown",K))  
                        return(keydown(K))
                      })
                    }
                  )                  
)