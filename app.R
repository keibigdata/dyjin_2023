library(markdown)
setwd("/srv/shiny-server/text")
#devtools::install_github("jcrodriguez1989/shinyParallel", upgrade = "never")
library(gdata)
# Shiny
library(shiny)
library(NLP4kec)
library(readr)
library(KoNLP)
library(arules)
library(igraph)
library(tm)
library(combinat)
library(DT)
library(showtext)
library(extrafont)
library(tidyverse)
library(ggrepel)
library(viridis)
library(writexl)
library(lubridate)
library(vroom)
library(RWeka)
library(ggpubr)
library(readxl)

#loadfonts()
showtext_auto()
font_add("NanumBarunGothic", "NanumBarunGothic.ttf")

s_date <- Sys.Date()
rn <- sample(1:100000000,1)
fn1 <- paste("./result/", s_date,"_",rn,".csv",sep="")

nn_dir <- "/media/Data/Naver_News/result/"
nn <- vroom(paste(nn_dir,"indexing.txt",sep=""),delim=",",col_names=FALSE)
nn <- nn[,c(1,2,5)]

mn_dir <- "/media/Data/Me_Report/result/"
#mn <- vroom(paste(mn_dir,"indexing.txt",sep=""),delim=",",col_names=FALSE)
mn <- read_csv(paste(mn_dir,"indexing.txt",sep=""),col_names = FALSE)
#write.table(mn,"indexing.txt",row.names=FALSE, col.names=FALSE,sep=",")
mn <- mn[,c(1,2,4)]
fn <- paste("./result/output_",rn,".xlsx",sep=" ")
fn2 <- paste("output_",s_date, "_", rn,".xlsx",sep="")


pkg <- '기후,대기,자원순환,환경보건,국토/자연,물,기타'
pkg_kw <- 
  "온실가스,온난화,이산화탄소,탄소,폭설,폭우,홍수,가뭄,혹서,혹한,해수면,열섬,이상고온,집중호우,해일,쓰나미,북극빙하,남극빙하,히말라야빙하,그린란드빙하,사막화,녹색성장,태풍\n
대기오염,대기질,미세먼지,황사,공해,초미세먼지,배기가스,스모그,공기,매연,SOx,NOx,이산화질소,이산화황,질소산화물,황산화물,VOCs,휘발성유기화합물,PM10,PM2.5,대기환경,맑은하늘,산성비,가시거리,오존,다이옥신,대기배출,특정대기유해물질,디젤입자,실내공기,배기가스,소각로,청정연료,벤젠,총량제,분진\n
쓰레기,폐기물,분리수거,재활용,종량제,소각,매립,리사이클,리싸이클,음폐수,무단투기,폐비닐,해양투기,폐자원,폐열,업사이클,폐금속,물질흐름분석,용출시험,자원순환,페전기전자,폐자동차,플라스틱\n
환경호르몬,발암물질,새집증후군,가습기,살균제,유해화학물질,아토피,환경성질환,내분비계,장애물질,중금속,석면,프탈레이트,비스페놀A,화평법,화관법,라돈,고엽제,DDT,포름알데히드,폼알데하이드,잔류성,카드뮴,수은,불산,염산,페놀,유독물,위해성,POPS\n
생물다양성,자연보전,자연환경,자연보호,야생동물,종다양성,생태계,교란종,외래종,멸종위기,야생식물,야생조류,산림보호,산림파괴,자연훼손,산림훼손,야생동식물,자연경관,국립공원,밀렵,생태관광,철새,생물자원,GMO,생물산업,수렵,고유종,CITES,자연공원,보전지역,생태계서비스,해양생물,의정서,유전자,습지,생태공원,생태발자국,생태용량\n
녹조,수돗물,수질,폐수,BOD,COD,4대강,하천,생태하천,맑은물,깨끗한물,수생태,하천생태,물환경,수변구역,총인,폐사,물고기,총질소,관거,분뇨,정화조,절수,물놀이,빗물,침수,1급수,치어방류,하천 복원,강살리기,물고기 떼죽음,무단방류,먹는물,물부족,물절약,물낭비,수도요금,물재이용,물재생,해수담수화,수처리\n
악취,빛공해,소음,방음"


bs_words <- "\\W+신문,\\W+뉴스,프로그램,제공,지역,환경,계획,정부,\\d+대,\\d+마리,\\d+건,\\d+년,\\d+월,\\d+일,\\d+개소,\\d+시,\\d+분,\\d+초,\\d+명,\\d+장,\\d+개,\\d+원,\\d+호,\\d+주년,\\d+리,\\d+톤,
#전재,전재,저작권자,수습기자,파이낸셜,오랫동안,홈페이지,기자,구독,채널,오전,오후,언론,전국,무단,배포,영상"
#bs_words <- "\\w{1-4}신문,\\w{1-4}뉴스,프로그램,제공,말,지역,환경,계획,정부,곳,\\d{1-10}\\w{1-4},저작권자,수습기자,파이낸셜,오랫동안"


ui <- navbarPage("환경 텍스트 분석 툴", inverse=TRUE,
                 tabPanel("데이터 선택",
                          sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              
                              # Input: Select a file ----
                              radioButtons("corpus", "문서집단 선택",
                                           choices = c("네이버 환경뉴스" = "naver",
                                                       "환경부 보도자료" = "me",
                                                       # "정책포커스" = "focus",
                                                       "사용자 정의파일" = "file"),
                                           selected = "head"),
                              fileInput('file1', '엑셀 파일 선택(*.xlsx)',  accept = c(".xlsx")),
                              
                              textInput(inputId = "start_date", label = "시작 날짜", value = "20190101"),
                              textInput(inputId = "end_date", label = "끝 날짜", value = "20190201"),
                              
                              textInput(inputId = "search_exp", label = "내용필터링", value = ""),
                              
                              # Horizontal line ----
                              
                              
                              # Input: Select number of rows to display ----
                              radioButtons("disp", "Display",
                                           choices = c(Head = "head",
                                                       All = "all"),
                                           selected = "head"),
                              
                              actionButton("do1", "데이터 로드"),
                              width=2
                            ),
                            
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel("데이터 보기 및 저장", DT::dataTableOutput("data1"), downloadButton("dBtn1", "Download"))
                              )
                              
                            )
                          )
                 ),
                 tabPanel("형태소 분석",
                          # Sidebar layout with input and output definitions ----
                          sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                              radioButtons(inputId = "morph_type", "형태소 분석기",
                                           choices = c(은닢한전= "mecab",
                                                           한나눔  = "hannanum",
                                                           영어  = "english",
                                                           띄워쓰기 = "space")),
                              
                              textInput("ng","NGram",value = "1"),
                              textInput("s_words2", "불용어",value = ""),
                              # Input: Select number of rows to display ----
                              radioButtons("disp2", "Display",
                                           choices = c(Head = "head",All = "all"),
                                           selected = "head"),
                              
                              #textInput("e_words2", "포함단어",value = ""),
                              actionButton("do2", "분석실행"), 
                              width=2
                            ),
                            # Main panel for displaying outputs ----
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel("형태소 분석 결과", DT::dataTableOutput("data2"),downloadButton("dBtn2", "Download"))
                              )
                            )
                          )
                 ),
                 
                 tabPanel("키워드 빈도수 분석",
                          sidebarLayout(
                            sidebarPanel(
                              textInput("s_words3", "불용어",value = ""),
                              #textInput("e_words3", "포함단어",value = ""),
                              radioButtons("disp3", "Display",
                                           choices = c(Head = "head",All = "all"),
                                           selected = "head"),
                              actionButton("do3", "빈도수 계산"),
                              width=2
                            ),
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel("빈도수 분석 결과", DT::dataTableOutput("data3"), downloadButton("dBtn3", "Download"))
                              )
                            )
                          )
                 ),
                 
                 
                 tabPanel("키워드 트렌드 분석",
                          sidebarLayout(
                            sidebarPanel(
                              textInput("s_words4", "불용어",value = ""),
                              textInput("e_words4", "포함단어",value = ""),
                              radioButtons("disp3", "Display",
                                           choices = c(Head = "head",All = "all"),
                                           selected = "head"),
                              actionButton("do4", "트렌드 분석"),
                              width=2
                            ),
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel("일별 빈도", DT::dataTableOutput("data4"),downloadButton("dBtn4", "Download"))
                                          #tabPanel("일변 빈도 시각화", plotlyOutput("plot4"))
                              )
                              
                            )
                          )
                 ),
                 
                 tabPanel("키워드 네트워크 분석",
                          sidebarPanel(
                            
                            # Input: Select a file ----
                            #fileInput('file1', '엑셀 파일 선택(*.xlsx)',  accept = c(".xlsx")),
                            
                            #selectInput(inputId = "category", "카테고리",  choices=''),
                            #textInput(inputId = "start_date", label = "시작날짜", value = "20100101"),
                            #textInput(inputId = "end_date", label = "끝날짜", value = "20171231"),
                            #actionButton("filtering", "필터링 실행"),
                            
                            # Horizontal line ----
                            #tags$hr(),
                            textInput(inputId = "SEED", label = "시드", value = "1001"),
                            textInput(inputId = "p_supp", label = "지지도", value = "0.1"),
                            textInput(inputId = "p_conf", label = "신뢰도", value = "0.3"),
                            textInput(inputId = "n_rel", label = "관계수", value = "30"),
                            textInput(inputId = "s_words5", label = "불용어", value = ""),
                            sliderInput(inputId = "v_size", label = "노드크기",  min= 0.01, max=1.0, value=0.1),
                            checkboxInput("chk", label = "2개 이상 관계 활용", value = FALSE),
                            #radioButtons(inputId = "morph_type", "형태소 분석기",
                            #             choices = c(은전한닢 = "mecab",
                            #                             한나눔= "hannanum",
                            #                             띄워쓰기 = "space")),
                            radioButtons(inputId = "ordering", "정렬방법",
                                         choices = c(Lift = "lift",
                                                     Support = "support")),
                            # Horizontal line ----
                            tags$hr(),
                            
                            # Input: Select number of rows to display ----
                            radioButtons("disp5", "Display",
                                         choices = c(Head = "head",
                                                     All = "all"),
                                         selected = "head"),
                            
                            actionButton("do5", "분석실행"), 
                            width=2
                          ),
                          
                          # Main panel for displaying outputs ----
                          mainPanel(
                            tabsetPanel(type = "tabs",
                                        #tabPanel("데이터", DT::dataTableOutput("data5")),
                                        tabPanel("연관분석", tableOutput("result5")),
                                        tabPanel("네트워크 시각화", plotOutput("plot5", width = "100%", height= 800),downloadButton("dBtn", "Download")),
                                        tabPanel("네트워크 지표", tableOutput("indi5"),downloadButton("dBtn5", "Download"))
                                        # tabPanel("메뉴얼",  includeMarkdown("kn_manual.Rmd"))
                            )
                          )
                 ),
                 
                 
                 tabPanel("키워드 변화분석",
                          sidebarLayout(
                            sidebarPanel(
                              textInput("s_words3", "스탑워드",value = ""),
                              #textInput("x_s", "x축 기준",value = ""),
                              #textInput("y_s", "y축 기준",value = ""),
                              radioButtons("disp7", "Display",
                                           choices = c(Head = "head",All = "all"),
                                           selected = "head"),
                              actionButton("do7", "키워드 변화분석"),
                              width=2
                            ),
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel("변화분석 결과", DT::dataTableOutput("result7"), downloadButton("dBtn7", "Download")),
                                          tabPanel("그래프", plotOutput("plot7"))
                              )
                            )
                          )
                 ),
                 tabPanel("키워드 그룹 분석",
                          sidebarLayout(
                            sidebarPanel(
                              textInput("kg_list6", "그룹설정", value = pkg),
                              textAreaInput("kw_list6", "그룹별 키워드설정",value = pkg_kw,rows=20),
                              
                              actionButton("do6", "키워드 그룹분석"),
                              width=3
                            ),
                            mainPanel(
                              tabsetPanel(type = "tabs",
                                          tabPanel("그룹 키워드 빈도수", tableOutput("result6")),
                                          tabPanel("그래프", plotOutput("kgf6"))
                              )
                              
                            )
                          )
                 )
                 
                 
)

temp_result_df1 <<- NULL
temp_result_df2 <<- NULL
temp_result_df3 <<- NULL

# Define server logic to read selected file ----
server <- function(input, output, session){
  result_df1 <- reactive({
    input$do1
    isolate({
      tdir <- NULL
      if(input$corpus == "naver") {
        result_df <- nn
        tdir <- nn_dir
      } else if(input$corpus == "me"){
        result_df <- mn
        tdir <- mn_dir
      } else if(input$corpus == "focus") {
        result_df <- mn
        tdir <- mn_dir
      } else{
        req(input$file1)
        file <- input$file1
        result_df <- read_excel(file$datapath,1)
      }
    
      # 날짜 필터링
      colnames(result_df) <- c("date","title","text")
      start_date <- input$start_date
      end_date <- input$end_date
      
      #print(result_df$date)
      
      start_idx <- which(result_df$date >= ymd(start_date))
      end_idx <- which(result_df$date <= ymd(end_date))
      
      
      c_idx <- intersect(start_idx,end_idx)
      
      if(length(c_idx) > 0){
        result_df <- result_df %>% slice(c_idx)
      }
      else{
        result_df <- result_df
      }
      #View(result_df)
      
      if(input$corpus != "file") {
        for(i in 1:nrow(result_df)){
          fn <- paste(tdir,substr(result_df[i,3],10,100),sep="")
          #print(fn)
          temp <- read_lines(fn)
          temp <- gsub("\\\"\\\"", "\"", temp)
          temp <- temp[-2]
          result_df[i,3] <- paste(temp,collapse=" ")
        }
      }
      
      #print(df$date)
      #print(colnames(result_df))
      
      se <- as.character(input$search_exp)
      
      
      # 내용 필터링
      result_df <- result_df %>% filter(grepl(se,title))
      
      return(result_df)
    })
  })
  
  
  result_df2 <- reactive({     
    input$do2
    #print("실행")
    df <- result_df1()
    
    baseData <- df[,"text"]
    parsedData <- baseData
    baseData <- as.vector(unlist(baseData))
    baseData <- as.vector(baseData)
    
    morph_type = input$morph_type
    if(morph_type == "mecab"){
      text <- r_extract_noun(baseData, language = "ko")
    } else if(morph_type == "hannanum"){
      text <- extractNoun(baseData)
      text <- unlist(lapply(text,paste,collapse=" "))
    } else if(morph_type == "mecab"){
      text <- r_extract_noun(baseData, language = "en")
    }
    else{
      text <- Map(strsplit_space,baseData)
      text <- unlist(lapply(text,paste,collapse=" "))
    }
    
    
    s_words <- input$s_words2
    s_words <- paste(bs_words,s_words,sep=",")
    s_words <- unlist(strsplit(s_words,","))
    
    for(s in s_words){
      text <- gsub(s, "", text)
      #text <- str_replace_all(text,s,"")
      
    }
    text <- gsub("\\s+"," ",text)
    
    result_mat <- data.frame(text)
    #write_xlsx(result_mat, path = fn)
    #temp_result_df2 <<- result_mat
    result_mat
  })   
  
  
  
  result_df3 <- reactive({     
    input$do3
    df <- result_df2()
    baseData <- df[,"text"]
    #print(baseData)
    #parsedData <- baseData
    baseData <- as.vector(unlist(baseData))
    baseData <- as.vector(baseData)
    
    
    corp <- VCorpus(VectorSource(as.vector(unlist(baseData))))
    s_words <- input$s_words3
    s_words <- paste(bs_words,s_words,sep=",")
    s_words <- unlist(strsplit(s_words,","))
    ng <- as.numeric(input$ng)
    ngramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = ng, max = ng))
    #if(morph_type == "english"){
    #  tdm <- TermDocumentMatrix(corp, control=list(tokenize = ngramTokenizer,wordLengths=c(2,Inf), stopwords=s_words, removePunctuation = TRUE))
    #} else{
    tdm <- TermDocumentMatrix(corp, control=list(tokenize = ngramTokenizer,wordLengths=c(2,Inf), stopwords=s_words))
    #}
    
    #분석 결과 가져오기
    
    m <- as.matrix(tdm) 
    v <- sort(rowSums(m),decreasing=TRUE) # 빈도수를 기준으로 내림차순 정렬
    d <- data.frame(word = names(v),freq=v)  # 데이터 프레임 생성
    rownames(d) <- NULL
    #write_xlsx(d, path = fn2)
    d
  })
  
  result_df4 <- reactive({
    input$do4
    cnt <- 30
    df1 <- result_df1()
    df2 <- result_df2()
    baseData <- cbind(df1[,"date"],df2[,"text"])
    dateList <- baseData$date
    dateList <- ymd(as.character(dateList))
    s_words = unlist(strsplit(input$s_words4,","))
    e_words = unlist(strsplit(input$e_words4,","))
    corp <- Corpus(VectorSource(as.vector(unlist(df2[,"text"]))))
    tdm <- TermDocumentMatrix(corp, control=list(wordLengths=c(4,Inf), stopwords=s_words))
    
    #분석 결과 가져오기
    
    m <- t(as.matrix(tdm))
    m <- as_tibble(m)
    m <- cbind(dateList,m)
    
    result <- aggregate(.~dateList, m ,sum)
    
    
    freq_sum <- apply(subset(result,select=-dateList),2,sum)
    
    idx <- order(-freq_sum)[1:cnt] 
    
    temp_idx <- NULL
    # 포함단어 인덱스 찾기
    
    for(e in e_words){
      temp_idx <- c(temp_idx,which(colnames(result) ==  e))
    }
    
    # dateList 컬럼을 제외했기 때문
    idx <- idx + 1
    idx <- c(idx,temp_idx)
    
    sub_result <- subset(result, select = idx)
    sub_result <- cbind(result$dateList,sub_result)
    colnames(sub_result)[1] = 'date'
    
    sub_result
  })
  
  strsplit_space <- function(x)
  {
    as.vector(unlist(strsplit(x," ")))
  }
  
  
  assoc_analysis <- function(p_supp, p_conf,SEED,n_rel,s_words,ordering,is_mwords,morph_type,v_size)
  {
    
    n_rel <- as.numeric(n_rel)
    
    s_words = unlist(strsplit(s_words,","))
    df = result_df2()
    rules <- as.vector(df$text)
    #print(rules)
    rules <- rules[!is.na(rules)]
    
    tran <- Map(strsplit_space,rules)
    tran <- unique(tran)
    tran <- sapply(tran, unique)
    tran <- Filter(function(x){length(x)>=2},tran)
    tran <- sapply(tran, function(x) {Filter(function(y) {nchar(y) > 1} ,x)})
    tran <- sapply(tran, function(x) {Filter(function(y) {all(y != s_words)},x)})
    tran <- Filter(function(x){length(x)>=2},tran)
    
    #print(tran)
    names(tran) <- paste("Tr", 1:length(tran), sep="")
    wordtran <- as(tran, "transactions")
    
    wordtab <- crossTable(wordtran)
    
    
    #지지도와 신뢰도를 낮게 설정할수록 결과 자세히 나옴
    tryCatch(
      {
        max_rules <- 100000
        ares <- apriori(wordtran, parameter=list(supp=as.numeric(p_supp), conf=as.numeric(p_conf),maxlen=5)) 
        if(length(ares) > max_rules){
          print("error")
          return(NA)
        }
        
        ares <- sort(ares, by=ordering)
        print('실행-1')
        result_mat <- as.matrix(arules::inspect(ares))
        colnames(result_mat)[2] <- "->"
        idx <- which(result_mat[,"lhs"] != "{}")
        idx <- intersect(idx,which(as.numeric(result_mat[,"count"]) >= 2))
        result_mat <- result_mat[idx,]
        #result_mat <- sort(result_mat, by="count")
        #ares <- ares[idx,]
        print('실행-2')
        #if(length(idx) < n_rel)
        #  n_rel <- length(idx)
        
        
        # {2개 이상인것} 제거
        if(is_mwords == FALSE){
          rnum <- 2
          idx <- intersect(idx,as.vector(which(sapply(result_mat[,"lhs"],function(x){length(strsplit(x,",")[[1]])}) < rnum)))
          result_mat <- result_mat[idx,]
        }
        
        
        idx <- order(-1*as.numeric(result_mat[,'count']))
        result_mat <- as.matrix(result_mat[idx,])
        
        
        if(n_rel > nrow(result_mat)){
          n_rel <- nrow(result_mat)
        }
        num_idx <- 1:n_rel
        result_mat <- result_mat[num_idx,]
        
        
        print('실행-3')
        # rules <- labels(ares, ruleSep=" ")
        # rules <- gsub("\\{","",rules)
        # rules <- gsub("\\}","",rules)
        # rules <- sapply(rules, strsplit, " ",  USE.NAMES=F)
        # rulemat <- do.call("rbind", rules)
        
        # print(head(rulemat))
        # idx에 포함되는것은 단어들간의 연관관계가 아니기 때문에 제외
        sub_result_mat <- result_mat[,c("lhs","rhs")]
        sub_result_mat <- gsub("\\{","",sub_result_mat)
        sub_result_mat <- gsub("\\}","",sub_result_mat)
        ruleg <- graph.edgelist(sub_result_mat,directed=F) 
        ruleg <- igraph::simplify(ruleg)
        # ruleg <- graph.edgelist(rulemat[idx,],directed=F) 
        
        #### 단어근접중심성파악 ####
        
        print('실행0')
        closen <- igraph::closeness(ruleg)
        
        #### node(vertex), link(edge) 크기 조절 (복잡) ####
        V(ruleg)$size<- igraph::degree(ruleg)/ (1/v_size) * 100
        #### node(vertex), link(edge) 크기 조절 (단순) ####
        indi <- NULL
        
        indi1 <- sort(igraph::degree(ruleg)/(length(degree(ruleg))-1), decreasing=T)
        indi1 <- cbind(names(indi1),round(as.vector(indi1),3))
        indi2 <- sort(igraph::closeness(ruleg), decreasing=T)
        indi2 <- cbind(names(indi2),round(as.vector(indi2),3))
        indi3 <- sort(igraph::betweenness(ruleg,normalized = TRUE), decreasing=T)
        indi3 <- cbind(names(indi3),round(as.vector(indi3),3))
        indi4 <- sort(igraph::eigen_centrality(ruleg)$vector, decreasing=T)
        indi4 <- cbind(names(indi4),round(as.vector(indi4),3))
        
        indi <- cbind(indi1,indi2,indi3,indi4)
        colnames(indi) <- c("연결중심성","값","근접중심성","값","매개중심성","값","고유중심성","값")
        
        #### 매개중심성 #### 
        btw<-igraph::betweenness(ruleg)
        btw.score<-round(btw)+1
        btw.colors<-rev(heat.colors(max(btw.score)))
        V(ruleg)$color<-btw.colors[btw.score]
        #V(ruleg)$degree<-degree(ruleg)
        #V(ruleg)$label.cex<-2*(V(ruleg)$degree / max(V(ruleg)$degree))
        
        ret <- list('result_mat' = result_mat, 'ruleg' = ruleg, 'indi' = indi)
        
        return(ret)
      },
      error=function(error_message) {
        message("This is my custom message.")
        message("And below is the error message from R:")
        return(NA)
      }
    )
  }
  
  
  result_df5 <- reactive({ 
    input$do5
    ret <- assoc_analysis(input$p_supp,input$p_conf,input$SEED,input$n_rel,input$s_words5,input$ordering,input$chk,input$morph_type,input$v_size)
    ret
  })
  
  
  result_df6 <- reactive({     
    input$do6
    df <- result_df1()
    baseData <- df[,"text"]
    baseData <- paste(as.vector(unlist(baseData)),collapse=" ")
    
    kg_list <- input$kg_list6
    kw_list <- input$kw_list6
    kg_list <- strsplit(kg_list, ",", fixed = TRUE)[[1]]
    lines <- strsplit(kw_list, "\n\n", fixed = TRUE)[[1]]
    lines <- trimws(lines)
    kw_list <- lines
    
    result_mat <- matrix(,ncol=1)
    result_mat <- as.data.frame(result_mat)
    colnames(result_mat) <- "번호"
    for(i in 1:length(kg_list)){
      kg <- kg_list[i]
      sub_kw_list <- kw_list[i]
      sub_kw_list <- strsplit(sub_kw_list, ",", fixed = TRUE)[[1]]
      
      wc_list <- NULL
      temp_result_mat <- NULL
      for(kw in sub_kw_list){
        wc <- str_count(baseData, kw)
        wc_list <- c(wc_list,wc)
      }
      
      temp_result_mat <- cbind(sub_kw_list,wc_list)
      #print(i)
      colnames(temp_result_mat) <- c(kg,"빈도수")
      temp_result_mat <- temp_result_mat[order(as.numeric(temp_result_mat[,"빈도수"]),decreasing = TRUE),]
      #print(temp_result_mat)
      
      result_mat <- cbindX(result_mat,temp_result_mat)
    }
    result_mat[,1] <- 1:nrow(result_mat)
    result_mat
  })   
  
  
  result_df7 <- reactive({     
    input$do7
    result_df4 <- result_df4()
    result_df4 <- as.matrix(result_df4[,-1])
    #result_df4 <- as.numeric(result_df4)
    freq_sum <- apply(result_df4,2,sum)
    
    
    x <- 1:nrow(result_df4)
    coef_list <- NULL
    for(i in 1:ncol(result_df4)){
      y <- as.numeric(result_df4[,i])
      y <- (y-min(y))/max(y)
      temp <- lm(y~x)
      coef <- as.numeric(temp$coefficients[1])
      coef <- round(coef,5)
      coef_list <- c(coef_list,coef)
      
    }
    
    
    result_df7 <- cbind(freq_sum,coef_list)
    colnames(result_df7) <- c('빈도수','변화계수')
    result_df7
  }) 
  
  
  
  observeEvent(input$do1,{
    output$data1 <- renderDataTable({
      #write_xlsx(result_df(), path = fn)
      result <- result_df1()
      if(input$disp == "head") {
        return(head(result))
      }
      else {
        return(result)
      }
      
    }, options = list(columnDefs = list(list(
      targets = c(1,2,3),
      render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 150 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 150) + '...</span>' : data;",
        "}")
    ))), callback = JS('table.page(3).draw(false);')
    )
    
    observeEvent(input$do2,{
      #req(input$file1)
      output$result2 <- renderText({"Processing..."})
      
      #s_words = unlist(strsplit(input$s_words,","))
      output$data2 <- renderDataTable({
        if(input$disp2 == "head") {
          return(head(result_df2()))
        }
        else {
          return(result_df2())
        }
        
      })
    })
    
    
    observeEvent(input$do3,{
      output$data3 <- renderDataTable({
        if(input$disp3 == "head") {
          return(head(result_df3()))
        }
        else {
          return(result_df3())
        }
        
      })
    })
    
    observeEvent(input$do4,{
      output$data4 <- renderDataTable({
        return(result_df4())
      })
    })
    
    
    observeEvent(input$do5,{
      output$result5 <- renderTable({
        ret <- result_df5()
        validate(
          need(ret, '데이터 또는 인자를 다시 입력하세요.')
        )
        return(ret$result_mat)
      })
      
      output$plot5 <- renderPlot({
        ret <- result_df5()
        validate(
          need(ret, '데이터 또는 인자를 다시 입력하세요.')
        )
        
        #cairo_pdf(fn2,family="NanumBarunGothic")
        #plot(ret$ruleg,vertex.label.family="NanumBarunGothic",vertex.label.cex=0.7)
        #dev.off()
        plot(ret$ruleg,vertex.label.family="NanumBarunGothic",vertex.label.cex=1.5)
      })
      output$indi5 <- renderTable({
        ret <- result_df5()
        validate(
          need(ret, '데이터 또는 인자를 다시 입력하세요.')
        )
        
        result_mat <- data.frame(ret$indi)
        #write_xlsx(result_mat, path = fn3)
        return(result_mat)
      })
    })
    
    observeEvent(input$do6,{
      output$result6 <- renderTable({
        return(result_df6())
      })
      
      output$kgf6 <- renderPlot({
        result_df6 <- result_df6()
        result_df6 <- result_df6[,2:ncol(result_df6)]
        
        
        # 분할할 열의 개수
        split_count <- 3
        
        # 데이터를 롱 형식으로 변환
        result_df <- NULL
        
        split_count <- 2
        cnt <- 30
        
    
        
        for (i in 1:(ncol(result_df6)/split_count)) {
          start_col <- (i - 1) * split_count + 1
          end_col <- i * split_count
          
          temp_df <- as.matrix(result_df6[, start_col:end_col])
          temp_df <- cbind(rep(colnames(temp_df)[1],nrow(temp_df)),temp_df[,1],temp_df[,2])
          result_df <- rbind(result_df, temp_df)
        }
        
        colnames(result_df) <- c('분야','키워드','빈도수')
        result_df <- result_df[complete.cases(result_df), ]
        result_df <- as.data.frame(result_df)
        result_df[,3] <- as.numeric(as.vector(unlist(result_df[,3])))
        
        result_df <- result_df[order(-result_df[,3]),]
        print("무야")
        result_df <- result_df[1:cnt,]
  
        
        ggdotchart(result_df, x="키워드", y="빈도수",
                   color=c("분야"),                                # Color by groups
                   sorting = "descending",                       # Sort value in descending order
                   add = "segments",                             # Add segments from y = 0 to dots
                   dot.size = 10,                                 # Large dot size
                   label = "빈도수",
                   font.label = list(color = "white", size = 20,  vjust = 0.6),               
                   xlab="",
                   ylab="빈도",
                   ggtheme = theme_pubr()                        # ggplot2 theme
        ) + theme(text = element_text(size=20)) + theme(legend.title = element_blank()) + guides(colour = guide_legend(nrow = 1))
      })
      
      
    })
    
    observeEvent(input$do7,{
      output$result7 <- renderDataTable({
        return(result_df7())
      })
      output$plot7 <- renderPlot({
        thres <- 0.2
        result_df7 <- result_df7()
        result_df7 <- cbind(row.names(result_df7),result_df7)
        result_df7 <- as.data.frame(result_df7)
        result_df7$빈도수 <- as.numeric(unlist(as.vector(unlist(result_df7$빈도수))))
        result_df7$변화계수 <- as.numeric(unlist(as.vector(unlist(result_df7$변화계수))))
        result_df7$group <- 0
        mc <- median(result_df7$빈도수)
        mg <- median(result_df7$변화계수)
        gidx1 <- (result_df7$빈도수 >= mc)
        gidx2 <- (result_df7$변화계수 >= mg)
        result_df7$group[gidx1&(!gidx2)] <- 1
        result_df7$group[(!gidx1)&gidx2] <- 2
        result_df7$group[gidx1&gidx2] <- 3
        
        
        p <- ggplot(data=result_df7,mapping=aes(x=빈도수, y=변화계수)) +  geom_point(size = 3, color = 'grey') + theme_classic(base_size = 30) + theme(legend.position="none")
        #p <-ggplot()
        p <- p + geom_vline(xintercept=mc, linetype='dashed', color='gray', size=1)
        p <- p + geom_hline(yintercept=mg, linetype='dashed', color='gray', size=1)
        p <- p + geom_label_repel(mapping=aes(fill = factor(group), label = as.character(result_df7[,1])),color = 'white', angle = 45, size=7, box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') + xlab("키워드 빈도수") + ylab("키워드 빈도 변화")
        p <- p + theme(axis.title=element_text(size=20))
        
        p
      })
      
    })
    
    
    
    
    output$dBtn1 <- downloadHandler(
      filename = function(){
        paste("output1_",rn,".xlsx",sep="")
      },
      content = function(file){
        write_xlsx(result_df1(), file)
      })
    
    
    output$dBtn2 <- downloadHandler(
      filename = function(){
        paste("output2_",rn,".xlsx",sep="")
      },
      content = function(file){
        write_xlsx(result_df2(), file)
      }
    )
    
    output$dBtn3 <- downloadHandler(
      filename = function(){
        paste("output3_",rn,".xlsx",sep="")
      },
      content = function(file){
        write_xlsx(result_df3(), file)
      }
    )
    
    output$dBtn4 <- downloadHandler(
      filename = function(){
        paste("output4_",rn,".xlsx",sep="")
      },
      content = function(file){
        write_xlsx(result_df4(), file)
      }
    )
    
  })
  
}

s_date <- Sys.Date()
rn <- sample(1:100000000,1)
fn1 <- paste("./output1_", s_date,"__",rn,".xlsx",sep="")
fn2 <- paste("./output2_", s_date,"__",rn,".xlsx",sep="")
fn3 <- paste("./output3_", s_date,"__",rn,".xlsx",sep="")

shinyApp(ui = ui, server = server)
