#크롤링 함수
#예시 : DoCrawling("2017-01-01","2018-09-30",4456)

DoCrawling <- function(startDate, endDate, portNum){
  
  #startDate, endDate를 숫자로 바꾸기
  startNum <- as.numeric(as.Date(startDate))
  endNum <- as.numeric(as.Date(endDate))
  
  #자동으로 페이지 이동해서 스크랩하기
  for(i in startNum:endNum){ 
    
    portNum <- as.integer(portNum + i - 16800)
    
    #Rselenium을 이용해서 크롬 열기
    ch <- wdman::chrome(port = portNum)
    remDr <- remoteDriver(port = portNum, browserName = "chrome")
    remDr$open()
    
    day <- as.Date(i, origin="1970-01-01") #차트 스크랩할 날짜 정하기
    url <- paste("https://music.bugs.co.kr/chart/track/day/total?chartdate=",day,sep="")
    
    #RSelenium이용해서 크롬 열기
    
    remDr$navigate(url) #벅스차트페이지이동
    
    frontpage <- remDr$getPageSource() #페이지의 소스 긁어오기
    #100차트 제목 따오기
    title <- read_html(frontpage[[1]]) %>%
      html_nodes("th") %>%
      html_nodes(".title") %>%
      html_node("a") %>%
      html_text()
    
    #100차트 아티스트 따오기
    artist <- read_html(frontpage[[1]]) %>%
      html_nodes("td") %>%
      html_nodes(".artist") %>%
      html_node("a") %>%
      html_text()
    
    #100차트 trackNum 따오기
    trackNum <- read_html(frontpage[[1]]) %>%
      html_nodes("th") %>%
      html_nodes(".title") %>%
      html_node("a")
    
    flag <- regexpr("\\listen", as.character(trackNum)) #''안에 숫자가 7개 혹은 8개!
    trackNum <- substr(as.character(trackNum),flag+8,flag+15)
    flag <- gregexpr("[0-9]", trackNum)
    
    index <- vector()
    for(j in 1:length(trackNum)){
      len <- length(flag[[j]])
      index <- c(index,len)
    }
    trackNum <- substr(trackNum, 1, index)
    
    
    #가사 크롤링하기위한 준비과정. 여러개의 크롬 클라이언트 열기
    ch1 <- wdman::chrome(port = as.integer(portNum * 2))
    remDr1 <- remoteDriver(port = as.integer(portNum * 2), browserName = "chrome")
    remDr1$open()
    
    ch2 <- wdman::chrome(port = as.integer(portNum * 3))
    remDr2 <- remoteDriver(port = as.integer(portNum * 3), browserName = "chrome")
    remDr2$open()
    
    ch3 <- wdman::chrome(port = as.integer(portNum * 4))
    remDr3 <- remoteDriver(port = as.integer(portNum * 4), browserName = "chrome")
    remDr3$open()
    
    ch4 <- wdman::chrome(port = as.integer(portNum * 5))
    remDr4 <- remoteDriver(port = as.integer(portNum * 5), browserName = "chrome")
    remDr4$open()
    
    lyrics <- vector()
    for(k in seq(from=1, to = length(trackNum), by = 4)){
      ##첫번째 포트에서 가사 크롤링
      url1 <- paste("https://music.bugs.co.kr/track/",trackNum[k],"?wl_ref=list_tr_08_chart",sep = "")
      remDr1$navigate(url1)
      frontpage1 <- remDr1$getPageSource()
      
      words1 <- read_html(frontpage1[[1]]) %>%
        html_node("xmp") %>%
        html_text()
      words1 <- gsub("\n"," ",words1)
      
      ##두번째 포트에서 가사 크롤링
      url2 <- paste("https://music.bugs.co.kr/track/",trackNum[k+1],"?wl_ref=list_tr_08_chart",sep = "")
      remDr2$navigate(url2)
      frontpage2 <- remDr2$getPageSource()
      
      words2 <- read_html(frontpage2[[1]]) %>%
        html_node("xmp") %>%
        html_text()
      words2 <- gsub("\n"," ",words2)
      
      ##세번째 포트에서 가사 크롤링
      url3 <- paste("https://music.bugs.co.kr/track/",trackNum[k+2],"?wl_ref=list_tr_08_chart",sep = "")
      remDr3$navigate(url3)
      frontpage3 <- remDr3$getPageSource()
      
      words3 <- read_html(frontpage3[[1]]) %>%
        html_node("xmp") %>%
        html_text()
      words3 <- gsub("\n"," ",words3)
      
      ##네번재 포트에서 가사 크롤링
      url4 <- paste("https://music.bugs.co.kr/track/",trackNum[k+3],"?wl_ref=list_tr_08_chart",sep = "")
      remDr4$navigate(url4)
      frontpage4 <- remDr4$getPageSource()
      
      words4 <- read_html(frontpage4[[1]]) %>%
        html_node("xmp") %>%
        html_text()
      words4 <- gsub("\n"," ",words4)
      
      
      lyrics <- c(lyrics, words1, words2, words3, words4)
    }
    
    #열었던 크롬창 닫기
    remDr$close()
    remDr1$close()
    remDr2$close()
    remDr3$close()
    remDr4$close()
    
    #title, artist, trackNum, lyrics로 이루어진 chart 테이블 생성하기
    chart <- data.frame(title,artist,trackNum,lyrics)
    path <- paste("C:/R/BigData/chart/chart",day,".csv",sep="")
    write.csv(chart, file = path)
    
    Sys.sleep(20) #지속적으로 접근시 차단되는 문제를 막고자 함
  }
  
}
