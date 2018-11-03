###군집별 가사 형태소 분석하기----##########################


##필요한 패키지 로딩----####################################
library(dplyr)
library(KoNLP)

##필요한 데이터 로딩----####################################
allChart <- read.csv("C:/R/BigData/AllChart.csv")
weather <- read.csv("C:/R/BigData/weatherIndex.csv")
#head()함수를 이용하여 성격 파악해보세요.

##데이터 전처리----##########################################

weather <- weather[,-1] #필요없는 열 제거
colnames(weather) <- c("chartDate","평균기온.C","일강수량.mm",
                       "평균풍속.m.s","평균상대습도.per","일최신심적설.cm",
                       "평균전운량.0.1","평균중하층운량.0.1","안개계속시간.hr",
                       "index") #열 이름 다시 붙이기
#allChart 데이터에 군집번호 index 열 붙이기
allChart <- merge(allChart, weather[,c(1,10)], by = "chartDate")

#1번 군집 차트 테이블 생성
Chart1 <- allChart %>% 
    filter(index == 1) %>%  #1번 군집에 해당하는 행만 추출
    select(-chartDate)      #필요없는 열 제거
Chart1 <- Chart1[!duplicated(Chart1),] #군집에서 중복되는 곡 제거

#2번 군집 차트 테이블 생성
Chart2 <- allChart %>% 
    filter(index == 2) %>%
    select(-chartDate)

Chart2 <- Chart2[!duplicated(Chart2),]

#3번 군집 차트 테이블 생성
Chart3 <- allChart %>% 
    filter(index == 3) %>%
    select(-chartDate)

Chart3 <- Chart3[!duplicated(Chart3),]

#4번 군집 차트 테이블 생성
Chart4 <- allChart %>% 
    filter(index == 4) %>%
    select(-chartDate)

Chart4 <- Chart4[!duplicated(Chart4),]

#5번 군집 차트 테이블 생성
Chart5 <- allChart %>% 
    filter(index == 5) %>%
    select(-chartDate)

Chart5 <- Chart5[!duplicated(Chart5),]


##형태소 분석----#########################################

#1번 군집 가사 형태소 분석
wordGroup1 <- vector()  #아래의 for문에서 생성한 words를 차례로 담기위한 비어있는 벡터생성
for(i in 1:nrow(Chart1)){ #1번 군집 곡 목록 전체의 가사 형태소 분석하기 위함.
    lyric <- Chart1$lyrics[i] #곡 목록의 i번째 가사 변수 생성
    
    if(!is.na(lyric)){ #가사 변수가 na인 경우를 방지하기 위함
        words <- paste(SimplePos22((as.character(lyric))))  #lyric를 문자열로 변환 후 22개 형태소로 분해 하는 SimPos22함수 이용
        words <- words[grep("NC|NQ|PV|PA|F",words)]         #22개 형태소중 원하는 형태소의 단어만 추출
        words <- substr(words,1,regexpr("/",words)-1)       #형태소 정보는 제거하고 단어만 추출
        words <- unique(words)                              #중복되는 단어 제거
    } else {
        words <- NA
    }
    
    wordGroup1 <- c(wordGroup1, words) #for문이 진행되면서 앞서있던 wordGroup1 뒤에 words를 이어붙임
}
table1 <- table(wordGroup1) #생성된 wordGroup1의 키워드 빈도 테이블 생성
Group1 <- data.frame(word = names(table1), freq1 = as.numeric(table1)) #생성된 테이블을 데이터 프레임으로 저장

#2번 군집 가사 형태소 분석 (1번 군집과 같은 방식이므로 설명은 생략하겠습니다.)
wordGroup2 <- vector()
for(i in 1:nrow(Chart2)){
    lyric <- Chart2$lyrics[i]
    
    if(!is.na(lyric)){
        words <- paste(SimplePos22((as.character(lyric))))
        words <- words[grep("NC|NQ|PV|PA|F",words)]
        words <- substr(words,1,regexpr("/",words)-1)
        words <- unique(words)
    } else {
        words <- NA
    }
    
    wordGroup2 <- c(wordGroup2, words)
}
table2 <- table(wordGroup2)
Group2 <- data.frame(word = names(table2), freq2 = as.numeric(table2))

#3번 군집 가사 형태소 분석
wordGroup3 <- vector()
for(i in 1:nrow(Chart3)){
    lyric <- Chart3$lyrics[i]
    
    if(!is.na(lyric)){
        words <- paste(SimplePos22((as.character(lyric))))
        words <- words[grep("NC|NQ|PV|PA|F",words)]
        words <- substr(words,1,regexpr("/",words)-1)
        words <- unique(words)
    } else {
        words <- NA
    }
    
    wordGroup3 <- c(wordGroup3, words)
}
table3 <- table(wordGroup3)
Group3 <- data.frame(word = names(table3), freq3 = as.numeric(table3))

#4번 군집 가사 형태소 분석
wordGroup4 <- vector()
for(i in 1:nrow(Chart4)){
    lyric <- Chart4$lyrics[i]
    
    if(!is.na(lyric)){
        words <- paste(SimplePos22((as.character(lyric))))
        words <- words[grep("NC|NQ|PV|PA|F",words)]
        words <- substr(words,1,regexpr("/",words)-1)
        words <- unique(words)
    } else {
        words <- NA
    }
    
    wordGroup4 <- c(wordGroup4, words)
}

table4 <- table(wordGroup4)
Group4 <- data.frame(word = names(table4), freq4 = as.numeric(table4))

#5번 군집 가사 형태소 분석
wordGroup5 <- vector()
for(i in 1:nrow(Chart5)){
    lyric <- Chart5$lyrics[i]
    
    if(!is.na(lyric)){
        words <- paste(SimplePos22((as.character(lyric))))
        words <- words[grep("NC|NQ|PV|PA|F",words)]
        words <- substr(words,1,regexpr("/",words)-1)
        words <- unique(words)
    } else {
        words <- NA
    }
    
    wordGroup5 <- c(wordGroup5, words)
}

table5 <- table(wordGroup5)
Group5 <- data.frame(word = names(table5), freq5 = as.numeric(table5))

####
#DTM(문서빈도행렬)생성----############################################

#빈도 테이블 만들기
word <- c(wordGroup1, wordGroup2, wordGroup3, wordGroup4, wordGroup5)   #다섯군집에서 형태소 분석 결과 얻어진 키워드들을 하나로 합침
word <- unique(word)                #중복제거하여 전체 키워드 목록 생성
dtm <- data.frame(word = word)      #키워드 목록을 데이터프레임으로 변환

dtm <- merge(dtm, Group1, by = "word", all = TRUE)  #키워드 목록 테이블과 1번 군집 키워드 테이블을 조인하기
dtm$freq1[is.na(dtm$freq1)] <- 0  #조인 후 NA값이 나타난 키워드는 1번 군집에서 한 번도 나타나지 않았으므로 0으로 처리

dtm <- merge(dtm, Group2, by = "word", all = TRUE)  #위 과정을 2번 군집에서 반복
dtm$freq2[is.na(dtm$freq2)] <- 0

dtm <- merge(dtm, Group3, by = "word", all = TRUE)  #위 과정을 3번 군집에서 반복
dtm$freq3[is.na(dtm$freq3)] <- 0

dtm <- merge(dtm, Group4, by = "word", all = TRUE)  #위 과정을 4번 군집에서 반복
dtm$freq4[is.na(dtm$freq4)] <- 0

dtm <- merge(dtm, Group5, by = "word", all = TRUE)  #위 과정을 5번 군집에서 반복
dtm$freq5[is.na(dtm$freq5)] <- 0

#빈도 테이블 저장
write.csv(dtm, "C:/R/BigData/freqTable11.csv", row.names = FALSE)