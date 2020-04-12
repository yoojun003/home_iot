install.packages('ggmap')
install.packages('gglpot2')
install.packages('raster')
install.packages('rgeos')
install.packages('maptools')
install.packages('rgdal')
install.packages('sf')

library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(sf)

install.packages("xlsx")
library(xlsx)
ls('package:xlsx')

people_2017<-read.csv('C:/Users/STU16/Desktop/교육/300_1차 프로젝트/2017_1인가구 현황.csv', stringsAsFactors = F,header = T)
str(people_2017)
people_2017[people_2017$행정구역별.시군구.=='서울특별시'& people_2017$X1인가구=='계','X1인가구']
people_2017_sum<-people_2017[nchar(people_2017$C행정구역별.시군구.)=='2' & people_2017$성별 == '계',c('행정구역별.시군구.','주택.계')]
people_2017_sum$per <- round(people_2017_sum$주택.계/sum(people_2017_sum$주택.계)*100,2)
people_2017_sum

one_people<-read.csv('C:/Users/STU16/Desktop/교육/300_1차 프로젝트/2016~2020 연령별 가구 현황.csv', stringsAsFactors = F, header = T)
one_people_sum<-one_people[one_people$가구주의.연령별=='합계',]
one_people_sum
one_people_sum<-one_people_sum[,-2]
one_people_sum
colnames(one_people_sum)<-c('지역','2016합계','2016년1인','2016년비율',
                            '2017합계','2017년1인','2017년비율',
                            '2018합계','2018년1인','2018년비율',
                            '2019합계','2019년1인','2019년비율',
                            '2020합계','2020년1인','2020년비율')
one_people_sum[,c(2,4,6)]
one_people_sum
rownames(one_people_sum)<-NULL
colnames(one_people_sum)
one_2019<-one_people_sum[, c('지역','2019년비율')]
str(one_2019)
one_2019$id <- c(11,21,22,23,24,25,26,29,31,32,33,34,35,36,37,38,39)
one_2019$id <- 1:17

colnames(one_2019)<- c('region','per','id')
one_2019 #2019년 1인 가구 비율

#2019년 1인 가구 비율 시각화(geom_polygon)
library(raster)
korea <- getData('GADM', country='kor', level=2)
ggplot() + geom_polygon(data=korea, aes(x=long, y=lat, group=group), fill='white', color='black')
korea <- shapefile('C:/Users/STU16/Desktop/교육/300_1차 프로젝트/SIG_201703/TL_SCCO_SIG.shp') 
ggplot() + geom_polygon(data=korea, aes(x=long, y=lat, group=group), fill='white', color='black')
korea <- fortify(korea, region='SIG_CD')
korea <- merge(korea, one_2019, by='id')
result <- read.csv("C:/Users/STU16/Desktop/교육/300_1차 프로젝트/result.csv", header=T, as.is=T)

tail(korea_map,50)
str(one_2019)
head(korea)



korea<-shapefile('C:/Users/STU16/Desktop/교육/300_1차 프로젝트/CTPRVN_201905/TL_SCCO_CTPRVN.shp')
korea
korea<-spTransform(korea,CRS('+proj=longlat'))
#좌표설정
korea_map <- fortify(korea)
head(korea_map)
head(korea)
str(korea)
str(korea_map)

merge_result<- merge(korea_map,one_2019,by='id')
merge_result

colors <- c(#018571','#F5F5F5','#A6611A')
  
  ggplot()+
    geom_polygon(data = merge_result,aes(x=long, y=lat, group=group, fill=per))+
    labs(fill = '1인가구 \n비율(%)')+
    labs(x='lon',y='lat')
  #scale_fill_gradient(name = "Years", low = "#A6611A",mid='white' ,high = "#018571", guide = "colorbar") +
  
  one_people<-read.csv('C:/Users/park/Desktop/300_1차 프로젝트/2016~2020 연령별 가구 현황.csv', stringsAsFactors = F, header = T)
  one_people_sum<-one_people[one_people$가구주의.연령별=='합계',]
  one_people_sum
  one_people_sum<-one_people_sum[,-2]
  one_people_sum
  colnames(one_people_sum)<-c('지역','2016합계','2016년1인','2016년비율',
                              '2017합계','2017년1인','2017년비율',
                              '2018합계','2018년1인','2018년비율',
                              '2019합계','2019년1인','2019년비율',
                              '2020합계','2020년1인','2020년비율')
  one_people_sum[,c(2,4,6)]
  one_people_sum
  rownames(one_people_sum)<-NULL
  colnames(one_people_sum)
  
  sum_hist<-one_people_sum[,c(1,3,6,9,12,15)]
  sum_hist[1,]
  colnames(sum_hist)<-c('지역','2016','2017','2018','2019','2020')
  sum_hist
  
  library(reshape2)
  hist_see<-melt(sum_hist, id = '지역')
  colnames(hist_see)<-c('region','year','cn')
  hist_see #지역별 1인가구 변화 테이블
  
  
  #지역별 1인가구 변화 시각화(peom_col,dodge)
  ggplot(hist_see,aes(x=region,y=cn/10000, group = year))+
    geom_col(aes(fill=year),position = 'dodge')+
    ggtitle('년도 별 1인 가구 변화')+
    theme(plot.title = element_text(lineheight=0.8,face='bold',color='darkblue',hjust = 0.5))+
    theme(plot.subtitle = element_text(lineheight=0.8,face='bold',color='darkblue',hjust = 0.5))+
    #theme(plot.subtitle = element_text()) subtitle 양식 설정
    labs(x='광역시',y='단위(만:가구)')+
    theme(legend.title.align=0.1, #범례 제목 위치
          legend.box.background = element_rect(), #범례 테두리 설정
          legend.box.margin = margin(t=0.1,r=0.1, b=0.1, l=0.1, unit='cm'))+ #legend.box.margin = margin() 범례의 공백설정
    scale_y_continuous(breaks=seq(0,140,20))+
    coord_flip()
  
  
  #서울구 시각화 테이블
  seoul_gu_2017<-read.csv('C:/Users/STU16/Desktop/교육/300_1차 프로젝트/서울_가구원수별 현황.csv', stringsAsFactors = F, header = T)
  seoul_gu_2017<-seoul_gu_2017[-1,]
  
  library(doBy)
  seoul_gu_2017<-orderBy(~-per,seoul_gu_2017)
  seoul_gu_2017
  
  #서울구 시각화(bar)
  ggplot(seoul_gu_2017,aes(x=reorder(region,per),y=per))+
    geom_bar(stat='identity', fill='cyan3')+
    geom_text(label = round(seoul_gu_2017$per,2),vjust=-0.3, size = 3)+
    ggtitle('2017년 서울시 1인가구 비율')+
    labs(x='지역',y='1인 가구 비율(%)')+
    coord_flip()
  
  #geom_text(aes(label=seoul_gu_2017$per),vjust=-0.5, size=3)
  #관악구 46.02%로 가장 높음, 그다음 중구, 종로구에 1인가구가 많은 것으로 파악됨
  #반면에 노원구 도봉구 양천구는 20%대로 상대적으로 적음
  
  #서울구 남녀 비율 테이블
  seoul_gu_2017_2<-read.csv('C:/Users/STU16/Desktop/교육/300_1차 프로젝트/서울_연령별1인가구_구_csv.csv', stringsAsFactors = F, header = T)
  seoul_gu_2017_2<-seoul_gu_2017_2[nchar(seoul_gu_2017_2$성별)==2,c(1:4,grep('대{1}',x))]
  seoul_sex_2017<-seoul_gu_2017_2[,1:3]
  colnames(seoul_sex_2017)<-c('gu','sex','people')
  str(seoul_sex_2017) #서울시 1인가구 성비
  seoul_sex_2017<-orderBy(~-people,seoul_sex_2017)
  seoul_sex_2017
  
  
  
  #서울구 남녀 파이차트 테이블
  seoul_sex_2017_2<-read.csv('C:/Users/STU16/Desktop/교육/300_1차 프로젝트/서울_연령별1인가구_구_sex.csv', stringsAsFactors = F, header = T)
  str(seoul_sex_2017_2)
  orderBy(~per,seoul_sex_2017_2)
  seoul_sex_2017_2#서울구 남녀 파이차트 테이블
  
  
  #서울구 남녀 시각화(파이차트)
  colnames(seoul_sex_2017_2)<-c('gu','성별','sum','per')
  
  ggplot(seoul_sex_2017_2, aes("", per, fill = 성별)) + 
    geom_bar(stat = "identity", color = "white", size = 1) +
    geom_text(aes(label = round(per,2)), 
              position = position_stack(vjust = 0.5), 
              color = "white", size = 3) +
    coord_polar(theta = "y") +
    facet_wrap(~ gu, ncol = 5) +
    scale_fill_manual(values = c("blue", "red")) +
    theme_void()
  
  #서울구 남녀 테이블(bar)
  seoul_gu_2017_2<-read.csv('C:/Users/STU16/Desktop/교육/300_1차 프로젝트/서울_연령별1인가구_구_csv.csv', stringsAsFactors = F, header = T)
  seoul_gu_2017_2<-seoul_gu_2017_2[nchar(seoul_gu_2017_2$성별)==2,c(1:4,grep('대{1}',x))]
  seoul_sex_2017<-seoul_gu_2017_2[,1:3]
  colnames(seoul_sex_2017)<-c('gu','sex','people')
  str(seoul_sex_2017) #서울시 1인가구 성비
  seoul_sex_2017<-orderBy(~-people,seoul_sex_2017)
  seoul_sex_2017
  colnames(seoul_sex_2017)<-c('gu','성별','people')
  
  #서울구 남녀 시각화(bar)
  ggplot(seoul_sex_2017,aes(x=reorder(gu,people), fill =성별, y= people/10000))+
    geom_bar(stat = "identity")+
    ggtitle('서울시 1인가구 성비')+
    geom_text(label = round(seoul_sex_2017$people/10000,2), size = 3.2, vjust = 2,
              position = position_stack())+
    labs(x='서울시',y='인구(명)')+
    coord_flip()
  
  a<-read.csv('C:/Users/park/Desktop/300_1차 프로젝트/서울_5대 범죄발생_2017년.csv',stringsAsFactors = F, header = T)
  str(a)
a$폭력<-as.data.frame(a$폭력/100)  
a$절도<-as.data.frame(a$절도/100)
a$강간강제추행<-as.data.frame(a$강간강제추행/10)
a
b<-a[,c(-2,-5,-7,-9,-11,-12)]  
colnames(b)<-c('region','살인','강도','강간강제추행/10','절도/100','폭력/100')

colnames(a)
str(a)
crime_seoul<-melt(b, id = 'region')
crime_seoul
colnames(crime_seoul)<-c('region','범죄','cn')
crime_seoul

#서울시 범죄 현황(peom_col,dodge)
ggplot(crime_seoul,aes(x=region,y=cn, group = 범죄))+
  geom_col(aes(fill=범죄),position = 'dodge')+
  ggtitle('서울시 강력 5대 범죄 현황')+
  theme(plot.title = element_text(lineheight=0.8,face='bold',color='darkblue',hjust = 0.5))+
  theme(plot.subtitle = element_text(lineheight=0.8,face='bold',color='darkblue',hjust = 0.5))+
  #theme(plot.subtitle = element_text()) subtitle 양식 설정
  labs(x='서울시',y='건수')+
  theme(legend.title.align=0.1, #범례 제목 위치
        legend.box.background = element_rect(), #범례 테두리 설정
        legend.box.margin = margin(t=0.1,r=0.1, b=0.1, l=0.1, unit='cm'))+ #legend.box.margin = margin() 범례의 공백설정
  scale_y_continuous(breaks=seq(0,140,20))
  
  
  
  
  
  
  
  
  
  