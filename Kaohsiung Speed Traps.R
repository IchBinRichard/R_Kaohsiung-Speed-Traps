install.packages('leaflet')  #首先安裝這次最主要的leaflet套件

library(leaflet)     #接著載入這次分析會使用到的套件
library(rio)
library(dplyr)

#把本次用來分析的csv檔import進RStudio
KAO <- import('D:/Richard_Lee/R_project/srcFile.csv')
#這次會用到的資料欄位做個篩選
camera_Kao <- select(KAO,設置市區鄉鎮,設置地址,經度,緯度,速限)
#這次的分析使用速限來做分組
limit_25 <- filter(camera_Kao,速限==25)
limit_40 <- filter(camera_Kao,速限==40)
limit_50 <- filter(camera_Kao,速限==50)
limit_60 <- filter(camera_Kao,速限==60)
limit_70 <- filter(camera_Kao,速限==70)
limit_90 <- filter(camera_Kao,速限==90)
#每一個指標popup出來的訊息，先在這裡調整好格式
inf25 <- paste(sep='<br/>',limit_25$設置市區鄉鎮,limit_25$設置地址,'速限:25')
inf40 <- paste(sep='<br/>',limit_40$設置市區鄉鎮,limit_40$設置地址,'速限:40')
inf50 <- paste(sep='<br/>',limit_50$設置市區鄉鎮,limit_50$設置地址,'速限:50')
inf60 <- paste(sep='<br/>',limit_60$設置市區鄉鎮,limit_60$設置地址,'速限:60')
inf70 <- paste(sep='<br/>',limit_70$設置市區鄉鎮,limit_70$設置地址,'速限:70')
inf90 <- paste(sep='<br/>',limit_90$設置市區鄉鎮,limit_90$設置地址,'速限:90')
#因為是用速限來做分組，所以對速限來分割區間，並加上label使得更容易閱讀
camera_Kao$rank_speed <- cut(camera_Kao$速限,
                             breaks = c(0,25,40,50,60,70,90),
                             labels = c('限速25公里','限速40公里','限速50公里',
                                        '限速60公里','限速70公里','限速90公里'))
#延續上一個步驟，對速限分割完區間，共分了六組，所以用六種顏色來分別標示不同的速限
pal = colorFactor(palette = c('white','purple','green',
                              'blue','red','black'),domain = camera_Kao$rank_speed)
#主要繪圖程式碼
map <- leaflet()%>%   #leaflet()製作繪圖板
  addTiles()%>%                 #addTiles()加上一個預設的地圖資料
  #fitBounds()用來ˊ設定地圖的位置，讓畫面可呈現指定的方形區域，這兩組經緯度是高雄市的經緯度位置
  fitBounds(120.1032,22.28,121.0115,23.28)%>%  
  #addAwesomeMarkers可以在地圖上標記資料內的經緯度位置
  addAwesomeMarkers(icon=awesomeIcons(icon = 'camera',markerColor = 'white'),
                    clusterOptions = markerClusterOptions(),
                    lng = limit_25$經度,lat = limit_25$緯度,popup = inf25,group = '限速25公里')%>%
  addAwesomeMarkers(icon=awesomeIcons(icon = 'camera',markerColor = 'purple'),
                    clusterOptions = markerClusterOptions(),
                    lng = limit_40$經度,lat = limit_40$緯度,popup = inf40,group = '限速40公里')%>%
  addAwesomeMarkers(icon=awesomeIcons(icon = 'camera',markerColor = 'green'),
                    clusterOptions = markerClusterOptions(),
                    lng = limit_50$經度,lat = limit_50$緯度,popup = inf50,group = '限速50公里')%>%
  addAwesomeMarkers(icon=awesomeIcons(icon = 'camera',markerColor = 'blue'),
                    clusterOptions = markerClusterOptions(),
                    lng = limit_60$經度,lat = limit_60$緯度,popup = inf60,group = '限速60公里')%>%
  addAwesomeMarkers(icon=awesomeIcons(icon = 'camera',markerColor = 'red'),
                    clusterOptions = markerClusterOptions(),
                    lng = limit_70$經度,lat = limit_70$緯度,popup = inf70,group = '限速70公里')%>%
  addAwesomeMarkers(icon=awesomeIcons(icon = 'camera',markerColor = 'black'),
                    clusterOptions = markerClusterOptions(),
                    lng = limit_90$經度,lat = limit_90$緯度,popup = inf90,group = '限速90公里')%>%
  #addLayersControl用來製作控制面板，可以和user進行互動
  addLayersControl(overlayGroups = c('限速25公里','限速40公里','限速50公里',
                                     '限速60公里','限速70公里','限速90公里'),
                   options = layersControlOptions(collapsed = TRUE))%>%
  addLegend(position = 'bottomright',pal=pal,values = camera_Kao$rank_speed,opacity = 1 )
map
