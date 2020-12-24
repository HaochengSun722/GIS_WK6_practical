library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(stringr)
library(tidyverse)
library(raster)#DBSCAN
library(fpc)#DBSCAN
library(dbscan)
library(ggplot2)
library(OpenStreetMap)
library(spdep)
##################################

#Part1 setting data
##Step1-加载数据
LondonBoroughs <- st_read(here::here("data", 
                                     "ESRI", 
                                     "London_Borough_Excluding_MHW.shp"))

BoroughMap <- LondonBoroughs %>%
  dplyr::filter(stringr::str_detect(GSS_CODE, "^E09"))%>%
  st_transform(., 27700)
qtm(BoroughMap)

##Now get the location of all Blue Plaques in the City
BluePlaques <- st_read(here::here("data",
                                  "open-plaques-london-2018-04-08.geojson")) %>%
  st_transform(.,27700)

summary(BluePlaques)

tmap_mode("view")

tm_shape(BoroughMap)+tm_polygons(col = NA,alpha = 0.1)+
  tm_shape(BluePlaques)+tm_dots(col = "blue")

##Step2-清理数据
BluePlaques <- distinct(BluePlaques) ###保留unique数据

BluePlaquesSub <- BluePlaques[BoroughMap,]###保留在BoroughMap内部的BluePlaques

tm_shape(BoroughMap) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

##Step3-提取少量数据
#extract the borough
Harrow <- BoroughMap %>%
  filter(., NAME=="Harrow")
#Check to see that the correct borough has been pulled out
tm_shape(Harrow) +
  tm_polygons(col = NA, alpha = 0.5)
#clip the data to our single borough
BluePlaquesSub <- BluePlaques[Harrow,]

tm_shape(Harrow) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")
#now set a window as the borough boundary
window <- as.owin(Harrow) ##提取geometry的形状
plot(window)

#spatstat有其独特的空间分析对象，不能识别一般sf,sp,polygon,point对象。
#spatstat 做point pattern analysis时，只能识别ppp(point pattern object)
BluePlaquesSub<- BluePlaquesSub %>%
  as(., 'Spatial') #先将SpatialPointsDataFrame转化为标准的Formal SPDF
plot(BluePlaquesSub)

BluePlaquesSub.ppp <- ppp(x=BluePlaquesSub@coords[,1], ##POINT的横坐标
                          y=BluePlaquesSub@coords[,2], ##POINT的纵坐标
                          window=window)

BluePlaquesSub.ppp %>%
  plot(.,pch=16,cex=0.5, 
       main="Blue Plaques Harrow")
#########################################################################3

###Part2-point pattern analysis
##Step1-核密度估算(Kernel Density Estimation)
BluePlaquesSub.ppp %>%
  density(., sigma=1000) %>% #sigma代表核密度的直径
  plot(main="BluePlaques in Harrow")

##Step2-四方分析(quadrat analysis)
#先将ppp以plot的形式表现出来
plot(BluePlaquesSub.ppp,
     pch=16,
     cex=0.5, 
     main="Blue Plaques in Harrow")

#now count the points in that fall in a 6 x 6
#grid overlaid across the 
#windowBluePlaquesSub.ppp2<-
BluePlaquesSub.ppp %>%
  quadratcount(.,nx = 6, ny = 6)%>%
  plot(., add=T, col="red")

#汇总各方格数据，并转化为Frequency
Qcount <- BluePlaquesSub.ppp %>%
  quadratcount(.,nx = 6, ny = 6) %>%
  as.data.frame() %>% #将各方格内的数据汇总形成dataframe
  dplyr::count(Var1=Freq)%>% #汇总frequency，各数据出现的频率
  dplyr::rename(Freqquadratcount=n) #重命名

Qcount %>% 
  summarise_all(class) #检查列表属性，是否为numeric


sums <- Qcount %>%
  #calculate the total blue plaques (Var * Freq)
  mutate(total = Var1 * Freqquadratcount) %>% #算出所有point的数量
  dplyr::summarise(across(everything(), sum))%>% #算出所有column的和
  dplyr::select(-Var1) #去掉column-Var1

lambda<- Qcount%>%
  #calculate lambda
  mutate(total = Var1 * Freqquadratcount)%>%
  dplyr::summarise(across(everything(), sum)) %>%
  mutate(lambda=total/Freqquadratcount) %>% #算出λ，平均出现次数
  dplyr::select(lambda)%>%
  pull(lambda)#将λ拉出，作为单独变量

QCountTable <- Qcount %>%
  mutate(Pr=((lambda^Var1)*exp(-lambda))/factorial(Var1))%>%
  #now calculate the expected counts based on our total number of plaques
  #and save them to the table
  mutate(Expected= (round(Pr * sums$Freqquadratcount, 0)))

#Compare the frequency distributions of the observed and expected point patterns
plot(c(1,5),c(0,14), type="n",
     xlab="Number of Blue Plaques (Red=Observed,Blue=Expected)", 
     ylab="Frequency of Occurances")
points(QCountTable$Freqquadratcount, 
       col="Red", 
       type="o", 
       lwd=3)
points(QCountTable$Expected, col="Blue", 
       type="o", 
       lwd=3)

#由于右侧显示出的结果，observed和excepted趋势相同，我们可以假设其可能为完全随机
#分布，但由于还不能确定因此需要检验Chi square的p-value是否大于0.05
teststats <- quadrat.test(BluePlaquesSub.ppp, nx = 6, ny = 6)
#Warning message:
#Some expected counts are small; chi^2 approximation may be inaccurate 
plot(teststats)

plot(BluePlaquesSub.ppp,pch=16,cex=0.5, main="Blue Plaques in Harrow")
plot(teststats, add=T, col = "red")
#左上角的数字代表observed即观察到的数据，右上角的数字的代表expected即按照泊松
#分布的预期数据，最下角的数字代表残差值residual value

##Step3 Ripley's K检验
K <- BluePlaquesSub.ppp %>%
  Kest(., correction="border") %>%
  plot()

###Part3 DBSCAN
#虽然quadrat分析和Ripley’s K可以发现空间中是否有集聚clusters（exploratory techniques），
#但是却无法确定cluster出现的位置，因此需要DBSCAN

##Step1 基础分析
#first check the coordinate reference system of the Harrow spatial polygon:
st_geometry(BoroughMap)

#first extract the points from the spatial points data frame
BluePlaquesSubPoints <- BluePlaquesSub %>%
  coordinates(.)%>%
  as.data.frame() #创建一个dataframe,包含各个点的坐标，两列数据

#now run the dbscan analysis
db <- BluePlaquesSubPoints %>%
  fpc::dbscan(.,eps = 700, MinPts = 4)#eps选为700m，MinPts选为4个

#now plot the results
plot(db, BluePlaquesSubPoints, main = "DBSCAN Output", frame = F)
plot(BoroughMap$geometry, add=T)


##如果不确定eps的选取，也可以利用dbscan::kNNdistplot找到合适的eps
BluePlaquesSubPoints%>%
  dbscan::kNNdistplot(.,k=4)

##Step2 美化输出（利用ggplot2）
db
db$cluster
BluePlaquesSubPoints<- BluePlaquesSubPoints %>%
  mutate(dbcluster=db$cluster) ##将各个点eps范围内的点的个数加到dataframe之后一列

chulls <- BluePlaquesSubPoints %>%
  group_by(dbcluster) %>%
  dplyr::mutate(hull = 1:n(),
                hull = factor(hull, chull(coords.x1, coords.x2)))%>%
  arrange(hull)
chulls <- chulls %>%
  filter(dbcluster >=1)
dbplot <- ggplot(data=BluePlaquesSubPoints, 
                 aes(coords.x1,coords.x2, colour=dbcluster, fill=dbcluster)) 
#add the points in
dbplot <- dbplot + geom_point()
#now the convex hulls
dbplot <- dbplot + geom_polygon(data = chulls, 
                                aes(coords.x1,coords.x2, group=dbcluster), 
                                alpha = 0.5) 
#now plot, setting the coordinates to scale correctly and as a black and white plot 
#(just for the hell of it)...
dbplot + theme_bw() + coord_equal()

##Step3 增加底图basemap
###add a basemap
##First get the bbox in lat long for Harrow
HarrowWGSbb <- Harrow %>%
  st_transform(., 4326)%>%
  st_bbox()

basemap <- OpenStreetMap::openmap(c(51.5549876,-0.4040502),c(51.6405356,-0.2671315),
                                  zoom=NULL,
                                  "stamen-toner")

# convert the basemap to British National Grid
basemap_bng <- openproj(basemap, projection="+init=epsg:27700")

autoplot.OpenStreetMap(basemap_bng) + 
  geom_point(data=BluePlaquesSubPoints, 
             aes(coords.x1,coords.x2, 
                 colour=dbcluster, 
                 fill=dbcluster)) + 
  geom_polygon(data = chulls, 
               aes(coords.x1,coords.x2, 
                   group=dbcluster,
                   fill=dbcluster), 
               alpha = 0.5)  


###Part4 空间自相关 Spatial Autocorrelation(Moran'I LISA)
##读取LondonWard的数据👉
#read the ward data in
LondonWards <- st_read(here::here("data","London-wards-2018_ESRI", "London_Ward.shp"))
plot(LondonWards)

##👉读取London boundaries的数据
LondonWardsMerged <- st_read(here::here("data", 
                                        "statistical-gis-boundaries-london", 
                                        "ESRI",
                                        "London_Ward_CityMerged.shp"))%>%
  st_transform(.,27700)

##👉读取Ward数据
WardData <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv", 
                     na = c("NA", "n/a")) %>% 
  janitor::clean_names()

##👉合并ward与boundaries数据 只保留邮编，名称和GCSE2014年最高平均得分
LondonWardsMerged <- LondonWardsMerged %>% 
  left_join(WardData, 
            by = c("GSS_CODE" = "new_code"))%>%
  distinct(GSS_CODE, ward_name, average_gcse_capped_point_scores_2014)

st_crs(LondonWardsMerged)

tmap_mode("view")
tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaques) +
  tm_dots(col = "blue")

##👉只保留在London内部的“纪念标牌”
BluePlaquesSub <- BluePlaques[LondonWardsMerged,]

tm_shape(LondonWardsMerged) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(BluePlaquesSub) +
  tm_dots(col = "blue")

##👉空间自相关分析需要使用连续变量，因此需要对各Ward内部纪念标牌数量进行统计
points_sf_joined <- LondonWardsMerged%>%
  st_join(BluePlaquesSub)%>%#将两个dataframe合并
  add_count(ward_name)%>%#统计wardname中各observation的数量
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%#计算出各area的面积
  #then density of the points per ward
  mutate(density=n/area)%>%#计算各ward中纪念标版的密度（数量/面积）
  #select density and some other variables 
  dplyr::select(density, ward_name, gss_code, n, average_gcse_capped_point_scores_2014)

points_sf_joined<- points_sf_joined %>%                    
  group_by(gss_code) %>%  ##根据邮编进行分类，得到所有Ward的dataframe       
  summarise(density = first(density),##永远记住 group_by()+summarise/summary()无论python&R
            wardname= first(ward_name),
            plaquecount= first(n))

tm_shape(points_sf_joined2) +
  tm_polygons("density",
              style="jenks",
              palette="PuOr",
              midpoint=NA,
              popup.vars=c("wardname", "density"),
              title="Blue Plaque Density")

##通过快速view可以看到，图中显示London中心的纪念标牌密度更高，其他区域较为平均
##进一步通过Moran's I确认Spatial Autocorrelation
##首先，确认空间权重矩阵Wij👉
##👉第一步，计算出所有Ward的质心中心
coordsW <- points_sf_joined%>%
  st_centroid()%>%
  st_geometry()

plot(coordsW,axes=TRUE)

##👉第二步，计算简单二进制空间矩阵，邻近方式为queen
#create a neighbours list

LWard_nb <- points_sf_joined %>%
  poly2nb(., queen=T)

#plot them
plot(LWard_nb, st_geometry(coordsW), col="red")
#add a map underneath
plot(points_sf_joined$geometry, add=T)

Lward.lw <- LWard_nb %>%
  nb2listw(., style="C")

head(Lward.lw$neighbours)

##其次，计算空间自相关，以①Moran's I为例👉
I_LWard_Global_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  moran.test(., Lward.lw)

I_LWard_Global_Density

##计算空间自相关，②Geary's C
C_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  geary.test(., Lward.lw)

C_LWard_Global_Density

##计算空间自相关，③Getis Ord General G（可以说明是高value的地区集聚还是低value地区
##集聚。if G > expected 则高value集聚。if G < expected 则低value集聚
G_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  globalG.test(., Lward.lw)

G_LWard_Global_Density

#结论👉
#The Moran’s I statistic = 0.67 (remember 1 = clustered, 0 = no pattern, -1 = 
#dispersed) which shows that we have some distinctive clustering

#The Geary’s C statistic = 0.41 (remember Geary’s C falls between 0 and 2; 
#1 means no spatial autocorrelation, <1 - positive spatial autocorrelation or 
#similar values clustering, >1 - negative spatial autocorreation or dissimilar 
#values clustering) which shows that similar values are clustering

#The General G statistic = G > expected, so high values are tending to cluster.

##👉最后我们可以为每一个Ward计算local Moran's I和Getis，找到各个wards中的重点hotpot
#use the localmoran function to generate I for each ward in the city

I_LWard_Local_count <- points_sf_joined %>%
  pull(plaquecount) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

I_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

#what does the output (the localMoran object) look like?
slice_head(I_LWard_Local_Density, n=5)

points_sf_joined <- points_sf_joined %>%
  mutate(plaque_count_I = as.numeric(I_LWard_Local_count$Ii))%>%
  mutate(plaque_count_Iz =as.numeric(I_LWard_Local_count$Z.Ii))%>%
  mutate(density_I =as.numeric(I_LWard_Local_Density$Ii))%>%
  mutate(density_Iz =as.numeric(I_LWard_Local_Density$Z.Ii))

##输出图像👉
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)#手动设计break
MoranColours<- rev(brewer.pal(8, "RdGy"))#创建调色板

tm_shape(points_sf_joined) +
  tm_polygons("plaque_count_Iz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, Blue Plaques in London")
###结论表明，在London中心地区，纪念标牌更多的地方，其邻近的纪念标牌也更多

Gi_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localG(., Lward.lw)

head(Gi_LWard_Local_Density)

points_sf_joined <- points_sf_joined %>%
  mutate(density_G = as.numeric(Gi_LWard_Local_Density))

GIColours<- rev(brewer.pal(8, "RdBu"))

#now plot on an interactive map
tm_shape(points_sf_joined) +
  tm_polygons("density_G",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, Blue Plaques in London")
###结论同上


##其他变量，以AVE_GSCE_socre为例
slice_head(points_sf_joined, n=2)

###
Datatypelist <- LondonWardsMerged %>% 
  st_drop_geometry()%>%
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist
###👆一种方便查看dataframe中各种variable类型的代码

I_LWard_Local_GCSE <- LondonWardsMerged %>%
  arrange(GSS_CODE)%>%
  pull(average_gcse_capped_point_scores_2014) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

points_sf_joined <- points_sf_joined %>%
  arrange(gss_code)%>%
  mutate(GCSE_LocIz = as.numeric(I_LWard_Local_GCSE$Z.Ii))


tm_shape(points_sf_joined) +
  tm_polygons("GCSE_LocIz",
              style="fixed",
              breaks=breaks1,
              palette=MoranColours,
              midpoint=NA,
              title="Local Moran's I, GCSE Scores")

G_LWard_Local_GCSE <- LondonWardsMerged %>%
  dplyr::arrange(GSS_CODE)%>%
  dplyr::pull(average_gcse_capped_point_scores_2014) %>%
  as.vector()%>%
  localG(., Lward.lw)

points_sf_joined <- points_sf_joined %>%
  dplyr::arrange(gss_code)%>%
  dplyr::mutate(GCSE_LocGiz = as.numeric(G_LWard_Local_GCSE))

tm_shape(points_sf_joined) +
  tm_polygons("GCSE_LocGiz",
              style="fixed",
              breaks=breaks1,
              palette=GIColours,
              midpoint=NA,
              title="Gi*, GCSE Scores")