rm(list = ls())

pacman::p_load(tidyverse, sf, raster, ggspatial, ggplot2,cowplot,stars)
crs_84 <- st_crs("EPSG:4326")  ## WGS 84 大地坐标
crs_al <- st_crs("+proj=aea +lat_1=25 +lat_2=47 +lon_0=105") ## Albers Equal Area Conic投影
ndvi <- raster("C:/Users/Admin/Desktop/guangdong_DEM/guangdong_China.tif")
df=as.data.frame(ndvi,xy=T)
colnames(df)=c("x","y","LandCover")#数据转化为dataframe
china_all <-
  sf::st_read("https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json") %>%
  st_transform(crs_84)
shp2 <- sf::read_sf("C:/Users/Admin/Desktop/guangdong/guangdong.json")
p1 <-
  ggplot() +
  geom_sf(size = .2, fill = "transparent", color = "black", data = china_all) +
  geom_sf(data=shp2,fill="NA",size=0.2,color="red")+
  theme_void()
p1


pacman::p_load(geoviz,tidyverse, sf, raster, ggspatial, ggplot2,cowplot,stars,terra,rasterVis,rgdal,RColorBrewer)
#DEM 颜色
crs_84 <- st_crs("EPSG:4326")  ## WGS 84 大地坐标
colormap <-  colorRampPalette(rev(brewer.pal(9,'RdYlGn')),1)(32)
# scales::show_col(colormap)
china_all <-
  sf::st_read("https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json") %>%
  st_transform(crs_84)
shp <- sf::read_sf("C:/Users/Admin/Desktop/guangdong_DEM/guangdong.json")
shp2 <- sf::read_sf("C:/Users/Admin/Desktop/guangdong_DEM/qyd.json")
shp3 <- sf::read_sf("C:/Users/Admin/Desktop/guangdong_DEM/conbine_for_3.json")%>%
  st_transform(crs_84)
lat=23
long=113
square_km=1000
dem<-mapbox_dem(lat,long,square_km,
                api_key="pk.eyJ1IjoiYmVueXNmIiwiYSI6ImNrczBtdWE0ajBwNjcydnBqMjRyZDdsOXkifQ.sUcMdooE7b9uQqzfrnWdSQ")
guilin<- raster::mask(dem,shp) %>% # 将地图与DEM数据结合
  crop(.,extent(shp))
df_guilin<- as.data.frame(as(guilin,"Raster"),xy=T) #格式转换

p2 <- ggplot() + 
  geom_sf(data = shp,fill="NA") + 
  geom_tile(data=df_guilin,aes(x=x,y=y,fill=layer),show.legend = F)+
  scale_fill_gradientn(colours =colormap,na.value="transparent",name="DEM",breaks = c(50,100,150,200,250,300))+
  labs(x=NULL,y=NULL)+
  geom_sf(data = shp3,fill = "NA",color="black") +
  geom_sf(data = shp2,fill = "NA",color = "darkred",size =1.5) +
  annotation_scale(location = "bl",text_face = "bold",pad_x = unit(1, "cm"),pad_y = unit(0.5, "cm")) +
  annotation_north_arrow(location="tr",style=north_arrow_fancy_orienteering)+  # 添加指北针
  theme_minimal()+
  theme_bw()+
  annotate('point',x=108,y=22,color='darkred',size=1.5)+
  annotate('text',x=108.3,y=22,color='black',label='site',size=4)+
  annotate('rect',xmin=107.5,xmax = 108.5,ymin = 21,ymax = 21.5,fill='NA',color="black")+
  annotate('text',x=109,y=21.3,color='black',label='study area',size=4)+
  guides(fill = guide_colorbar(ticks.colour = "black",frame.colour = "black"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        legend.position= "bottom",
        legend.background = element_blank(),
        legend.key.width = unit(30.8, "mm"),
        legend.key.height = unit(4, "mm")) # 左边边缘距离)

ggdraw()+draw_plot(p1,x=-0.35,scale=0.3)+draw_plot(p2,x=0.135,scale=0.7)+
  geom_segment(aes(x=0.19,y=0.47,xend=0.32,yend=0.23),size=0.5)+ ## 设置两根线的起点和终点
  geom_segment(aes(x=0.19,y=0.47,xend=0.32,yend=0.805),size=0.5) ## 设置两根线的起点和终点
ggsave("C:/Users/Admin/Desktop/guangdong/map.pdf",dpi=300)#保存图片


