setwd("D:/DATASET")
house=read.table("train.csv",sep=",",header=TRUE)
library(ggplot2)
library(dplyr)
library(patchwork)
library(tidyr)
library("plotly")
library("ggcorrplot")
library("reshape2")
library("ggcorrplot")

#pre processing data
house$TotArea= house$TotalBsmtSF+house$GrLivArea
#Korelasi Plot
df4 <- data.frame(house$SalePrice, house$LotArea,house$TotArea,
                  house$BsmtFinSF1,house$BsmtFinSF2,house$BsmtUnfSF,house$TotalBsmtSF,house$X1stFlrSF,
                  house$X2ndFlrSF,house$LowQualFinSF,house$GrLivArea,house$BsmtFullBath,house$BsmtHalfBath,
                  house$FullBath,house$HalfBath,house$BedroomAbvGr,house$KitchenAbvGr,house$TotRmsAbvGrd,house$Fireplaces,
                  house$GarageCars,house$GarageArea,house$WoodDeckSF,house$OpenPorchSF,house$EnclosedPorch,house$X3SsnPorch,house$ScreenPorch,
                  house$PoolArea,house$MiscVal)

COR <- cor(as.matrix(df4[,1]), as.matrix(df4[,-1]))
COR=data.frame(COR)
data2=COR%>%gather(key="Variabel",value="val")
data2$CleanVariabel=gsub("house.","",data2$Variabel)

cor=ggplot(data2,aes(x=reorder(CleanVariabel,-val),y=val))+geom_bar(stat="identity",fill="#677836")+theme_light()+
  theme(axis.text.x = element_text(angle=30,hjust=1),axis.text = element_text(colour="#433036"),plot.title = element_text(face='bold'))+labs(title="Bar Chart",
                                                          subtitle="Korelasi antara SalePrice dengan Variabel Lainnya",
                                                           x="Variabel",
                                                           y="Korelasi",caption="Sumber: House Price (Kaggle)")+
  geom_text(aes(label=round(val,2)),position=position_dodge(width=0.9),vjust=-0.6,size=3)
cor
ggplotly(cor)

#histogram 
mean_price <- house %>% 
  pull(SalePrice) %>% 
  mean()
mean_price=round(mean_price,0)
med_price <- house %>% 
  pull(SalePrice) %>% 
  median()
med_price=round(med_price,2)
hist=ggplot(house, aes(x = SalePrice)) + 
  geom_histogram(bins=30,fill="#677836",colour="#678436")+theme_light()+
  theme(axis.text = element_text(colour="#677136"),plot.title = element_text(face='bold'))+
  labs(title="Histogram",subtitle="Persebaran data Variabel SalePrice (Harga Jual)",x="Harga Jual ($)",y="Jumlah",caption="Sumber: House Price (Kaggle)")+
  geom_vline(xintercept = mean_price,col = "#A52300",lwd = 0.5)+
  geom_text(aes(x=mean_price+0.77e+05, label=paste0("Mean: ",mean_price), y=170),size=3.4,colour="#A52300")+
  geom_vline(xintercept = med_price,col = "#422300",lwd = 0.5)+
  geom_text(aes(x=med_price+0.74e+05, label=paste0("Median: ",med_price), y=188),size=3.4,colour="#422300")
hist
ggplotly(hist)

##PreProcessing For Count Plot
house[house== "2Types"] <- "Banyak Tipe"  
house[house=="Attchd"]<-"Menempel Rumah"
house[house=="Basment"]<-"Garasi Bawah Tanah"
house[house=="BuiltIn"]<-"Built-In"
house[house=="Detchd"]<-"Tidak Menempel Rumah"
house$GarageType=as.factor(house$GarageType)
GarageLab=c("Menempel Rumah","Tidak Menempel Rumah","Built-In","Garasi Bawah Tanah","CarPort","Banyak Tipe","Tidak ada")

df1=house %>% filter(SalePrice<180921)%>%
  count(GarageType)
df2=house %>% filter(SalePrice>180921)%>%
  count(GarageType)
df3=c(rep("Kurang dari Rata-Rata",7),rep("Lebih dari Rata-Rata",6))
dim(df3)=c(length(df3),1)
df3=as.data.frame(df3)
tos=rbind(df1,df2)
tos[,3]=df3
#Grouped Count Plot 
cp=ggplot(tos,mapping=aes(reorder(GarageType,-n),n,fill=V1)) +
  geom_bar(position = "dodge",stat = "identity")+theme_light()+scale_fill_manual(values = c("#A52300","#A5BE00"))+
  scale_x_discrete(labels=GarageLab)+
  labs(title="Grouped Count Plot",
       subtitle="Jumlah Rumah berdasarkan Tipe Garasi dan Harga Rumah",
       x="Tipe Garasi",
       y="Jumlah Rumah",
       caption="Sumber: House Price (Kaggle)")+theme(legend.justification = "top",plot.title = element_text(face='bold'))+guides(fill=guide_legend("Harga Rumah (Rata-Rata: $180921)"))
cp+geom_text(aes(label=n),position=position_dodge(width=0.9),vjust=-0.5)
cp
ggplotly(cp)

#scatter plot
house[house== "Abnorml"] <- "Abnormal"  
house[house=="Alloca"]<-"Allocation"
house$SaleCondition=as.factor(house$SaleCondition)
scatter1=ggplot(house,aes(x=TotArea,y=SalePrice,col=SaleCondition))+geom_point(alpha=0.7)+
  labs(x="Total Area (m^2)",
       y="Harga Jual ($)",
       caption="Sumber: House Price (Kaggle)")+theme_light()+theme(legend.position = "bottom")+geom_hline(yintercept=4e+05, linetype="dashed", color = "red")+
  geom_vline(xintercept=6000, linetype="dashed", color = "red")+scale_color_manual(values = c("#679436","#A4F2FA","#064789","#427AA1","#A5BE00","#A52300"))+
  guides(col=guide_legend("Kondisi Penjualan"))
hist_x=house %>%
  ggplot(aes(x = TotArea)) +
  geom_histogram(bins = 30,fill="#812300",colour="#3F3E36")+theme_minimal()+theme(axis.text = element_blank(),plot.title = element_text(face='bold'),axis.title = element_blank())+
  labs(title="Scatter Plot dan Histogram",
       subtitle="Harga Jual vs Total Area (Ruang Bawah Tanah dan Bangunan di Atas Permukaan Tanah)")
hist_y=house %>%
  ggplot(aes(x = SalePrice)) +
  geom_histogram(bins = 30,fill="#812300",colour="#3F3E36")+coord_flip()+theme_minimal()+theme(axis.text = element_blank(),axis.title = element_blank())
#combine scatter and hist
com=hist_x+plot_spacer()+scatter1+hist_y + 
  plot_layout(
    ncol = 2, 
    nrow = 2, 
    widths = c(6, 1),
    heights = c(1, 4))
com
ggplotly(com)

#violin plot ttg 
med_price <- median(house$SalePrice)
vio <- ggplot(house) +
  aes(x = MSZoning, y = SalePrice, fill = MSZoning) +
  geom_violin(adjust = 1L, scale = "area", width = 1.3, trim=F) +
  scale_fill_manual(values= c("#264653","#2a9d8f","#f4a261","yellow","#e76f51")) +
  theme_minimal()+
  geom_boxplot(fill='white', width=0.5) + geom_jitter(width = 0.2) +
  stat_summary(geom = 'point', fun = mean, shape=23, size=2, fill = "red") + 
  guides(fill=F)+
  geom_hline(yintercept=med_price, linetype="dashed", color = "red")+
  labs(title ="Violin Plot",
       subtitle = "Distribusi berdasarkan Klasifikasi Daerah Rumah",
       x = "Tipe Daerah",
       y = "Harga Jual",
       caption = "Sumber: House Price (Kaggle)")+
  theme(axis.text.x = element_text(hjust=1),plot.title = element_text(face='bold', size = 16))
vio
ggplotly(vio)


#tile plot
tile <- ggplot(as.data.frame(table(house$SaleType, 
                                   house$SaleCondition))) +
  geom_tile(aes(x = Var1, y = Var2, fill = Freq)) +
  geom_text(aes(x = Var1, y = Var2, label = Freq), 
            color = "white") +
  labs(title ="Tiles Plot",
       subtitle = "Pengelompokan Rumah berdasarkan Jenis dan Kondisi Penjualan",
       x = "Jenis Penjualan",
       y = "Kondisi Penjualan",
       caption = "Sumber: House Price (Kaggle)")+theme_minimal()+
  theme(axis.text.x = element_text(hjust=1),plot.title = element_text(face='bold', size = 16))
tile
ggplotly(tile)


#correlation plot
house.corr <- house[,c(47,62,63,39,44,50,55,57,45,68,51,5,48,38,52,71,72,70,37,49,76,46,69,53)]
corr <- round(cor(house.corr), 1)
p.mat <- cor_pmat(house.corr)
head(p.mat[,1:4])
corplot <- ggcorrplot(corr,hc.order = T, outline.col = "white",lab = F,insig = "blank", type = "lower")+
  labs(title ="Correlation Plot",
       subtitle = "Korelasi Antar Faktor yang Mempengaruhi Harga Rumah",
       caption = "Sumber: House Price (Kaggle)")

corplot
ggplotly(corplot)

