## Input: m.csv : A land cover changes table. The last tree coloumns represent RGB colour
## Analysis: 
## Author: Agung Wahyudi
## Date first created: 06/08/2018
## 
## About the circular diagram
## https://jokergoo.github.io/circlize_book/book/index.html
## https://gjabel.wordpress.com/2016/05/18/updated-circular-plots-for-directional-bilateral-migration-data/




## 0. START ============================================================
rm(list=ls())
if(!is.null(dev.list())) dev.off()

library(circlize)
library(migest)
library(dplyr)



## 1. LOAD DATA ============================================================
## prepare your data as exemplified in the m.csv. 
## The last three coloumns are Red, Green, Blue color 
## with each row corrends to colour of each class
m   <- read.csv('~/CircularPlot/Input/m.csv')
n   <- dim(m)[1]                                # length of the classes (i.e. country)
df1 <- m[, c(1,2, seq(from=n+3,to=n+5,by=1))]   # identifier and RGB only

m <- m[,-(1:2)]/1e04                            # scale down the size (unit in 10.000 hectare) | ADJUST THIS
m <- as.matrix(m[,c(1:n)])                      # ?? convert to matrix??
dimnames(m) <- list(orig = df1$country, dest = df1$country) # ?? df1$country == work like table in matlab. a.[heading]


#Sort order of data.frame and matrix for plotting in circos
df1 <- arrange(df1, order)
df1$country <- factor(df1$country, levels = df1$country)
m <- m[levels(df1$country),levels(df1$country)]


### Define ranges of circos sectors and their colors (both of the sectors and the links)
df1$xmin <- 0
df1$xmax <- rowSums(m) #+ colSums(m) #
n <- nrow(df1)
df1$rcol<-rgb(df1$r, df1$g, df1$b, max = 255)
df1$lcol<-rgb(df1$r, df1$g, df1$b, alpha=200, max = 255)



## 2. PLOT DATA ============================================================
## prepare your data as exemplified in the m.csv. 
## The last three coloumns are Red, Green, Blue color 
## with each row corrends to colour of each class
### Plot sectors (outer part)
par(mar=rep(0,4)) #what is mar, why is zero is repeated 4 times?
circos.clear()

### Basic circos graphic parameters
circos.par(cell.padding=c(0,0,0,0), track.margin=c(0,0.15), start.degree = 90, gap.degree =4)

### Sector details
circos.initialize(factors = df1$country, xlim = cbind(df1$xmin, df1$xmax)) #cbind combine row and col; XLim is the total land per class


### Plot sectors
circos.trackPlotRegion(ylim = c(0, 1), factors = df1$country, track.height=0.1,
                       #panel.fun for each sector
                       panel.fun = function(x, y) {
                         #select details of current sector
                         name = get.cell.meta.data("sector.index")
                         i = get.cell.meta.data("sector.numeric.index")
                         xlim = get.cell.meta.data("xlim")
                         ylim = get.cell.meta.data("ylim")
                         
                         #text direction (dd) and adjusmtents (aa)
                         theta = circlize(mean(xlim), 1.3)[1, 1] %% 360
                         dd <- ifelse(theta < 90 || theta > 270, "clockwise", "reverse.clockwise")
                         aa = c(1, 0.5)
                         if(theta < 90 || theta > 270)  aa = c(0, 0.5)
                         
                         #plot country labels
                         circos.text(x=mean(xlim), y=1.7, labels=name, facing = dd, cex=1,  adj = aa)
                         
                         #plot main sector
                         circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2], ytop=ylim[2], 
                                     col = df1$rcol[i], border=df1$rcol[i])
                         
                         #blank in part of main sector
                         circos.rect(xleft=xlim[2]-diag(m)[i], ybottom=ylim[1], xright=xlim[2], ytop=ylim[1]+0.3, 
                                     col = "white", border = "white")
                         
                         #white line all the way around
                         circos.rect(xleft=xlim[1], ybottom=0.35, xright=xlim[2], ytop=0.32, col = "white", border = "white")
                         
                         #plot axis
                         circos.axis(labels.cex=0.6, direction = "outside", major.at=seq(from=0,to=floor(df1$xmax)[i],by=5), 
                                     minor.ticks=1, labels.away.percentage = 0.15)
                       })

### Plot links (inner part)
### Add sum values to df1, marking the x-position of the first links
### out (sum1) and in (sum2). Updated for further links in loop below.

#df1$sum1 <- rowSums(m)

df1$sum1 <- numeric(n) #rowSums(m) - diag(m)
df1$sum2 <- rowSums(m) - diag(m)
#df1$chg  <- rowSums(m) - diag(m)

### Create a data.frame of the flow matrix sorted by flow size, to allow largest flow plotted first
diag(m) <- 0 #remove the non-changes land values

df2 <- cbind(as.data.frame(m),orig=rownames(m),  stringsAsFactors=FALSE)
df2 <- reshape(df2, idvar="orig", varying=list(1:n), direction="long",
               timevar="dest", time=rownames(m),  v.names = "m")
df2 <- arrange(df2,desc(m))

### Keep only the largest flows to avoid clutter
df2 <- subset(df2, m > quantile(m,0.6))



### Plot links
for(k in 1:nrow(df2)){
  #i,j reference of flow matrix
  i<-match(df2$orig[k],df1$country)
  j<-match(df2$dest[k],df1$country)
  
  #plot link
  circos.link(sector.index1=df1$country[i], point1=c(df1$sum1[i], df1$sum1[i] + abs(m[i, j])),
              sector.index2=df1$country[j], point2=c(df1$sum2[j], df1$sum2[j] + abs(m[i, j])),
              rou = 0.73, col = df1$lcol[i])
  #abs absolute value of changes
  
  #update sum1 and sum2 for use when plotting the next link
  df1$sum1[i] = df1$sum1[i] + abs(m[i, j]) # so the next link move slightly from the previous
  df1$sum2[j] = df1$sum2[j] + abs(m[i, j])
}







## 3. EXPORT DATA ============================================================
dev.copy2pdf(file = "~/CircularPlot/Output/subUrb_circ_plot.pdf", height=10, width=10 )
dev.print(png, file = "~/CircularPlot/Output/subUrb_circ_plot.png", height=10, width=10, units = "in", res=500)
# file.show("~/CircularPlot/Output/subUrb_circ_plot.png")
dev.off()
