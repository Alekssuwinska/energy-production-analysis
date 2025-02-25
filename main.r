rm(list=ls())
library('reshape')
library('ggplot2')
library('patchwork')
theme_set(theme_grey())

df <- read.table(
    file = file.path('data','AggregatedGenerationPerType_pl_de.csv'),
    sep=";",dec=".",header=T)
print(head(df))

# filtrowanie do okreslonego kraju [PL|DE]
df <- df[df$Country=="PL",]

# aktywne zrodla 
df <- df[,c(T,T,apply(df[,-(1:2)],2,sum)>0)]

# formatowanie stempla czasowego 
df$DateTime <- as.POSIXct(df$DateTime,tz="GMT")
# sortowanie po czasie 
df <- df[order(df$DateTime),]
print(head(df))

d <- data.frame(
  Date = as.Date(paste0(substr(as.character(df$DateTime),1,7),"-01")),
  Wind = df$Wind.Onshore, 
  Solar = df$Solar,
  Fossil = df$Fossil.Brown.coal.Lignite + df$Fossil.Coal.derived.gas + df$Fossil.Hard.coal,
  OilGas = df$Fossil.Gas + df$Fossil.Oil,
  Water = df$Hydro.Pumped.Storage + df$Hydro.Run.of.river.and.poundage + df$Hydro.Water.Reservoir, 
  Other = df$Biomass + df$Other + df$Other.renewable
)

d <- d[d$Date>=as.Date("2022-01-01"),]
d <- d[d$Date<=as.Date("2024-10-31"),]
d <- reshape::melt(d,id.vars="Date")
d <- aggregate(value~Date+variable,data=d,FUN=sum)


d$variable_img2 <- factor(d$variable,levels = c("Other","Water","Solar","Wind","OilGas","Fossil"))
custom_colors <- c(
"Wind" = "green", "Solar" = "yellow", "Water" = "blue", 
"Fossil"="black", "OilGas"="brown", "Other"="red"
)


img1 <- (ggplot(d, aes(x=Date, y=value, fill=variable))
        +geom_area(colour="black")
        + scale_y_continuous(labels = function(x) format(x / 1e6, scientific = FALSE))
        + xlab("Data")
        + ylab("[TWh]")
        +ggtitle("Zagregowana dobowa produkcja energii w podziale na zrodla - ustawienia domysle")
        +theme_bw() 
        +theme(legend.position = "bottom",
               legend.title = element_blank())
        + guides(fill = guide_legend(nrow = 1))
)


img2 <- img1 + aes(fill = d$variable_img2) + scale_fill_manual(values = custom_colors)

pdf(file='Zadanie3_Suwinska.pdf', width = 10,  height = 9)

#x11(width = 10, height = 9);
print(img1 / img2)

dev.off()
