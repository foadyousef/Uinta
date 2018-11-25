library(ggplot2)
library(dplyr)
library(tidyr)
library(plyr)

setwd("E:/Uinta/LANDSAT/tables/")
mylist <- list.files(getwd(), pattern = '.csv', full.names = TRUE)

#df5 <- read.csv("E:/Uinta/LANDSAT/L5-8/L5tot.csv")
#df7 <- read.csv("E:/Uinta/LANDSAT/L5-8/L7tot.csv")
#df8 <- read.csv("E:/Uinta/LANDSAT/L5-8/L8tot_b10.csv")

#df <- data.frame(Date=character(),
#                 Month=character(),
#                 Site=character(),
#                 Month=double())

for (ls in mylist){
  print(ls)
  mytab <- read.csv(ls)
  write.table(mytab, file="E:/Uinta/LANDSAT/tables/tot.csv", append = TRUE, sep=",")
}

# The data
df <- read.csv("tot.csv", header = TRUE)


# Changing Data type
df$Date <- as.POSIXct(df$Date, format="%m/%d/%Y")
df$Month <- factor(df$Month,
                   levels= month.name)
df$Elevation <-factor(round(df$Elevation))

# =============== Use if you want long format ==================
#Use tidyR and dplyr fucntions to cleanup and make Dataframes with a long-format
ndf <- df %>% 
  select(Site1, Site2, Date, Month) %>%
  gather(Site, Temp, Site1:Site2)

ndf$Site <- as.factor(ndf$Site)
#===============================================================
#ggplot
ggplot(data=subset(df, GNIS_Name=="Hoop Lake")) + geom_point(aes(x=Date, y=Temp)) + facet_wrap(~Month)+ labs(title="Surface Temperature In July") + xlab("Time (yrs)") + ylab("Temperature (deg C)") +
  theme(strip.text = element_text(face="bold",size=8),
        strip.background = element_rect(fill="lightblue", colour="black",size=1), text = element_text(face="bold", size=10))

ggsave(filename="E:/Uinta/LANDSAT/Figures/Surface Temp in Jul.png", plot = p, width = 28, height = 20, dpi = 120, units ="cm" )
# ==================== Annual Box Plot =============================
ggplot(data=subset(df, Month=="July")) + geom_boxplot(aes(x=Elevation, y=Temp)) + labs(title="Annual Lake Surface Temperature") + xlab("Time (yrs)") + ylab("Temperature (deg C)") +
  theme(strip.text = element_text(face="bold",size=8),
        strip.background = element_rect(fill="lightblue", colour="black",size=1), text = element_text(face="bold", size=7))

ggsave(filename="E:/Uinta/LANDSAT/Figures/Annual Lake Surface Temperature.png", plot = q, width = 28, height = 20, dpi = 120, units ="cm" )

# Remove rows with empty Temp
df <- na.omit(df)
# Mirror Lake Data
ML <- df[df$Sites == "X43",]
summary(ML$Temp)
# Calcualte p_value
models <- dlply(ML, as.quoted(c("Sites", "Month")), function(ML)lm(Temp ~ Date, data=ML))

mlist <- ldply(models, coef)

# Extract p-value and N
cnt=1
p_val = vector()
n = vector()
for (x in models){
  p_val[cnt] = (summary(x)$coefficients[,4])
  n[cnt] = length(residuals(x))
  cnt=cnt+1
}

# insert them into a df
mlist$p_val <- as.numeric(round(p_val,digits = 3))
mlist$n <- as.numeric(n)

#rename the columns
colnames(mlist) <- c("Site", "Month", "Intercept", "Slope", "P_value", "n")

#Only sig trends with > 30 sample size
sig_months <- subset(mlist, P_value < 0.05 & n > 30)
write.csv(sig_months, file = "MoonLake.csv")

