if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("cowplot")) install.packages("cowplot"); library("cowplot")
if(!require("reshape")) install.packages("reshape"); library("reshape")
if(!require("gridExtra")) install.packages("gridExtra"); library("gridExtra")


# Percent of Tenure in U.S. 
ten_time <- read.csv2("percentTenure.csv") 
plot_Tenure <- ggplot(data=ten_time, aes(x=Year, y=Tenure))+
  geom_line()+
  geom_point(shape=18) +
  theme_cowplot(font_size = 16) +
  scale_y_continuous(name="% of Tenure", limits=c(25, 100))




# Sum of Doctoral Degrees in U.S. 
doc_degree <- read.csv2("Total_Doctor_Degree.csv")
doc_degree[,2:3] <- doc_degree[,2:3]/1000 # Divide by 1000 to have the same scale as the plot above
doc_melt <- melt(doc_degree,id="Year")
plot_PhD <- ggplot(data=doc_melt, aes(x=Year, y=value, col= variable,shape=variable))+
  geom_line()+
  geom_point() +
  theme_cowplot(font_size = 16) +
  #scale_color_viridis_d(option = "D") +
  scale_y_continuous(name="Sum of Doctor Degrees (x1000)", limits=c(0, 100))+
    guides(fill = FALSE, color = FALSE, linetype = FALSE, shape = FALSE) # Dot = male, triangle = female

grid.arrange(plot_Tenure,plot_PhD,ncol=1, nrow=2)
