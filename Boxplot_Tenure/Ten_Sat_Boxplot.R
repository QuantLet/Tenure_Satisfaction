if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("cowplot")) install.packages("cowplot"); library("cowplot")
if(!require("reshape")) install.packages("reshape"); library("reshape")
if(!require("gridExtra")) install.packages("gridExtra"); library("gridExtra")




sub_box <- subset(sub1, select=c(TENSTA, SATSAL  ,  SATBEN  ,  SATSEC  ,  SATLOC  ,  SATADV  ,  SATCHAL ,  SATRESP ,  SATIND  ,  SATSOC ,FACRANK, AGEGRP ))
g <- length(unique(sub_box$AGEGRP))
names_output <- c(paste0("Age Group ", seq(from=25,to=70,by = 5)))
sub_box <- subset(sub_box, TENSTA!="L")
sub_box$TENSTA <- as.factor(as.character(sub_box$TENSTA))

p <- list()
b <- 1
for (j in 1:10) {
  a <- b
  g2 <- seq(from=25,to=70,by = 5)
  p[[j]] <- ggplot(data=sub_box[sub_box$AGEGRP==g2[j],], aes_string(x="TENSTA", y="SATCHAL")) + # Specify dataset, input or grouping col name and Y
    geom_boxplot(notch=F) + guides(fill=FALSE) + # Boxplot by which factor + color guide
    theme(panel.background = element_rect(fill = "white", colour="black", color = "black"),axis.title.y = element_text(face="bold", size=12)) +
    stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
    xlab("Tenure status") +
    ylab(names_output[b])
  b <- a+1
}

do.call(grid.arrange, c(p, ncol=3))

# Mean/Median satisfaction by age-group
satis_age <- group_by(sub1,AGEGRP) %>% summarise(mean=mean(SATADV), median=median(SATADV))
satis_age


