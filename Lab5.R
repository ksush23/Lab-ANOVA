library(plotrix)
library(rcompanion)
require(DescTools)
suicide <- read.csv("D:/suicide.csv")

suicides = suicide[,c("suicides.100k.pop")]
generation = suicide[,c("generation")]
gdp = suicide[,c("gdp_for_year....")]

plot(gdp, suicides, pch = as.numeric(generation), xlab = "gdp", ylab = "suicides")


generation = c(generation, "Means")
suicides = c(suicides, 1)
stripchart(suicides~generation, xlab="Generations", ylab="Number of suicides in 100k", 
           col=c("orange","red", "skyblue", "blue", "green", "brown", "white"), vertical = TRUE, 
           group.names=c("Boomers", "G.I. Generation", "Generation X", "Generation Z", "Millenials", "Silent", "Means"))
m = tapply(suicides, generation, mean)

points(7, m[1], col="orange", pch=19)
points(7, m[2], col="red", pch=19)
points(7, m[3], col="skyblue", pch=19)
points(7, m[4], col="blue", pch=19)
points(7, m[5], col="green", pch=19)
points(7, m[6], col="brown", pch=19)

meansSuicides <- tapply(suicides, generation, mean)
SDsSuicides <- tapply(suicides, generation, FUN = sd)
for(i in 0:6){
  segments(y0 = 0.9+i,
           x0 = meansSuicides[1+i],
           y1 = 1.1+i,
           x1 = meansSuicides[1+i], lwd = 3, lend = "square")
}
#for(i in 0:6){
#  arrows(x0 = meansSuicides[1+i]-SDsSuicides[1+i], 
#         y0 = 1+i,
#         x1 = meansSuicides[1+i]+SDsSuicides[1+i],
#         y1 = 1+i, lend = "square", code = 0)
#}

#medians <- tapply(suicides, generation, mean)
#points(c(1, 2), medians, pch="-", cex=3, col="yellow")

summary(aov(suicides.100k.pop ~ generation, data = suicide))

M <- lm(suicides ~ generation, data = suicide)

summary(M)

CI<-groupwiseMean(suicides.100k.pop~generation, data = suicide, conf=(0.95)^(1/6))
plotCI(1:6,y=CI$Mean,ui=CI$Trad.upper,li=CI$Trad.lower, xlab=" ",ylab="counts",xaxt="n")
# виводимо горизонтальну вiсь з позначеннями рiвнiв фактора:
axis(1,at=1:6,labels=levels(CI$spray))

contrasts(suicide$generation)
contrasts(suicide$generation) <- contr.sum(n = 6)
M3 <- lm(suicides~generation)
aov1 = aov(suicides~generation)
summary(M3)

ScheffeTest(aov1)
#confint(aov1, level = 0.90)

contrasts(suicide$generation) <- c(1, 0, 0, 0, 0, -1)
M5 <- lm(suicides ~ generation)
summary(M5)
aov2 <- aov(suicides ~ generation)
#confint(aov1, level = 0.90)

ScheffeTest(aov2)