hsb <- read.csv("~/R_Script/BMI_5352/hsb2.csv")
str(hsb)
summary(hsb)

boxplot(hsb$write, hsb$read,
        main = "grades comparision by category",
        ylim = c(25, 80),
        horizontal = FALSE,
        notch = TRUE,
        names = c("writing", "reading"))

t.test(hsb$write, hsb$read, alternative = ("two.sided"), paired = TRUE)
hsb$write - hsb$read -> diff.idx
summary(diff.idx)
sd(diff.idx)

gra = hsb[,c(7:8)]
summary(gra)

attach(gra)
plot(read, write, main = "Relationship between grades for reading and writing", xlab = 'reading', ylab = 'writing')
abline(lm(write ~ read), col="red") # regression line (y~x)
lines(lowess(read,write), col="blue") # lowess line (x,y)

cor(read, write, method = 'pearson')
lm(write ~ read)
detach(gra)

epa <- read.delim("~/R_Script/BMI_5352/epa_final.txt")
head(epa)
summary(epa)

which(epa$transmission_desc=="Automatic") -> auto.idx
which(epa$transmission_desc=="Manual") -> man.idx

hist(epa$city_mpg[auto.idx])
hist(epa$city_mpg[man.idx])

boxplot(epa$city_mpg[auto.idx], epa$city_mpg[man.idx], 
        main = "mpg Comparision",
        horizontal = FALSE,
        notch = TRUE,
        names=c("Auto", "Manual"))

summary(epa$city_mpg[auto.idx])
summary(epa$city_mpg[man.idx])
sd(epa$city_mpg[auto.idx])
sd(epa$city_mpg[man.idx])
2*(1-pt(-4.15, df=27))

t.test(epa$city_mpg[man.idx],epa$hwy_mpg[man.idx], alternative = ("two.sided"), paired = TRUE)
epa$city_mpg[man.idx] - epa$hwy_mpg[man.idx] -> diffmpg.idx
summary(diffmpg.idx)
sd(diffmpg.idx)

bodyl <- read.delim("~/R_Script/BMI_5352/bdims.txt")
bod = bodyl[,c(10,24)]
summary(bod)
bod2 = bodyl[,c(14,23)]
summary(bod2)

attach(bod)
plot(hgt,sho.gi, main = "Relationship between height and shoulder girth", xlab = 'height', ylab = 'shoulder girth')
abline(lm(sho.gi ~ hgt), col="red") # regression line (y~x)
lines(lowess(hgt,sho.gi), col="blue") # lowess line (x,y)
plot(lm(sho.gi ~ hgt), data = bod, pch=507, which = 1)
cor(hgt, sho.gi, method = 'pearson')
lm(sho.gi ~ hgt)

fit <- glm(hgt ~ sho.gi, data = bod)
summary(fit)
detach(bod)

attach(bod2)
plot(wgt,hip.gi, main = "Relationship between weight and hip girth", xlab = 'weight', ylab = 'hip girth')
abline(lm(hip.gi ~ wgt), col="red") # regression line (y~x)
lines(lowess(wgt,hip.gi), col="blue") # lowess line (x,y)

cor(wgt, hip.gi, method = 'pearson')
lm(hip.gi ~ wgt)

fit2 <- glm(hip.gi ~ wgt, data = bod)
summary(fit2)
detach(bod2)


