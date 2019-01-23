#a)
KO<-file.choose()
PEP <- file.choose()
KO<-read.csv(KO)
PEP <- read.csv(PEP)
x=PEP$logreturn
y=KO$logreturn
mean(x)
mean(y)
t.test(x, alternative = "two.sided", mu = 0)
t.test(y, alternative = "two.sided", mu = 0)

#b)
t.test(x,y,alternative = "two.sided",var.equal = TRUE)

#c)
cor(x, y)
cor.test(x,y)

#d)
results <- lm(y~x)
summary(results)
confint(results,"x", level=0.95)

#e)
xx = data.frame(x=c(6,7))
predict(results, xx, interval="predict") 
fitted <- results$fitted.values
fitted
plot(x, fitted)
newdata = data.frame(x)
pred.int = predict(results, newdata, interval="predict") 

#graph
fitted.values = pred.int[,1]
pred.lower = pred.int[,2]
pred.upper = pred.int[,3]
plot(x[1:251],y[1:251])
lines(x[1:251],fitted.values[1:251],col="red",lwd=2)
lines(x[1:251],pred.lower[1:251],lwd=2,col="blue")
lines(x[1:251],pred.upper[1:251],lwd=2,col="blue")