rent<- read.csv("~/Downloads/houses_to_rent.csv")

rent<-rent[,-1]
rent$rent.amount<-parse_number(as.character(rent$rent.amount))
rent$hoa<-parse_number(as.character(rent$hoa))
rent$property.tax<-parse_number(as.character(rent$property.tax))
rent$fire.insurance<-parse_number(as.character(rent$fire.insurance))
rent$total<-parse_number(as.character(rent$total))

rent$animal<-as.factor(rent$animal)
rent$furniture<-as.factor(rent$furniture)

rent$floor<-ifelse(rent$floor=="-",NA,as.numeric(rent$floor))
rent$floor[is.na(rent$floor)]=1

rent$hoa[is.na(rent$hoa)]=0
rent$property.tax[is.na(rent$property.tax)]=0

rent$city<-as.factor(rent$city)



Q<-quantile(rent$fire.insurance, probs=c(.25, .75))
iqr <- IQR(rent$fire.insurance)
rent <- rent%>% filter(fire.insurance > (Q[1] - 1.5*iqr) & 
                           +                           fire.insurance< (Q[2] + 1.5*iqr))


Q <- quantile(rent$hoa, probs=c(.25, .75))
iqr <- IQR(rent$hoa)
rent <- rent%>% filter(hoa > (Q[1] - 1.5*iqr) &hoa< (Q[2] + 1.5*iqr))

Q <- quantile(rent$rent.amount, probs=c(.25, .75))
iqr <- IQR(rent$rent.amount)
rent <- rent%>% filter(rent.amount> (Q[1] - 1.5*iqr) & 
                         +                           rent.amount< (Q[2] + 1.5*iqr))


Q <- quantile(rent$property.tax, probs=c(.25, .75))
iqr <- IQR(rent$property.tax)
rent <- rent%>% filter(property.tax> (Q[1] - 1.5*iqr) & 
                         +                           property.tax< (Q[2] + 1.5*iqr))


s<-rent[,c(1,2,3,4,5,6,9,10,11,12,13)]
s$city<-as.numeric(s$city)
s$area<-as.numeric(s$area)
s$rooms<-as.numeric(s$rooms)
s$bathroom<-as.numeric(s$bathroom)
s$parking.spaces<-as.numeric(s$parking.spaces)
s$floor<-as.numeric(s$floor)


library(faraway)
round(cor(s),2)

model1<-lm(total~.,data = rent)

vif(model1)


rent<-rent[,-10]
rent<-rent[,-11]

#dummy<-dummyVars(total~.,data=rent)
#new<-data.frame(predict(dummy,newdata=rent))
#new<-cbind(new,rent$total)

intrain<-createDataPartition(y=rent$total,p=0.7,list = FALSE)
train<-rent[intrain,]
test<-rent[-intrain,]

model2<-lm(total~.,data = train)

#model3<-lm("rent$total~city.1+area+rooms+bathroom+parking.spaces+floor+furniture.not.furnished+hoa+property.tax",data = new)

pred1<-predict(model2,newdata = train)

pred2<-predict(model2,newdata = test)


plot(test$total,type = "l",col="red")
lines(pred2,type = "l",col="green")


library(DMwR)
regr.eval(test$total,pred2)
