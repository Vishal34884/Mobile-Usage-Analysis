project=read.csv("D:/VISHAL/Mini Project/MOBILE.csv")
print(project)
summary(project)
             
#Sample Extraction
sample=project[1:30,]
print(sample)
View(sample)

sample2=project[31:60,]
print(sample2)

sample3=project[61:90,]
print(sample3)

sample4=project[91:101,]
print(sample4)


#correlation
cor(sample2$ENTERTAINMENT,sample2$DAY)

cor.test(sample2$ENTERTAINMENT,sample2$DAY)

barplot(sample2$ENTERTAINMENT)

#T.test
t.test(sample2$ENTERTAINMENT,sample2$DAY)


#1.is there any relation between day and studying
cor(project$STUDYING,project$DAY,method = "pearson")

#plot
tb1=table(project$STUDYING,project$DAY)
print(tb1)
regions=c("0 hr","1 hr","2 hrs","3 hrs","4 hrs","5 hrs","6 hrs")
colors=c("red","blue","green","yellow","pink","orange","purple","black")
barplot(tb1,xlab = "Mobile usage per day",ylab = "No of Person",col=colors,ylim = c(0,25))
legend("topright",regions,cex=0.6,fill = colors)





#2.IS THERE ANY RELATION BETWEEN ENTERTAINMENT AND DAY
cor(project$ENTERTAINMENT,project$DAY,method = "pearson")

tb2=table(project$ENTERTAINMENT,project$DAY)
print(tb2)

#plot
regions=c("1 hr","2 hrs","3 hrs","4 hrs","5 hrs","6 hrs","7 hrs","8 hrs")
colors=c("red","blue","green","yellow","pink","orange","purple","sky blue")
barplot(tb2,col=colors,main="Entertainment Purpose",xlab="MOBILE USAGE PER DAY",ylab="No of person using mobile for entertainment purpose",axes = TRUE,ylim = c(0,25))
legend("topright",regions,cex=0.5,fill=colors)

#3.is there any relation between Reduce in focus and increase in performance
tb3=table(project$REDUCING.FOCUS,project$PERFORMANCE.INCREASE)
print(tb3)
print(chisq.test(tb3))
barplot(tb3)

#4.For what kind of materials their having most using bar chart
tb4=table(project$GENDER,project$MATERIALS)
print(tb4)
regions=c("Female","Male")
colors=c("green","blue")
barplot(tb4,col = colors,beside = TRUE,xlab = "PURPOSE",ylab = "NO OF PERSON",ylim = c(0,50))
legend("topright", regions, cex = 0.8, fill = colors)


#5.relationship between gender and study using bar chart
tb5=table(project$GENDER,project$STUDYING)
print(tb5)
regions=c("Male","Female")
colors=c("green","blue")
barplot(tb5,col = colors,besides=TRUE,xlab = "No of hours",ylab = "No of person",ylim = c(0,40))
legend("topright",regions,cex=0.8,fill = colors)

#6.relation between which gender using most study and entertainment purpose using bar chart
tb6=table(project$GENDER,project$ENTERTAINMENT)
print(tb6)
colors=c("blue","red")
regions=c("Female","Male")
barplot(tb6,col = colors,beside = TRUE,xlab = "No of hours",ylab = "NO OF PERSON",ylim =c(0,25))
legend("topright", regions, cex = 0.6, fill = colors)





#Regression
day=project$DAY
study=project$STUDYING
entertainment=project$ENTERTAINMENT
data1=lm(day~study+entertainment)
d1=data.frame(study=c(4,5,6),entertainment=c(6,7,8))
result=predict(data1,d1)
result

#plot
barplot(result,xlab = "No of person",ylab = "No of Hours",ylim = c(0,15))

#full attribute prediction
data=data.frame(actual =project$DAY,predicted =predict(data1))
head(data,30)

data1=lm(day~study+entertainment)
d1=data.frame(study,entertainment)
result=predict(data1,d1)
result
barplot(result,xlab = "No of person",ylab = "No of Hours",ylim = c(0,15))
mean(result)
summary(data1)

#Gender
tabl=table(project$GENDER)
tabl

#Reducing focus on studies
tb6=table(project$GENDER,project$REDUCING.FOCUS)
print(tb6)
col=c("red","blue")
regions=c("Female","Male")
barplot(tb6,xlab = "Reducing focus on studies",col=colors)
legend("topleft",regions,cex=0.6,fill = colors)

#performance increases in studies
tb7=table(project$GENDER,project$PERFORMANCE.INCREASE)
print(tb7)
col=c("red","blue")
regions=c("Female","Male")
barplot(tb7,xlab = "Performance increasing in studies without Mobile phone",col=colors,ylim = c(0,60))
legend("topright",regions,cex=0.6,fill = colors)





