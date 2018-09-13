setwd("C:/Questionnaire_db/mydir")
Mydata = read.csv("BioPocket.csv", header = TRUE)
#creating the frequency tables of age and motivations 
tblage <- Mydata[c(17,5, 6, 7, 8, 9, 10, 11, 12)]
colnames(tblage) <- c("Age", "M1", "M2", "M3", "M4","M5", "M6", "M7", "M8")
for (h in 2:9){
  for(i in 1:74){
    
    if(tblage[i,h]==1){tblage[i,h]<-8}
    else if(tblage[i,h]==2){tblage[i,h]<-7}
    else if(tblage[i,h]==3){tblage[i,h]<-6}
    else if(tblage[i,h]==4){tblage[i,h]<-5}
    else if(tblage[i,h]==5){tblage[i,h]<-4}
    else if(tblage[i,h]==6){tblage[i,h]<-3}
    else if(tblage[i,h]==7){tblage[i,h]<-2}
    else if(tblage[i,h]==8){tblage[i,h]<-1}
    
    
  }}

k <- 1
for (i in 2:9){
  var.age <- paste("agem", k, sep = "")
  assign(var.age, tblage[c(1, i)])
  var.freq <- paste("Fage", k, sep = "") 
  assign(var.freq, table(get(var.age)))
  k <- k + 1
}
#creating the frequency tables of education and motivations
tbledu <- Mydata[c(18,5, 6, 7, 8, 9, 10, 11, 12)]
colnames(tbledu) <- c("education", "M1", "M2", "M3", "M4","M5", "M6", "M7", "M8")
for (h in 2:9){
  for(i in 1:74){
    
    if(tbledu[i,h]==1){tbledu[i,h]<-8}
    else if(tbledu[i,h]==2){tbledu[i,h]<-7}
    else if(tbledu[i,h]==3){tbledu[i,h]<-6}
    else if(tbledu[i,h]==4){tbledu[i,h]<-5}
    else if(tbledu[i,h]==5){tbledu[i,h]<-4}
    else if(tbledu[i,h]==6){tbledu[i,h]<-3}
    else if(tbledu[i,h]==7){tbledu[i,h]<-2}
    else if(tbledu[i,h]==8){tbledu[i,h]<-1}
    
    
  }}
k <- 1
for (i in 2:9){
  var.age <- paste("educationm", k, sep = "")
  assign(var.age, tbledu[c(1, i)])
  var.freq <- paste("Fedu", k, sep = "") 
  assign(var.freq, table(get(var.age)))
  k <- k + 1
}
#creating the frequency tables of gender and motivations
tblgen <- Mydata[c(19,5, 6, 7, 8, 9, 10, 11, 12)]
colnames(tblgen) <- c("gender", "M1", "M2", "M3", "M4","M5", "M6", "M7", "M8")
for (h in 2:9){
  for(i in 1:74){
    
    if(tblgen[i,h]==1){tblgen[i,h]<-8}
    else if(tblgen[i,h]==2){tblgen[i,h]<-7}
    else if(tblgen[i,h]==3){tblgen[i,h]<-6}
    else if(tblgen[i,h]==4){tblgen[i,h]<-5}
    else if(tblgen[i,h]==5){tblgen[i,h]<-4}
    else if(tblgen[i,h]==6){tblgen[i,h]<-3}
    else if(tblgen[i,h]==7){tblgen[i,h]<-2}
    else if(tblgen[i,h]==8){tblgen[i,h]<-1}
    
    
  }}


k <- 1
for (i in 2:9){
  var.age <- paste("genderm", k, sep = "")
  assign(var.age, tblgen[c(1, i)])
  var.freq <- paste("Fgen", k, sep = "") 
  assign(var.freq, table(get(var.age)))
  k <- k + 1
}
#creating the frequency tables of Occupation and motivations
tblocc <- Mydata[c(20,5, 6, 7, 8, 9, 10, 11, 12)]
colnames(tblocc) <- c("Job", "M1", "M2", "M3", "M4","M5", "M6", "M7", "M8")
for (h in 2:9){
  for(i in 1:74){
    
    if(tblocc[i,h]==1){tblocc[i,h]<-8}
    else if(tblocc[i,h]==2){tblocc[i,h]<-7}
    else if(tblocc[i,h]==3){tblocc[i,h]<-6}
    else if(tblocc[i,h]==4){tblocc[i,h]<-5}
    else if(tblocc[i,h]==5){tblocc[i,h]<-4}
    else if(tblocc[i,h]==6){tblocc[i,h]<-3}
    else if(tblocc[i,h]==7){tblocc[i,h]<-2}
    else if(tblocc[i,h]==8){tblocc[i,h]<-1}
    
    
  }}
k <- 1
for (i in 2:9){
  var.age <- paste("occupm", k, sep = "")
  assign(var.age, tblocc[c(1, i)])
  var.freq <- paste("Focc", k, sep = "") 
  assign(var.freq, table(get(var.age)))
  k <- k + 1
}
#creating the frequency tables of residence type and motivations
tblres <- Mydata[c(21,5, 6, 7, 8, 9, 10, 11, 12)]
colnames(tblres) <- c("Residence", "M1", "M2", "M3", "M4","M5", "M6", "M7", "M8")
for (h in 2:9){
  for(i in 1:74){
    
    if(tblres[i,h]==1){tblres[i,h]<-8}
    else if(tblres[i,h]==2){tblres[i,h]<-7}
    else if(tblres[i,h]==3){tblres[i,h]<-6}
    else if(tblres[i,h]==4){tblres[i,h]<-5}
    else if(tblres[i,h]==5){tblres[i,h]<-4}
    else if(tblres[i,h]==6){tblres[i,h]<-3}
    else if(tblres[i,h]==7){tblres[i,h]<-2}
    else if(tblres[i,h]==8){tblres[i,h]<-1}
    
    
  }}
k <- 1
for (i in 2:9){
  var.age <- paste("resdm", k, sep = "")
  assign(var.age, tblres[c(1, i)])
  var.freq <- paste("Fres", k, sep = "") 
  assign(var.freq, table(get(var.age)))
  k <- k + 1
}

Fage7<- matrix ( c(6,9,0,0,1,16,10,5,1,1, 9,2,0,0,0,0,0,0,0,0, 1,1,0,0,0,0,1,0,0,0, 3,3,1,0,1,1,2,0,0,0) ,nrow=5, ncol = 8)

#creating average points matrices based on the privius matrices for age and motivation

MPoints <- matrix(c(1,2,3,4,5,6,7,8), nrow = 8, ncol=1)
S<-data.frame(cbind(rsum=rowSums(Fage1)))

Mats <- list(Fage1, Fage2, Fage3, Fage4, Fage5, Fage6, Fage7, Fage8)
j<- 1

for (i in 1:8){
  varf<- Mats[[i]]
  var.p <- paste("p", j, sep = "")
  assign(var.p, (t((varf) %*% MPoints)))
  
  j<- j+1
}
# converting average points list for each motivation to matrices
p1<- matrix(unlist(p1))
p2<- matrix(unlist(p2))
p3<- matrix(unlist(p3))
p4<- matrix(unlist(p4))
p5<- matrix(unlist(p5))
p6<- matrix(unlist(p6))
p7<- matrix(unlist(p7))
p8<- matrix(unlist(p8))

pointsAge <- matrix(c(p1,p2,p3,p4,p5,p6,p7,p8),nrow=length(p1))
colnames(pointsAge) <- c("M1", "M2", "M3", "M4","M5", "M6", "M7", "M8")
rownames(pointsAge) <- c("15-24", "25-34", "35-44", "45-54", "55-64")


# calculating the correlations, hetcor function


#Age vs Technical questions
agetec <- Mydata[c(17, 13:16)]
#agetec<- as.matrix(agetec)
colnames(agetec)=c("Age", "Location", "Login", "Time", "Technology")
k<-1
for (c in 2:5){
  ageT <- paste("ageTec",k,sep = "" )
  assign(ageT, polycor::hetcor(agetec[c(1,c)]))
  k<-k+1
}


# corelation age vs motivations and technical questions
CorAgeMo<- polycor::hetcor(tblage)
CorAgeTech<-polycor::hetcor(agetec)


### res points
Fres7<- matrix ( c(10,0,1,2,1,2,17,0,0,4,6,6,6,0,0,0,1,4,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,3,1,0,0,0,4,1,0,0,0,0,2) ,nrow=6, ncol = 8)
Mats <- list(Fres1, Fres2, Fres3, Fres4, Fres5, Fres6, Fres7, Fres8)
j<- 1


for (i in 1:8){
  varf<- Mats[[i]]
  var.p <- paste("pR", j, sep = "")
  assign(var.p, (t((varf) %*% MPoints)))
  
  j<- j+1
}
# converting average points list for each motivation to matrices
pR1<- matrix(unlist(pR1))
pR2<- matrix(unlist(pR2))
pR3<- matrix(unlist(pR3))
pR4<- matrix(unlist(pR4))
pR5<- matrix(unlist(pR5))
pR6<- matrix(unlist(pR6))
pR7<- matrix(unlist(pR7))
pR8<- matrix(unlist(pR8))

pointsRes <- matrix(c(pR1,pR2,pR3,pR4,pR5,pR6,pR7,pR8),nrow=length(pR1))
colnames(pointsRes) <- c("M1", "M2", "M3", "M4","M5", "M6", "M7", "M8")
#rownames(pointsRes) <- c("15-24", "25-34", "35-44")

###Freeman theta coeficient on Residence vs Motivations
ThetaRes<- rcompanion::freemanTheta(pointsRes)
j<-1
for (i in 1:8){
  varf<- Mats[[i]]
  var.p <- paste("ThetaR", j, sep = "")
  assign(var.p, rcompanion::freemanTheta(varf))
  
  j<- j+1
}
ThetaR <- matrix(c(ThetaR1,ThetaR2,ThetaR3,ThetaR4,ThetaR5,ThetaR6,ThetaR7,ThetaR8), nrow = 1)

### Freeman theta for Occupation
Mats <- list(Focc1, Focc2, Focc3, Focc4, Focc5, Focc6, Focc7, Focc8)
j<- 1
for (i in 1:8){
  varf<- Mats[[i]]
  var.p <- paste("ThetaO", j, sep = "")
  assign(var.p, rcompanion::freemanTheta(varf))
  
  j<- j+1
}
ThetaO <- matrix(c(ThetaO1,ThetaO2,ThetaO3,ThetaO4,ThetaO5,ThetaO6,ThetaO7,ThetaO8), nrow = 1)

### Freeman theta for Gender
Mats <- list(Fgen1, Fgen2, Fgen3, Fgen4, Fgen5, Fgen6, Fgen7, Fgen8)
j<- 1
for (i in 1:8){
  varf<- Mats[[i]]
  var.p <- paste("ThetaG", j, sep = "")
  assign(var.p, rcompanion::freemanTheta(varf))
  
  j<- j+1
}
ThetaG <- matrix(c(ThetaG1,ThetaG2,ThetaG3,ThetaG4,ThetaG5,ThetaG6,ThetaG7,ThetaG8), nrow = 1)

### Freeman theta for Education
Mats <- list(Fedu1, Fedu2, Fedu3, Fedu4, Fedu5, Fedu6, Fedu7, Fedu8)
j<- 1
for (i in 1:8){
  varf<- Mats[[i]]
  var.p <- paste("ThetaE", j, sep = "")
  assign(var.p, rcompanion::freemanTheta(varf))
  
  j<- j+1
}
ThetaE <- matrix(c(ThetaE1,ThetaE2,ThetaE3,ThetaE4,ThetaE5,ThetaE6,ThetaE7,ThetaE8), nrow = 1)

## Matrix Theta coeficients 
ThetaCoef<- rbind(ThetaR, ThetaO, ThetaG, ThetaE)
colnames(ThetaCoef)=c("M1","M2", "M3", "M4","M5", "M6", "M7", "M8")
rownames(ThetaCoef)=c("Residence Type", "Occupation", "Gender", "Education")

# Correlation Age, Education and Age 
AgeEduMo<-Mydata[c(17,18,5:12)]
colnames(AgeEduMo)=c("Age","Education", "M1","M2", "M3", "M4","M5", "M6", "M7", "M8")
CorAgeEduMo<- polycor::hetcor(AgeEduMo)



###motivations ranking
colnames(tblage)=c("Age", "Helping the nature", "Learning about biodiversity", "Spending time in the nature","Contributing to a scientific project","Having fun", "Growing my social network", "Get recognized among my friends and social connections", "Receiving an award or certificate" )
Mo <- tblage[c(2:9)]
Mo<-(colSums(Mo)/74)
Mo<-sort(Mo)
par(mai=c(1,5,1,1))
barplot(Mo, horiz = TRUE, las=1, col = c("lightblue"))
