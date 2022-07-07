library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)

setwd("C:/Users/Sahil/Documents/Nottingham/Masters/Data Modelling and Analysis/Project")
df =  read.csv("diabetes.csv")
head(df)

# Data types
str(df)

# Summary statistics
summary(df)

# Histograms to display distribution of features
hist(df$Pregnancies,xlab = "Pregnancies",main="Histogram for Pregnancies")
hist(df$Glucose,xlab="Glucose",main="Histogram for Glucose")
hist(df$BloodPressure,xlab="Blood Pressure",main="Histogram for Blood Pressure")
hist(df$SkinThickness,xlab="Skin Thickness",main="Histogram for Skin Thickness")
hist(df$Insulin,xlab="Insulin",main="Histogram for Insulin")
hist(df$Age,xlab="Age",main="Histogram for Age")
hist(df$BMI,xlab="BMI",main="Histogram for BMI")
hist(df$DiabetesPedigreeFunction,xlab="DPF",main="Histogram for DPF")
hist(df$Outcome,xlab="Outcome",main="Histogram for Outcome")

# Correlation plot
corr<- melt(round(cor(df[1:8]),2)) %>% 
  ggplot(aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  coord_fixed() + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)+ylab("")+xlab("")+
  ggtitle("Correlation Matrix of Numerical Variables")+
  theme(plot.title = element_text(hjust = 0.5,face="bold"),
        axis.text.x = element_text(angle = 45, hjust=1))
corr

# Violin-boxplots grouped by outcome
preg <- ggplot(df, aes(x=Outcome,y=Pregnancies,group=Outcome,fill=Outcome)) + 
  geom_violin()+ 
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  labs(title="Violin Plot of Pregnancies by Outcome",x="Outcome", y = "Pregnancies")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none"
  )

gluc <- ggplot(df, aes(x=Outcome,y=Glucose,group=Outcome,fill=Outcome)) + 
  geom_violin()+
  geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  labs(title="Violin Plot of Glucose by Outcome",x="Outcome", y = "Glucose")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none"
  )

bp <- ggplot(df, aes(x=Outcome,y=BloodPressure,group=Outcome,fill=Outcome)) + 
  geom_violin()+geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  labs(title="Violin Plot of Blood Pressure by Outcome",x="Outcome", y = "Blood Pressure")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none"
  )

st <- ggplot(df, aes(x=Outcome,y=SkinThickness,group=Outcome,fill=Outcome)) + 
  geom_violin()+ geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  labs(title="Violin Plot of Skin Thickness by Outcome",x="Outcome", y = "Skin Thickness")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none"
  )

insuln <- ggplot(df, aes(x=Outcome,y=Insulin,group=Outcome,fill=Outcome)) + 
  geom_violin()+ geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  labs(title="Violin Plot of Insulin by Outcome",x="Outcome", y = "Insulin")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none"
  )
bmi <- ggplot(df, aes(x=Outcome,y=BMI,group=Outcome,fill=Outcome)) + 
  geom_violin()+ geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  labs(title="Violin Plot of BMI by Outcome",x="Outcome", y = "BMI")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none"
  )

dbf <- ggplot(df, aes(x=Outcome,y=DiabetesPedigreeFunction,group=Outcome,fill=Outcome)) + 
  geom_violin()+geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  labs(title="Violin Plot of Diabetes Pedigree Function by Outcome",x="Outcome",
       y = "Diabetes Pedigree Function")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none"
  )

age <- ggplot(df, aes(x=Outcome,y=Age ,group=Outcome,fill=Outcome)) + 
  geom_violin()+ geom_boxplot(width=0.1, color="grey", alpha=0.2) +
  labs(title="Violin Plot of Age  by Outcome",x="Outcome", y = "Age")+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position="none"
  )


gridExtra::grid.arrange(preg,gluc,bp,st,insuln,bmi,dbf,age)







