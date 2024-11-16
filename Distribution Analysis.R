# Load Library
library(ggplot2)

# Load Data
carsDB

# Weight vs. displacement scatter plot with smoothing line
ggplot(data = carsDB, aes(x = wt, y = disp)) +
  geom_point() +
  geom_smooth(aes(group = 1), se =TRUE) +
  labs(title = " Weight vs. Displacement", x = "Weight", y = "Displacement")

#Weight vs. Miles/(US) gallon scatter plot with smoothing line
ggplot(data = carsDB, aes(x=wt, y=mpg))+
  geom_point()+
  geom_smooth(aes(group=1), se= TRUE)+
  labs(title="Weight vs. Miles/(US) gallon " , x="weight", y = "Miles/(US) gallon" )

#Weight vs.Gross horsepower scatter plot with smoothing line
ggplot(data = carsDB, aes(x=wt, y=hp))+
  geom_point()+
  geom_smooth(aes(group=1), se=TRUE)+
  labs(title="Weight vs. Horsepower " , x="weight", y = "hp" )

