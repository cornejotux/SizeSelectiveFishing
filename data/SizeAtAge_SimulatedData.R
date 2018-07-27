## Define input Paramenters!
ageDistro <- read.csv(file="data/MeanSD_BB.csv")
#ageDistro <- c(3.86421, 0.6871429) #Mean and SD of Total Age

dataSet <- temp <- data.frame(totalAge=NULL, Sex=NULL, Length=NULL)
for (i in 1:dim(ageDistro)[1])
    {
    data <- rnorm(1000, mean=ageDistro$meanLength[i], sd=ageDistro$sdLength[i])
    temp <- data.frame(totalAge=ageDistro$Salt.Water.Age[i], Sex=ageDistro$Sex[i], Length=data)
    dataSet <- rbind(dataSet, temp)    
}

ggplot(data = dataSet, aes(x=Length)) +
    geom_histogram() + facet_grid(totalAge~Sex)
