## title: Fishing Selectivity Model
## author: Jorge Cornejo
## date: 12/15/2017
## Updated and revisited: Jul 27, 2018
## Goal: This is simple proof of concept model to test the effects of 
##       fishing selectivity on a Sockeye like salmon population.


## Simple model to obtain length from age.
## This need to include a gausian distribution of the length for each age
ageToLength <- function(ageSex, ageDistro)
{
  ageSex <- merge(ageSex, ageDistro, by.x=c("sex", "Salt.Water.Age"), by.y=c("Sex", "Salt.Water.Age"))[,-c(3)]
  length <- unlist(lapply(ageSex$meanLength, ageSex$sdLength, FUN=rnorm, n=1))
  #length <- rnorm(1, ageSex$meanLength, ageSex$sdLength)
  return(length)
}


##########################
## This is the model ##
# F = Fishing mortaltity
# selectivity = If the model should account for fishing selectivity
# Other input variable are defined at the bottom
##########################
selectivityModel <- function(F = 0.6, selectivity = T, runFor = 30, sexComp=0.5, 
                             ageDistro, selPoints, FishAges, rec, escGoal = 2000, yearToCompareLength = 5, rmd = FALSE, ...)
{
  selPointL <- selPoints[1:2,]
  modelL <- lm(pp ~ size, data = selPointL) #Model the selectivity curve
  selML <- modelL$coefficients[2]; selCL <- modelL$coefficients[1]
  
  selPointU <- selPoints[3:4,]
  modelU <- lm(pp ~ size, data = selPointU) #Model the selectivity curve
  selMU <- modelU$coefficients[2]; selCU <- modelU$coefficients[1]
  ## This is the initial population!
  population <- data.frame(age=NULL, sex=NULL, length=NULL)
  
  for (i in 1:dim(ageDistro)[1])
  {
    if(is.na(ageDistro$sd[i])) 
    {ageDistro$sd[i] <- 1}
    temp <- data.frame(age = ageDistro$Salt.Water.Age[i],
                       sex = ageDistro$Sex[i],
                       length = round(rnorm(ageDistro$n[i], mean=ageDistro$mean[i], sd=ageDistro$sd[i]),0))
    population <- rbind(population, temp)
  }
  temp <- summarize(group_by(population, sex), n=n())
  
  newSexComp <- filter(temp, sex == "female")$n/sum(temp$n)
  
  # Data Frame to store the outputs of the model!
  output <<- data.frame(run    = rep(NA, runFor), FAge1 = rep(NA, runFor), FAge2 = rep(NA, runFor), FAge3 = rep(NA, runFor), FAge4 = rep(NA, runFor), 
                        FAge5  = rep(NA, runFor), FAge6 = rep(NA, runFor), FAge7 = rep(NA, runFor), FAge8 = rep(NA, runFor), 
                        femToal= rep(NA, runFor), total = rep(NA, runFor), sexComp= rep(NA, runFor), landings=rep(NA, runFor),
                        meanSize=rep(NA, runFor), sdSize = rep(NA, runFor))
  
  ## This is where the simulation runs!
  if (rmd==FALSE)  {pb <- txtProgressBar(min = 0, max = runFor, style = 3)} ## This is a progress bar. Do not use it when un RMarkDown
  for (i in 1:runFor)
  {
    
    recSize <- alpha*dim(population)[1]*exp(beta*dim(population)[1])
    #if (dim(population)[1]>recSize)
    #{
    #    population <- sample_n(population, round((dim(population)[1]+recSize), 0))
    #}
    
    neggs <- summarize(group_by(population, sex, age), n = n())
    neggs$age <- as.numeric(as.character(neggs$age))
    neggs <- merge(neggs, rec, by=c("age"))
    
    neggs <- mutate(filter(neggs, sex=="female"), nEggs = n*ferti)
    neggs <- neggs[neggs$nEggs!=0,]
    neggs$nEggs <- round((dim(population)[1]+recSize) * neggs$nEggs/sum(neggs$nEggs), 0)
    
    
    age <- NULL
    for (j in 1:dim(neggs)[1])
    {
      if (neggs$age[j] <= 3)
      {
        age <- c(age,round(runif(neggs$nEggs[j], neggs$age[j], neggs$age[j]+2), 0))
      }
      else if (neggs$age[j] <= 6)
      {
        age <- c(age,round(runif(neggs$nEggs[j], neggs$age[j]-1, neggs$age[j]+1), 0))
      } else {age <- c(age,round(runif(neggs$nEggs[j], neggs$age[j]-2, neggs$age[j]), 0))}
    }
    
    #I'm assuming that the new population is derived from a 0.5 sex composition ratio
    nextPopSex <- rbinom(length(age), 1, sexComp) ## 1 = Female
    
    nextPopSex <- ifelse(nextPopSex == 1, "female", "male")
    ageSex <- data.frame(Salt.Water.Age = as.numeric(age), sex = nextPopSex)
    ######## Call function to transform from age to sex
    length <- ageToLength(ageSex, ageDistro) ###################### <<--This consumes a ver long time!!!
    ###################################
    population <- data.frame(age = age, length = length, sex = nextPopSex)
    ## Now we apply selective fishing for large fish and females
    
    
    if (selectivity == T)
    {
      ##This define the selectivity based on the length of a fish
      fishingSmall <- filter(population, length <= selPoints$size[2])
      fishingSmall$Vuln <- selML * fishingSmall$length + selCL
      
      fishingMed   <- filter(population, length > selPoints$size[2] & length <= selPoints$size[3])
      fishingMed$Vuln <- mean(selPoints$pp[2:3])
      
      fishingLarge <- filter(population, length > selPoints$size[3])
      fishingLarge$Vuln <- selMU * fishingLarge$length + selCU
      
      population <- rbind(fishingSmall, fishingMed, fishingLarge)
      
      # This create a 
      fishing <- lapply(population$Vuln, FUN=rnorm, n = 1, sd=0.06) ## Check for sensitivity to SD
      fishing <- unlist(fishing)
      
    } else {fishing <- runif(dim(population)[1], 0, 1)} 
    
    population$fishing <- 0
    if (fishingStart < i)
    {
      population$fishing <- fishing
    }
    
    
    population$id <- seq(1, dim(population)[1])
    if (dim(population)[1] > escGoal)
    {
      escapement <- sample(population$id, escGoal)
      escaped <- filter(population, id %in% escapement)
      availableToFish <- population[-escapement,]
      ## The F is corrected to account for the fish that cannot be captured.
      correctedF <- F*dim(availableToFish)[1]/dim(population)[1]
      
      population$toFish <- ifelse(population$fishing > quantile(population$fishing, (1-correctedF)), 1, 0) ## 1 are those who are removed by the fishery
      
      # Population after Fishery
      popAfterF <- population[population$toFish != 1,] ## Fishery
    } else {
      popAfterF <- population
    }
    
    
    temp <- summarize(group_by(popAfterF, sex), n=n())
    
    newSexComp <- filter(temp, sex == "female")$n/sum(temp$n)
    
    females <- popAfterF[popAfterF$sex == "female",]
    
    #hist(population$popAge, xlim=c(FishAges[1],FishAges[7]), freq=F, breaks = FishAges,
    #     main=paste(i, "#", length(population$popAge)), xlab = "Total Age [years]", ylab = "")
    
    ## This container will have the size estructure by age!
    if (i == yearToCompareLength)
    {
      females5 <- females
    }
    if (i == runFor)
    {
      femalesEnd <- females
    }
    #femalesLength$trunkedLength <- round(femalesLength$length/25, 0)*25
    #countsFemalesBySize <- summarize(group_by(femalesLength, age, trunkedLength), n=n())
    
    ######################
    
    counts <- summarize(group_by(females, age), n=n())
    counts <- merge(rec, counts, by=c("age"), all.x = T)
    
    output$run[i] <- i
    for (k in 1:length(counts$n))
    {
      output[i,(counts$age[k]+1)] <- counts$n[k]
    }
    output$femToal[i] <- sum(counts$n, na.rm = T)
    output$total[i] <- summarize(group_by(popAfterF), n=n())
    output$sexComp[i] <- newSexComp
    output$landings[i] <- dim(population)[1] - dim(popAfterF)[1]
    output$meanSize[i] <- mean(population$length)
    output$sdSize[i] <- sd(population$length)
    
    ## Now define Females as Population to start next iteration
    population <- females
    output <<- output
    if (rmd==FALSE) {setTxtProgressBar(pb, i)}
  }
  return(list(ouput=output, fem5=females5, femEnd=femalesEnd))
}

