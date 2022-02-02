###################################
# EMB - DAY 8 5h salt incubation  #
# Margot Le Bot - Master 1        #
#                                 #
#    January 25th 2022            #
###################################
library("readxl")
library("ggplot2")

###
#Day 7 data, no salt and salt cultures incubated for 24h. Put in plate reader for 24h at 30°C
#Kinetic cycle of 30minutes with 1000s of shaking, then absorbance measure at 600nm
###

table1 <- read_excel('C:\\Users\\margo\\Documents\\Master AIRE Life Sciences\\Master 1\\EMB\\Plate reader 5h incubation\\Plate-reader-2.xlsx', 2)


###
#Media
### 

growthratemedia <- rbind(subset.data.frame(table1, table1$`Time [s]` =='Growth M1'),
                         subset.data.frame(table1, table1$`Time [s]` =='Growth M2'),
                         subset.data.frame(table1, table1$`Time [s]` =='Growth M3'),
                         subset.data.frame(table1, table1$`Time [s]` =='Growth M4'),
                         subset.data.frame(table1, table1$`Time [s]` =='Growth M5'),
                         subset.data.frame(table1, table1$`Time [s]` =='Growth M6'))

growthratemedia <- data.frame(t(growthratemedia[-1]))
growthratemedia <- data.frame(values = c(growthratemedia$X1,growthratemedia$X2,growthratemedia$X3,growthratemedia$X4,growthratemedia$X5,growthratemedia$X6),
                              names = c(rep('M1',36),rep('M2',36),rep('M3',36),rep('M4',36),rep('M5',36),rep('M6',36)))

comparaison1 <- mean(growthratemedia$values)
error <- sd(growthratemedia$values)
#We know that population is not normal 
#Student t test pairwise
t.test(as.numeric(growthratemedia$values), mu = comparaison1, var.equal = FALSE)
#True mean is not equal to 0


###
#0 g/L
###

growthrate0NS <- rbind(subset.data.frame(table1, table1$`Time [s]` =='Growth 0 no salt 1'),
                         subset.data.frame(table1, table1$`Time [s]` =='Growth 0 no salt 2'),
                         subset.data.frame(table1, table1$`Time [s]` =='Growth 0 no salt 3'))

growthrate0NS <- data.frame(t(growthrate0NS[-1]))
growthrate0NS <- data.frame(values = c(growthrate0NS$X1,growthrate0NS$X2,growthrate0NS$X3),
                              names = c(rep('0NS1',36),rep('0NS 2',36),rep('0NS 3',36)))

comparaison <- mean((growthrate0NS$values))

#We know that population is not normal 
#Student t test pairwise
t.test(growthrate0NS$values, mu = comparaison, var.equal = FALSE)
##all replicates are comparable to the mean
sd(growthrate0NS$values)

growthrate0S <- rbind(subset.data.frame(table1, table1$`Time [s]` =='Growth 0 salt 1'),
                      subset.data.frame(table1, table1$`Time [s]` =='Growth 0 salt 2'),
                      subset.data.frame(table1, table1$`Time [s]` =='Growth 0 salt 3'))
growthrate0S <- data.frame(t(growthrate0S[-1]))
growthrate0S <- data.frame(values = c(growthrate0S$X1,growthrate0S$X2,growthrate0S$X3),
                            names = c(rep('0S 1',36),rep('0S 2',36),rep('0S 3',36)))

comparaison2 <- mean((growthrate0S$values))

#We know that population is not normal 
#Student t test pairwise
t.test(as.numeric(growthrate0S$values), mu = comparaison2 , var.equal = FALSE)
#all replicates are comparable to the mean

growthrate0 <- data.frame(values = c(growthrate0NS$values, growthrate0S$values),
                          names = c(rep('no salt',108),rep('salt',108)))


shapiro.test(growthrate0$values)
#Data is not normal 
t.test(growthrate0NS$values, growthrate0S$values, var.equal = FALSE)
#Salt and no salt are NOT different modalities for 0

###
#20 g/L
###

growthrate20NS <- rbind(subset.data.frame(table1, table1$`Time [s]` =='Growth 20 no salt 1'),
                       subset.data.frame(table1, table1$`Time [s]` =='Growth 20 no salt 2'),
                       subset.data.frame(table1, table1$`Time [s]` =='Growth 20 no salt 3'))

growthrate20NS <- data.frame(t(growthrate20NS[-1]))
growthrate20NS <- data.frame(values = c(growthrate20NS$X1,growthrate20NS$X2,growthrate20NS$X3),
                            names = c(rep('20NS1',36),rep('20NS 2',36),rep('20NS 3',36)))

comparaison3 <- mean((growthrate20NS$values))

#We know that population is not normal 
#Student t test pairwise
t.test(as.numeric(growthrate20NS$values), mu = comparaison3, var.equal = FALSE)
##all replicates are comparable to the mean

growthrate20S <- rbind(subset.data.frame(table1, table1$`Time [s]` =='Growth 20 salt 1'),
                      subset.data.frame(table1, table1$`Time [s]` =='Growth 20 salt 2'),
                      subset.data.frame(table1, table1$`Time [s]` =='Growth 20 salt 3'))
growthrate20S <- data.frame(t(growthrate20S[-1]))
growthrate20S <- data.frame(values = c(growthrate20S$X1,growthrate20S$X2,growthrate20S$X3),
                           names = c(rep('20S 1',36),rep('20S 2',36),rep('20S 3',36)))

comparaison4 <- mean((growthrate20S$values))

#We know that population is not normal 
#Student t test pairwise
t.test(as.numeric(growthrate20S$values), mu = comparaison4, var.equal = FALSE)
##all replicates are comparable to the mean

growthrate20 <- data.frame(values = c(growthrate20NS$values, growthrate20S$values),
                          names = c(rep('no salt',108),rep('salt',108)))

shapiro.test(growthrate20$values)
#Data is not normal 
t.test(growthrate20NS$values, growthrate20S$values, var.equal = FALSE)
#Salt and no salt are different modalities for 20

###
#40 g/L
###

growthrate40NS <- rbind(subset.data.frame(table1, table1$`Time [s]` =='Growth 40 no salt 1'),
                        subset.data.frame(table1, table1$`Time [s]` =='Growth 40 no salt 2'),
                        subset.data.frame(table1, table1$`Time [s]` =='Growth 40 no salt 3'))

growthrate40NS <- data.frame(t(growthrate40NS[-1]))
growthrate40NS <- data.frame(values = c(growthrate40NS$X1,growthrate40NS$X2,growthrate40NS$X3),
                             names = c(rep('40NS1',36),rep('40NS 2',36),rep('40NS 3',36)))

comparaison5 <- mean((growthrate40NS$values))

#We know that population is not normal 
#Student t test pairwise
t.test(as.numeric(growthrate40NS$values), mu = comparaison5, var.equal = FALSE)
##all replicates are comparable to the mean

growthrate40S <- rbind(subset.data.frame(table1, table1$`Time [s]` =='Growth 40 salt 1'),
                       subset.data.frame(table1, table1$`Time [s]` =='Growth 40 salt 2'),
                       subset.data.frame(table1, table1$`Time [s]` =='Growth 40 salt 3'))
growthrate40S <- data.frame(t(growthrate40S[-1]))
growthrate40S <- data.frame(values = c(growthrate40S$X1,growthrate40S$X2,growthrate40S$X3),
                            names = c(rep('40S 1',36),rep('40S 2',36),rep('40S 3',36)))

comparaison6 <- mean((growthrate40S$values))

#We know that population is not normal 
#Student t test pairwise
t.test(as.numeric(growthrate40S$values), mu = comparaison6, var.equal = FALSE)
##all replicates are comparable to the mean

growthrate40 <- data.frame(values = c(growthrate40NS$values, growthrate40S$values),
                           names = c(rep('no salt',108),rep('salt',108)))

shapiro.test(growthrate40$values)
#Data is not normal 
t.test(growthrate40NS$values, growthrate40S$values, var.equal = FALSE)
#Salt and no salt are different modalities for 40

###
#60 g/L
###

growthrate60NS <- rbind(subset.data.frame(table1, table1$`Time [s]` =='Growth 60 no salt 1'),
                        subset.data.frame(table1, table1$`Time [s]` =='Growth 60 no salt 2'),
                        subset.data.frame(table1, table1$`Time [s]` =='Growth 60 no salt 3'))

growthrate60NS <- data.frame(t(growthrate60NS[-1]))
growthrate60NS <- data.frame(values = c(growthrate60NS$X1,growthrate60NS$X2,growthrate60NS$X3),
                             names = c(rep('60NS1',36),rep('60NS 2',36),rep('60NS 3',36)))

comparaison7 <- mean((growthrate60NS$values))

#We know that population is not normal 
#Student t test pairwise
t.test(as.numeric(growthrate60NS$values), mu = comparaison7, var.equal = FALSE)
##all replicates are comparable to the mean

growthrate60S <- rbind(subset.data.frame(table1, table1$`Time [s]` =='Growth 60 salt 1'),
                       subset.data.frame(table1, table1$`Time [s]` =='Growth 60 salt 2'),
                       subset.data.frame(table1, table1$`Time [s]` =='Growth 60 salt 3'))
growthrate60S <- data.frame(t(growthrate60S[-1]))
growthrate60S <- data.frame(values = c(growthrate60S$X1,growthrate60S$X2,growthrate60S$X3),
                            names = c(rep('60S 1',36),rep('60S 2',36),rep('60S 3',36)))

comparaison8 <- mean((growthrate60S$values))

#We know that population is not normal 
#Student t test pairwise
t.test(as.numeric(growthrate60S$values), mu = comparaison8, var.equal = FALSE)
##all replicates are comparable to the mean

growthrate60 <- data.frame(values = c(growthrate60NS$values, growthrate60S$values),
                           names = c(rep('no salt',108),rep('salt',108)))

shapiro.test(growthrate60$values)
#Data is not normal 
t.test(growthrate60NS$values, growthrate60S$values, var.equal = FALSE)
#Salt and no salt are different modalities for 60

###
#80 g/L
###

growthrate80NS <- rbind(subset.data.frame(table1, table1$`Time [s]` =='Growth 80 no salt 1'),
                        subset.data.frame(table1, table1$`Time [s]` =='Growth 80 no salt 2'),
                        subset.data.frame(table1, table1$`Time [s]` =='Growth 80 no salt 3'))

growthrate80NS <- data.frame(t(growthrate80NS[-1]))
growthrate80NS <- data.frame(values = c(growthrate80NS$X1,growthrate80NS$X2,growthrate80NS$X3),
                             names = c(rep('80NS1',36),rep('80NS 2',36),rep('80NS 3',36)))

comparaison9 <- mean((growthrate80NS$values))

#We know that population is not normal 
#Student t test pairwise
t.test(growthrate80NS$values, mu = comparaison9, var.equal = FALSE)
##all replicates are comparable to the mean

growthrate80S <- rbind(subset.data.frame(table1, table1$`Time [s]` =='Growth 80 salt 1'),
                       subset.data.frame(table1, table1$`Time [s]` =='Growth 80 salt 2'),
                       subset.data.frame(table1, table1$`Time [s]` =='Growth 80 salt 3'))
growthrate80S <- data.frame(t(growthrate80S[-1]))
growthrate80S <- data.frame(values = c(growthrate80S$X1,growthrate80S$X2,growthrate80S$X3),
                            names = c(rep('80S 1',36),rep('80S 2',36),rep('80S 3',36)))

comparaison10 <- mean((growthrate80S$values))

#We know that population is not normal 
#Student t test pairwise
t.test(as.numeric(growthrate80S$values), mu = comparaison10, var.equal = FALSE)
##all replicates are comparable to the mean

growthrate80 <- data.frame(values = c(growthrate80NS$values, growthrate80S$values),
                           names = c(rep('no salt',108),rep('salt',108)))

shapiro.test(growthrate80$values)
#Data is not normal 
t.test(growthrate80NS$values, growthrate80S$values, var.equal = FALSE)
#Salt and no salt are different modalities for 80

###
#Testing for a difference between NS growth rate
#No data is normally distributed therefore all t.test
###

growthNS <- data.frame(values = c(growthratemedia$values, growthrate0NS$values, growthrate20NS$values, growthrate40NS$values, growthrate60NS$values, growthrate80NS$values),
                       names = c(rep('growth media', 216),rep('growth 0',108),rep('growth 20',108),rep('growth 40',108),rep('growth 60',108), rep('growth 80', 108)))

t.test(growthNS$values, var.equal = FALSE)
#At least one of the data set is different 

boxplot(growthNS$values~growthNS$names)
pairwise.t.test(x= growthNS$values, g= growthNS$names, p.adjust.method = 'bonferroni')

#growth 0, 20 and 40 are comparable when no salt 
#growth 60 and 80 are different from the first 3 and within each other

###
#Testing for a difference between S growth rate
#No data is normally distributed therefore all t.test
###

growthS <- data.frame(values = c(growthrate0S$values, growthrate20S$values, growthrate40S$values, growthrate60S$values, growthrate80S$values),
                       names = c(rep('growth 0',108),rep('growth 20',108),rep('growth 40',108),rep('growth 60',108), rep('growth 80', 108)))

t.test(growthS$values, var.equal = FALSE)
#At least one of the data set is different 

pairwise.t.test(x= growthS$values, g= growthS$names, p.adjust.method = 'bonferroni')

#growth 20 and 40 are comparable when salt 
#growth 0, 60 and 80 are different from the first 2 and within each other

###
#Comparing for graphical observation
###

#40NS and 20S

t.test(growthrate20S$values, growthrate40NS$values, var.equal = FALSE)
#They are statistically different 

#60NS and 40S

t.test(growthrate40S$values, growthrate60NS$values, var.equal = FALSE)
#They are NOT statistically different 

#80NS and 60S

t.test(growthrate60S$values, growthrate80NS$values, var.equal = FALSE)
#They are NOT statistically different 

###
#Day 3 data, no salt cultures incubated for 24h. Put in plate reader for 24h at 30°C
#Kinetic cycle of 30minutes with 1000s of shaking, then absorbance measure at 600nm
###

table2 <- read_excel('C:\\Users\\margo\\Documents\\Master AIRE Life Sciences\\Master 1\\EMB\\Plate reader day 3\\Plate-reader-1.xlsx')

###
#Media g/L
###

ratemedia <- rbind(subset.data.frame(table2, table2$`Cycle Nr.` =='M1'),
                   subset.data.frame(table2, table2$`Cycle Nr.` =='M2'),
                   subset.data.frame(table2, table2$`Cycle Nr.` =='M3'))

ratemedia <- data.frame(t(ratemedia[-1]))
ratemedia <- data.frame(values = c(ratemedia$X1,ratemedia$X2,ratemedia$X3),
                              names = c(rep('M1',41),rep('M2',41),rep('M3',41)))

comparaison11 <- mean(ratemedia$values)
#We know that population is not normal 
#Student t test pairwise
t.test(as.numeric(ratemedia$values), mu = comparaison11, var.equal = FALSE)
#All replicates are equivalent

###
#0 g/L
###

rate0 <- rbind(subset.data.frame(table2, table2$`Cycle Nr.` =='0 1'),
                   subset.data.frame(table2, table2$`Cycle Nr.` =='0 2'),
                   subset.data.frame(table2, table2$`Cycle Nr.` =='0 3'))

rate0 <- data.frame(t(rate0[-1]))
rate0 <- data.frame(values = c(rate0$X1,rate0$X2,rate0$X3),
                        names = c(rep('0 1',41),rep('0 2',41),rep('0 3',41)))

comparaison12 <- mean(rate0$values)
#We know that population is not normal 
#Student t test pairwise
t.test(as.numeric(rate0$values), mu = comparaison12, var.equal = FALSE)
#All replicates are equivalent

###
#20 g/L
###

rate20 <- rbind(subset.data.frame(table2, table2$`Cycle Nr.` =='20 1'),
               subset.data.frame(table2, table2$`Cycle Nr.` =='20 2'),
               subset.data.frame(table2, table2$`Cycle Nr.` =='20 3'))

rate20 <- data.frame(t(rate20[-1]))
rate20 <- data.frame(values = c(rate20$X1,rate20$X2,rate20$X3),
                    names = c(rep('20 1',41),rep('20 2',41),rep('20 3',41)))

comparaison13 <- mean(rate20$values)
#We know that population is not normal 
#Student t test pairwise
t.test(as.numeric(rate20$values), mu = comparaison13, var.equal = FALSE)
#All replicates are equivalent

###
#40 g/L
###

rate40 <- rbind(subset.data.frame(table2, table2$`Cycle Nr.` =='40 1'),
               subset.data.frame(table2, table2$`Cycle Nr.` =='40 2'),
               subset.data.frame(table2, table2$`Cycle Nr.` =='40 3'))

rate40 <- data.frame(t(rate40[-1]))
rate40 <- data.frame(values = c(rate40$X1,rate40$X2,rate40$X3),
                    names = c(rep('40 1',41),rep('40 2',41),rep('40 3',41)))

comparaison14 <- mean(rate40$values)
#We know that population is not normal 
#Student t test pairwise
t.test(as.numeric(rate40$values), mu = comparaison14, var.equal = FALSE)
#All replicates are equivalent

###
#60 g/L
###

rate60 <- rbind(subset.data.frame(table2, table2$`Cycle Nr.` =='60 1'),
               subset.data.frame(table2, table2$`Cycle Nr.` =='60 2'),
               subset.data.frame(table2, table2$`Cycle Nr.` =='60 3'))

rate60 <- data.frame(t(rate60[-1]))
rate60 <- data.frame(values = c(rate60$X1,rate60$X2,rate60$X3),
                    names = c(rep('60 1',41),rep('60 2',41),rep('60 3',41)))

comparaison15 <- mean(rate60$values)
#We know that population is not normal 
#Student t test pairwise
t.test(as.numeric(rate60$values), mu = comparaison15, var.equal = FALSE)
#All replicates are equivalent

###
#80 g/L
###

rate80 <- rbind(subset.data.frame(table2, table2$`Cycle Nr.` =='80 1'),
               subset.data.frame(table2, table2$`Cycle Nr.` =='80 2'),
               subset.data.frame(table2, table2$`Cycle Nr.` =='80 3'))

rate80 <- data.frame(t(rate80[-1]))
rate80 <- data.frame(values = c(rate80$X1,rate80$X2,rate80$X3),
                    names = c(rep('80 1',41),rep('80 2',41),rep('80 3',41)))

comparaison16 <- mean(rate80$values)
#We know that population is not normal 
#Student t test pairwise
t.test(as.numeric(rate80$values), mu = comparaison16, var.equal = FALSE)
#All replicates are equivalent

###
#Testing for a difference between growth rate
#No data is normally distributed therefore all t.test
###

rate <- data.frame(values = c(ratemedia$values, rate0$values, rate20$values, rate40$values, rate60$values, rate80$values),
                       names = c(rep('growth media', 123),rep('growth 0',123),rep('growth 20',123),rep('growth 40',123),rep('growth 60',123), rep('growth 80', 123)))

t.test(rate$values, var.equal = FALSE)
#At least one of the data set is different 

boxplot(rate$values~rate$names)
pairwise.t.test(x= rate$values, g= rate$names, p.adjust.method = 'bonferroni')
#Growth rate of 0, 20 and 40 are statistically the same 
#Growth rate 60 and 80 are NOT statistically different from each other 
#First and second group are statistically different 

###
#Day 9 data, no salt and salt cultures incubated for 24h. Put in plate reader for 24h at 30°C
#Kinetic cycle of 30minutes with 1000s of shaking, then absorbance measure at 600nm
###

table3 <- read_excel('C:\\Users\\margo\\Documents\\Master AIRE Life Sciences\\Master 1\\EMB\\Plate reader 24h\\Plate-reader-3.xlsx')


###
#Media
### 

growthmedia <- rbind(subset.data.frame(table3, table3$`Cycle Nr.` =='M1'),
                     subset.data.frame(table3, table3$`Cycle Nr.` =='M2'),
                     subset.data.frame(table3, table3$`Cycle Nr.` =='M3'),
                     subset.data.frame(table3, table3$`Cycle Nr.` =='M4'),
                     subset.data.frame(table3, table3$`Cycle Nr.` =='M5'),
                     subset.data.frame(table3, table3$`Cycle Nr.` =='M6'))

growthmedia <- data.frame(t(growthmedia[-1]))
growthmedia <- data.frame(values = c(growthmedia$X1,growthmedia$X2,growthmedia$X3,growthmedia$X4,growthmedia$X5,growthmedia$X6),
                              names = c(rep('M1',39),rep('M2',39),rep('M3',39),rep('M4',39),rep('M5',39),rep('M6',39)))

comparaison17 <- mean(growthmedia$values)
error <- sd(growthmedia$values)
#We know that population is not normal 
#Student t test pairwise
t.test(as.numeric(growthmedia$values), mu = comparaison17, var.equal = FALSE)
#All replicates are equivalent


###
#0 g/L
###

growth0NS <- rbind(subset.data.frame(table3, table3$`Cycle Nr.` =='0 no salt 1'),
                   subset.data.frame(table3, table3$`Cycle Nr.` =='0 no salt 2'),
                   subset.data.frame(table3, table3$`Cycle Nr.` =='0 no salt 3'))

growth0NS <- data.frame(t(growth0NS[-1]))
growth0NS <- data.frame(values = c(growth0NS$X1,growth0NS$X2,growth0NS$X3),
                        names = c(rep('0NS1',39),rep('0NS 2',39),rep('0NS 3',39)))

comparaison18 <- mean((growth0NS$values))

#We know that population is not normal 
#Student t test pairwise
t.test(growth0NS$values, mu = comparaison18, var.equal = FALSE)
##all replicates are comparable to the mean
sd(growthrate0NS$values)

growth0S <- rbind(subset.data.frame(table3, table3$`Cycle Nr.` =='0 salt 1'),
                  subset.data.frame(table3, table3$`Cycle Nr.` =='0 salt 2'),
                  subset.data.frame(table3, table3$`Cycle Nr.` =='0 salt 3'))
growth0S <- data.frame(t(growth0S[-1]))
growth0S <- data.frame(values = c(growth0S$X1,growth0S$X2,growth0S$X3),
                       names = c(rep('0S 1',39),rep('0S 2',39),rep('0S 3',39)))

comparaison19 <- mean((growth0S$values))

#We know that population is not normal 
#Student t test pairwise
t.test(as.numeric(growth0S$values), mu = comparaison2 , var.equal = FALSE)
#all replicates are comparable to the mean

growth0 <- data.frame(values = c(growth0NS$values, growth0S$values),
                      names = c(rep('no salt',117),rep('salt',117)))


shapiro.test(growth0$values)
#Data is not normal 
t.test(growth0NS$values, growth0S$values, var.equal = FALSE)
#Salt and no salt are NOT different modalities for 0

###
#20 g/L
###

growth20NS <- rbind(subset.data.frame(table3, table3$`Cycle Nr.` =='20 no salt 1'),
                    subset.data.frame(table3, table3$`Cycle Nr.` =='20 no salt 2'),
                    subset.data.frame(table3, table3$`Cycle Nr.` =='20 no salt 3'))

growth20NS <- data.frame(t(growth20NS[-1]))
growth20NS <- data.frame(values = c(growth20NS$X1,growth20NS$X2,growth20NS$X3),
                         names = c(rep('20NS1',39),rep('20NS 2',39),rep('20NS 3',39)))

comparaison20 <- mean((growth20NS$values))

#We know that population is not normal 
#Student t test pairwise
t.test(growth20NS$values, mu = comparaison20, var.equal = FALSE)
##all replicates are comparable to the mean

growth20S <- rbind(subset.data.frame(table3, table3$`Cycle Nr.` =='20 salt 1'),
                  subset.data.frame(table3, table3$`Cycle Nr.` =='20 salt 2'),
                  subset.data.frame(table3, table3$`Cycle Nr.` =='20 salt 3'))
growth20S <- data.frame(t(growth20S[-1]))
growth20S <- data.frame(values = c(growth20S$X1,growth20S$X2,growth20S$X3),
                       names = c(rep('20S 1',39),rep('20S 2',39),rep('20S 3',39)))

comparaison21 <- mean((growth20S$values))

#We know that population is not normal 
#Student t test pairwise
t.test(as.numeric(growth20S$values), mu = comparaison21 , var.equal = FALSE)
#all replicates are comparable to the mean

growth20 <- data.frame(values = c(growth20NS$values, growth20S$values),
                      names = c(rep('no salt',117),rep('salt',117)))


shapiro.test(growth20$values)
#Data is not normal 
t.test(growth20NS$values, growth20S$values, var.equal = FALSE)
#Salt and no salt are NOT different modalities for 20

###
#40 g/L
###

growth40NS <- rbind(subset.data.frame(table3, table3$`Cycle Nr.` =='40 no salt 1'),
                    subset.data.frame(table3, table3$`Cycle Nr.` =='40 no salt 2'),
                    subset.data.frame(table3, table3$`Cycle Nr.` =='40 no salt 3'))

growth40NS <- data.frame(t(growth40NS[-1]))
growth40NS <- data.frame(values = c(growth40NS$X1,growth40NS$X2,growth40NS$X3),
                         names = c(rep('40NS1',39),rep('40NS 2',39),rep('40NS 3',39)))

comparaison22 <- mean((growth40NS$values))

#We know that population is not normal 
#Student t test pairwise
t.test(growth40NS$values, mu = comparaison22, var.equal = FALSE)
##all replicates are comparable to the mean

growth40S <- rbind(subset.data.frame(table3, table3$`Cycle Nr.` =='40 salt 1'),
                   subset.data.frame(table3, table3$`Cycle Nr.` =='40 salt 2'),
                   subset.data.frame(table3, table3$`Cycle Nr.` =='40 salt 3'))
growth40S <- data.frame(t(growth40S[-1]))
growth40S <- data.frame(values = c(growth40S$X1,growth40S$X2,growth40S$X3),
                        names = c(rep('40S 1',39),rep('40S 2',39),rep('40S 3',39)))

comparaison23 <- mean((growth40S$values))

#We know that population is not normal 
#Student t test pairwise
t.test(as.numeric(growth40S$values), mu = comparaison23 , var.equal = FALSE)
#all replicates are comparable to the mean

growth40 <- data.frame(values = c(growth40NS$values, growth40S$values),
                       names = c(rep('no salt',117),rep('salt',117)))


shapiro.test(growth40$values)
#Data is not normal 
t.test(growth40NS$values, growth40S$values, var.equal = FALSE)
#Salt and no salt are NOT different modalities for 40


###
#60 g/L
###

growth60NS <- rbind(subset.data.frame(table3, table3$`Cycle Nr.` =='60 no salt 1'),
                    subset.data.frame(table3, table3$`Cycle Nr.` =='60 no salt 2'),
                    subset.data.frame(table3, table3$`Cycle Nr.` =='60 no salt 3'))

growth60NS <- data.frame(t(growth60NS[-1]))
growth60NS <- data.frame(values = c(growth60NS$X1,growth60NS$X2,growth60NS$X3),
                         names = c(rep('60NS1',39),rep('60NS 2',39),rep('60NS 3',39)))

comparaison24 <- mean((growth60NS$values))

#We know that population is not normal 
#Student t test pairwise
t.test(growth60NS$values, mu = comparaison24, var.equal = FALSE)
##all replicates are comparable to the mean

growth60S <- rbind(subset.data.frame(table3, table3$`Cycle Nr.` =='60 salt 1'),
                   subset.data.frame(table3, table3$`Cycle Nr.` =='60 salt 2'),
                   subset.data.frame(table3, table3$`Cycle Nr.` =='60 salt 3'))
growth60S <- data.frame(t(growth60S[-1]))
growth60S <- data.frame(values = c(growth60S$X1,growth60S$X2,growth60S$X3),
                        names = c(rep('60S 1',39),rep('60S 2',39),rep('60S 3',39)))

comparaison25 <- mean((growth60S$values))

#We know that population is not normal 
#Student t test pairwise
t.test(as.numeric(growth60S$values), mu = comparaison25 , var.equal = FALSE)
#all replicates are comparable to the mean

growth60 <- data.frame(values = c(growth60NS$values, growth60S$values),
                       names = c(rep('no salt',117),rep('salt',117)))


shapiro.test(growth60$values)
#Data is not normal 
t.test(growth60NS$values, growth60S$values, var.equal = FALSE)
#Salt and no salt are different modalities for 60

###
#80 g/L
###

growth80NS <- rbind(subset.data.frame(table3, table3$`Cycle Nr.` =='80 no salt 1'),
                    subset.data.frame(table3, table3$`Cycle Nr.` =='80 no salt 2'),
                    subset.data.frame(table3, table3$`Cycle Nr.` =='80 no salt 3'))

growth80NS <- data.frame(t(growth80NS[-1]))
growth80NS <- data.frame(values = c(growth80NS$X1,growth80NS$X2,growth80NS$X3),
                         names = c(rep('80NS1',39),rep('80NS 2',39),rep('80NS 3',39)))

comparaison26 <- mean((growth80NS$values))

#We know that population is not normal 
#Student t test pairwise
t.test(growth80NS$values, mu = comparaison26, var.equal = FALSE)
##all replicates are comparable to the mean

growth80S <- rbind(subset.data.frame(table3, table3$`Cycle Nr.` =='80 salt 1'),
                   subset.data.frame(table3, table3$`Cycle Nr.` =='80 salt 2'),
                   subset.data.frame(table3, table3$`Cycle Nr.` =='80 salt 3'))
growth80S <- data.frame(t(growth80S[-1]))
growth80S <- data.frame(values = c(growth80S$X1,growth80S$X2,growth80S$X3),
                        names = c(rep('80S 1',39),rep('80S 2',39),rep('80S 3',39)))

comparaison27 <- mean((growth80S$values))

#We know that population is not normal 
#Student t test pairwise
t.test(as.numeric(growth80S$values), mu = comparaison27 , var.equal = FALSE)
#all replicates are comparable to the mean

growth80 <- data.frame(values = c(growth80NS$values, growth80S$values),
                       names = c(rep('no salt',117),rep('salt',117)))


shapiro.test(growth80$values)
#Data is not normal 
t.test(growth80NS$values, growth80S$values , var.equal = FALSE)
#Salt and no salt are different modalities for 80


###
#Testing for a difference between NS growth rate
#No data is normally distributed therefore all t.test
###

growthNS24 <- data.frame(values = c(growthmedia$values, growth0NS$values, growth20NS$values, growth40NS$values, growth60NS$values, growth80NS$values),
                         names = c(rep('growth media',234),rep('growth 0',117),rep('growth 20',117),rep('growth 40',117),rep('growth 60',117), rep('growth 80', 117)))

t.test(growthNS24$values, var.equal = FALSE)
#At least one of the data set is different 

boxplot(growthNS24$values~growthNS24$names)
pairwise.t.test(x= growthNS24$values, g= growthNS24$names, p.adjust.method = 'bonferroni')

#growth 0, 20 and 40 are comparable when no salt 
#growth 60 and 80 are different from the first 3 and within each other

###
#Testing for a difference between S growth rate
#No data is normally distributed therefore all t.test
###

growthS24 <- data.frame(values = c(growth0S$values, growth20S$values, growth40S$values, growth60S$values, growth80S$values),
                      names = c(rep('growth 0',117),rep('growth 20',117),rep('growth 40',117),rep('growth 60',117), rep('growth 80', 117)))

growth <- data.frame(values = c(growthS24$values, growthNS24$values),
                     names = c(rep('S',585), rep('NS', 819)))
t.test(growthS24$values, var.equal = FALSE)
#At least one of the data set is different 
t.test(growth$values, var.equal = FALSE)
boxplot(growthS24$values~growthS24$names)
pairwise.t.test(x= growthS24$values, g= growthS24$names, p.adjust.method = 'bonferroni')

#growth 20 and 40 are comparable when salt 
#growth 0, 60 and 80 are different from the first 2 and within each other

###
#Comparing for graphical observation
###

t.test(x= as.numeric(growth0S$values), y = as.numeric(growth0NS$values), alternative = 'greater')
t.test(x= as.numeric(growth20S$values), y = as.numeric(growth40NS$values), alternative = 'greater')
t.test(x= as.numeric(growth40S$values), y = as.numeric(growth40NS$values), alternative = 'greater')
t.test(x= as.numeric(growth60S$values), y = as.numeric(growth60NS$values), alternative = 'greater')
t.test(x= as.numeric(growth80S$values), y = as.numeric(growth80NS$values), alternative = 'greater')

t.test(x= as.numeric(growth0NS$values), y = as.numeric(growth20S$values), alternative = 'greater')
t.test(x= as.numeric(growth80S$values), y = as.numeric(growth60NS$values))
t.test(x= as.numeric(growth0NS$values), y = as.numeric(growth20S$values))
