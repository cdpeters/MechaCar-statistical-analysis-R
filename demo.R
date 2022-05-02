library(tidyverse)
library(jsonlite)

# read in a demo csv file
demo_table <- read.csv(file='demo.csv',check.names=F,stringsAsFactors = F)
# read in a demo json file
demo_table2 <- fromJSON(txt='demo.json')


# Sampling from a 2D data structure -------------------------
# capture number of rows
num_rows <- 1:nrow(demo_table)
# sample 3 of those rows
sample_rows <- sample(num_rows, 3)
# retrieve the requested data within the demo_table
demo_table[sample_rows,]

# mutating demo_table, add columns to original data frame
demo_table <- demo_table %>%
                mutate(Mileage_per_Year=Total_Miles/(2020-Year),IsActive=TRUE) 

# summarize our demo_table2
summarize_demo <- demo_table2 %>%
                    group_by(condition) %>%
                    summarize(Mean_Mileage=mean(odometer), .groups = 'keep')

# another summary example
summarize_demo2 <- demo_table2 %>%
                     group_by(condition) %>%
                     summarize(Mean_Mileage=mean(odometer),
                               Maximum_Price=max(price),
                               Num_Vehicles=n(),
                               .groups = 'keep')

# reshaping example
demo_table3 <- read.csv('demo2.csv',check.names = F,stringsAsFactors = F)
# reshape as a long table
# long_table <- gather(demo_table3,key="Metric",value="Score",buying_price:popularity)
long_table <- demo_table3 %>% gather(key="Metric",value="Score",buying_price:popularity)

wide_table <- long_table %>% spread(key="Metric",value="Score")

# plotting --------------------------------------------------------------------
# plot 1
# import dataset into ggplot2
plt <- ggplot(mpg,aes(x=class))
plt + geom_bar() #plot a bar plot

# create summary table
mpg_summary <- mpg %>%
                 group_by(manufacturer) %>%
                 summarize(Vehicle_Count=n(), .groups = 'keep')


# plot 2
#import dataset into ggplot2
plt <- ggplot(mpg_summary,aes(x=manufacturer,y=Vehicle_Count))

# plot a bar plot
plt + geom_col() 

# plot bar plot with axis labels
plt + geom_col() + xlab("Manufacturing Company") + ylab("Number of Vehicles in Dataset") 

# plot a bar plot with rotated labels. Rotate the x-axis label 45 degrees
plt +
  geom_col() +
  xlab("Manufacturing Company") +
  ylab("Number of Vehicles in Dataset") +
  theme(axis.text.x=element_text(angle=45,hjust=1))


# plot 3
# create summary table
mpg_summary <- subset(mpg,manufacturer=="toyota") %>%
                 group_by(cyl) %>%
                 summarize(Mean_Hwy=mean(hwy), .groups = 'keep')

# import dataset into ggplot2
plt <- ggplot(mpg_summary,aes(x=cyl,y=Mean_Hwy))

# line chart
plt + geom_line()

# add line plot with labels
plt + geom_line() + scale_x_discrete(limits=c(4,6,8)) + scale_y_continuous(breaks = c(15:30))


# plot 4
# import dataset into ggplot2
plt <- ggplot(mpg,aes(x=displ,y=cty)) 

# add scatter plot with labels
plt + geom_point() + xlab("Engine Size (L)") + ylab("City Fuel-Efficiency (MPG)")


# plot 5
# import dataset into ggplot2
plt <- ggplot(mpg,aes(x=displ,y=cty,color=class))

#add scatter plot with labels
plt + geom_point() + labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class")


# plot 6
# import dataset into ggplot2
plt <- ggplot(mpg,aes(x=displ,y=cty,color=class,shape=drv))

#add scatter plot with multiple aesthetics
plt + geom_point() + labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class",shape="Type of Drive")


# plot 7
# import dataset into ggplot2
plt <- ggplot(mpg,aes(y=hwy)) 

# add boxplot
plt + geom_boxplot()


# plot 8
#import dataset into ggplot2
plt <- ggplot(mpg,aes(x=manufacturer,y=hwy))

#add boxplot and rotate x-axis labels 45 degrees
plt + geom_boxplot() + theme(axis.text.x=element_text(angle=45,hjust=1))


# plot 9
# create summary table
mpg_summary <- mpg %>%
                 group_by(class,year) %>%
                 summarize(Mean_Hwy=mean(hwy), .groups = 'keep') 

plt <- ggplot(mpg_summary, aes(x=class,y=factor(year),fill=Mean_Hwy))

# create heatmap with labels
plt + geom_tile() + labs(x="Vehicle Class",y="Vehicle Year",fill="Mean Highway (MPG)") 


# plot 10
# create summary table
mpg_summary <- mpg %>%
                 group_by(model,year) %>%
                 summarize(Mean_Hwy=mean(hwy), .groups = 'keep')

# import dataset into ggplot2
plt <- ggplot(mpg_summary, aes(x=model,y=factor(year),fill=Mean_Hwy)) 

# add heatmap with labels. Rotate x-axis labels 90 degrees
plt +
  geom_tile() +
  labs(x="Model",y="Vehicle Year",fill="Mean Highway (MPG)") +
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=.5)) 


# plot 11 layering
plt <- ggplot(mpg,aes(x=manufacturer,y=hwy)) #import dataset into ggplot2
plt +
  geom_boxplot() + #add boxplot
  theme(axis.text.x=element_text(angle=45,hjust=1)) + #rotate x-axis labels 45 degrees
  geom_point() #overlay scatter plot on top


# plot 12 layering
# create summary table
mpg_summary <- mpg %>%
                 group_by(class) %>%
                 summarize(Mean_Engine=mean(displ), .groups = 'keep')

# import dataset into ggplot2
plt <- ggplot(mpg_summary,aes(x=class,y=Mean_Engine))

# add scatter plot
plt + geom_point(size=4) + labs(x="Vehicle Class",y="Mean Engine Size")


# plot 13 layering
mpg_summary <- mpg %>%
                 group_by(class) %>%
                 summarize(Mean_Engine=mean(displ),SD_Engine=sd(displ), .groups = 'keep')

# import dataset into ggplot2
plt <- ggplot(mpg_summary,aes(x=class,y=Mean_Engine))


plt +
  geom_point(size=4) +
  labs(x="Vehicle Class",y="Mean Engine Size") + #add scatter plot with labels
  geom_errorbar(aes(ymin=Mean_Engine-SD_Engine,ymax=Mean_Engine+SD_Engine)) #overlay with error bars


# plot 14 faceting
# plot a (no faceting)
# convert to long format
mpg_long <- mpg %>% gather(key="MPG_Type",value="Rating",c(cty,hwy))

head(mpg_long)

# import dataset into ggplot2
plt <- ggplot(mpg_long,aes(x=manufacturer,y=Rating,color=MPG_Type))

# add boxplot with labels rotated 45 degrees
plt + geom_boxplot() + theme(axis.text.x=element_text(angle=45,hjust=1))

# plot b (faceting)
#import dataset into ggplot2
plt <- ggplot(mpg_long,aes(x=manufacturer,y=Rating,color=MPG_Type))

plt +
  geom_boxplot() +
  facet_wrap(vars(MPG_Type)) + #create multiple boxplots, one for each MPG type
  theme(axis.text.x=element_text(angle=45,hjust=1),legend.position = "none") +
  xlab("Manufacturer") #rotate x-axis labels


# test for normality --------------------------------------------------------
# qualitative
ggplot(mtcars,aes(x=wt)) + geom_density() #visualize distribution using density plot

# quantitative
shapiro.test(mtcars$wt)



# Random sampling -----------------------------------------------------------

# import used car dataset
population_table <- read.csv('used_car_data.csv',check.names = F,stringsAsFactors = F)
#import dataset into ggplot2
plt <- ggplot(population_table,aes(x=log10(Miles_Driven)))
#visualize distribution using density plot
plt + geom_density()

# randomly sample 50 data points
sample_table <- population_table %>% sample_n(50)
# import dataset into ggplot2
plt <- ggplot(sample_table,aes(x=log10(Miles_Driven)))
# visualize distribution using density plot
plt + geom_density() 


# one-sample t-test ----------------------------------------------------------
# compare sample versus population means
t.test(log10(sample_table$Miles_Driven),mu=mean(log10(population_table$Miles_Driven))) 

# two-sample t-test ----------------------------------------------------------
# generate 50 randomly sampled data points
sample_table <- population_table %>% sample_n(50)
# generate another 50 randomly sampled data points
sample_table2 <- population_table %>% sample_n(50)
# compare means of two samples
t.test(log10(sample_table$Miles_Driven),log10(sample_table2$Miles_Driven))

# paired two-sample t-test ---------------------------------------------------
# import dataset
mpg_data <- read.csv('mpg_modified.csv')
# select only data points where the year is 1999
mpg_1999 <- mpg_data %>% filter(year==1999)
# select only data points where the year is 2008
mpg_2008 <- mpg_data %>% filter(year==2008)
# compare the mean difference between two samples
t.test(mpg_1999$hwy,mpg_2008$hwy,paired = T)

# ANOVA test -----------------------------------------------------------------
# filter columns from mtcars dataset
mtcars_filt <- mtcars[,c("hp","cyl")]
# convert numeric column to factor
mtcars_filt$cyl <- factor(mtcars_filt$cyl)
# compare means across multiple levels
aov(hp ~ cyl,data=mtcars_filt)

summary(aov(hp ~ cyl,data=mtcars_filt))

# correlation coefficient ----------------------------------------------------
head(mtcars)
# import dataset into ggplot2
plt <- ggplot(mtcars,aes(x=hp,y=qsec))
# create scatter plot
plt + geom_point()
# calculate correlation coefficient
cor(mtcars$hp,mtcars$qsec)

# read in dataset
used_cars <- read.csv('used_car_data.csv',stringsAsFactors = F)
head(used_cars)

# import dataset into ggplot2
plt <- ggplot(used_cars,aes(x=Miles_Driven,y=Selling_Price))
# create a scatter plot
plt + geom_point()
# calculate correlation coefficient
cor(used_cars$Miles_Driven,used_cars$Selling_Price)

# Correlation matrix --------------------------------------------------------
# convert data frame into numeric matrix
used_matrix <- as.matrix(used_cars[,c("Selling_Price","Present_Price","Miles_Driven")])
cor(used_matrix)

# Simple linear regression --------------------------------------------------
# create linear model
lm(qsec ~ hp,mtcars)
# summarize linear model
summary(lm(qsec~hp,mtcars))

# create linear model
model <- lm(qsec ~ hp,mtcars)
# determine y-axis values from linear model
yvals <- model$coefficients['hp']*mtcars$hp +
  model$coefficients['(Intercept)']

# import dataset into ggplot2
plt <- ggplot(mtcars,aes(x=hp,y=qsec))
# plot scatter and linear model
plt + geom_point() + geom_line(aes(y=yvals), color = "red")

# Multiple linear regression ------------------------------------------------
# generate multiple linear regression model
lm(qsec ~ mpg + disp + drat + wt + hp,data=mtcars)
# generate summary statistics
summary(lm(qsec ~ mpg + disp + drat + wt + hp,data=mtcars))

# Chi-squared test ----------------------------------------------------------
# generate contingency table
table(mpg$class,mpg$year)
# generate contingency table
tbl <- table(mpg$class,mpg$year)
#compare categorical distributions
chisq.test(tbl)





