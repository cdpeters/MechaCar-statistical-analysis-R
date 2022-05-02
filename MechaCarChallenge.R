library(dplyr)

#############################
# Deliverable 1
#############################

# Read in Data -----------------------------------------------------------------
# Read in MechaCar_mpg csv file
MechaCar_mpg <- read.csv(file='MechaCar_mpg.csv',
                         check.names=F,
                         stringsAsFactors=F)

# Multiple linear regression ---------------------------------------------------
# generate multiple linear regression model
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,
   data=MechaCar_mpg)
# generate summary statistics
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,
           data=MechaCar_mpg))


#############################
# Deliverable 2
#############################

# Read in Data -----------------------------------------------------------------
# Read in MechaCar_mpg csv file
Suspension_Coil <- read.csv(file='Suspension_Coil.csv',
                         check.names=F,
                         stringsAsFactors=F)

total_summary <- Suspension_Coil %>%
                 summarize(Mean=mean(PSI),
                           Median=median(PSI),
                           Variance=var(PSI),
                           SD=sd(PSI))

lot_summary <- Suspension_Coil %>%
               group_by(Manufacturing_Lot) %>%
               summarize(Mean=mean(PSI),
                         Median=median(PSI),
                         Variance=var(PSI),
                         SD=sd(PSI))
               
               