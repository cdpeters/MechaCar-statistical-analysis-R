# **MechaCar Statistical Analysis in R**

## **Overview of Project**
In this project, several statistical concepts and tests are applied to a car
manufacturing dataset from a fictional company known as MechaCar. Statistical
tests are applied to predict mpg, review sample distributions of suspension coil
weight capacities, and even to designing a study to compare performance of the
MechaCar against its competitors.

The main analysis file is the following RScript file
*[MechaCarChallenge.R](/MechaCarChallenge.R)*.

### **Linear Regression to Predict MPG**
The analysis begins with an exploration to see if a linear model would be
appropriate for predicting the miles per gallon (mpg) rating for the prototype
cars. A multiple linear regression analysis was performed with vehicle length,
and weight, spoiler angle, ground clearance, and AWD designation as independent
variables, and the mpg rating as the dependent variable. The results are shown
here:

<div align="center">
    <img src="images/linear_regression_summary_output.svg"
         alt="linear regression summary" />
</div>

From the output of the linear regression summary above it is observed that both
vehicle length and ground clearance provide a non-random amount of variation in
the data as they are below the 0.001 or 0.1% significance level. The y-intercept
is also below this significance level, however, since no cars of interest
produce a situation in which all of these variables are zero, the intercept has
no intrinsic meaning here. Looking at the remaining variables, it is seen that
vehicle weight contributes non-random variance at the 0.1 or 10% significance
level. The variables spoiler angle and AWD contribute random variance.

Because the p-value is extremely small, lower than the 0.001 significance level,
we can reject the null hypothesis that the slope of this linear model is zero.

Furthermore, the high r-squared value of 0.71 tells us that this model can be
decently effective at predicting the mpg of MechaCar prototypes. Taking the
square root, we get an approximate r-value of 0.85 which indicates that mpg has
a strong linear correlation to our variables.

### **Summary Statistics on Suspension Coils**
To determine if the manufacturing of suspension coils is consistent across
different lots in production, the summary statistics for the coil's weight
capacity (in pounds per square inch PSI) in total and across all lots are
presented here. It should be noted that the specification for the variance of
the suspension coils weight capacity distribution should not exceed 100 PSI.

#### *Summary Statistics: Total*
<div align="center">
    <img src="images/suspension_coil_total_summary.png"
         alt="suspension coil total summary" />
</div>

#### *Summary Statistics: By Lot*
<div align="center">
    <img src="images/suspension_coil_lot_summary.png"
         alt="suspension coil lot summary" />
</div>

We see here that for the total distribution coil weight capacity the variance is
62.3 PSI which is below the design specification limit. However, in the by lot
breakdown, we see that lot 3 has a variance equal to 170.3 PSI, which greatly
exceeds the design limit. In total, the specification is met, but on an
individual lot basis, lot 3 fails to meet the design limits for weight capacity
variance.

### **T-Tests on Suspension Coils**
Taking a further look at the distributions of suspension coil weight capacities,
a one sample t-test is performed for the entire manufacturing sample and for
each lot individually. The first t-test output shown here is for the entire
sample (all three lots together):

<div align="center">
    <img src="images/t-test_total.svg"
         alt="suspension coil lot summary" />
</div>

The p-value of 0.06 for the total sample is above the 0.05 significance level
and thus there is no significant difference between the mean of the total sample
versus the mean of the population.

Below are the one-sample t-tests for each of the three manufacturing lots:

<div align="center">
    <img src="images/t-test_lot1.svg"
         alt="suspension coil lot summary" />
</div>

Lot 1 shows a p-value of 1, very much above the significance level of 0.05 (the
mean of Lot 1 is exactly the mean of the population) therefore there is no
statistically significant difference between the sample and population means.

<div align="center">
    <img src="images/t-test_lot2.svg"
         alt="suspension coil lot summary" />
</div>

Lot 2 has a p-value of 0.61, also well above the significance level of 0.05 and
thus the lot 2's mean is not statistically different than the population mean of
1500.

<div align="center">
    <img src="images/t-test_lot3.svg"
         alt="suspension coil lot summary" />
</div>

Lot 3 has a p-value of 0.042 which is below the threshold of a 0.05 significance
level and thus we can reject the null hypothesis that the means are not
different. In conclusion, the sample mean for lot 3 of 1496.14 is statistically
different than the population mean of 1500.

### **Study Design: MechaCar vs Competition**
In order to inform the design process for the MechaCar prototypes, it is
important to quantify the performance of these prototypes in comparison to the
offerings from competing manufacturers. The following is an example of a study
that can be performed to do exactly that.

The main performance metric for this study will be safety rating across
manufacturers of similar designs (horsepower, size, weight, fuel efficiency,
engine type) with additional exploration into the relation between safety rating
and maintenance cost. I am curious if there is a statistically significant
difference between the MechaCar prototypes and the competition in terms of their
safety performance, and what that difference actually is. In regards to
maintenance cost, it would be interesting to see if higher safety ratings are
correlated to higher maintenance cost for the MechaCar and cars of similar
design across manufacturers. Perhaps the safety systems in higher safety rated
cars are more complex requiring more work (or perhaps higher quality safety
systems actually require less work).

The following are two sets of hypotheses for the study:

1. Safety rating across manufacturers:

    - $H_{0}$: The mean safety rating of the MechaCar design is less than the
      mean safety of the competitor's design.
        - this is assuming there is a sample of cars (n>1) that are tested for
          safety ratings per each manufacturer, i.e. an actual distribution of
          safety ratings exists.
        - this hypothesis should be evaluated once for each competitor's
          distribution versus MechaCar's distribution.
    - $H_{a}$: The mean safety rating of the MechaCar design is greater than the
      mean safety of the competitor's design.
    - Here, a one-tailed hypothesis is chosen because it is assumed that the
      consumer wants to know, in general, if the safety of the car they're
      considering is greater than the competition.

2. Safety rating and maintenance cost:
    - $H_{0}$: The slope of the linear model between safety rating (independent
      variable) and maintenance cost (dependent variable) is zero.
    - $H_{a}$: The slope of the linear model between safety rating (independent
      variable) and maintenance cost (dependent variable) is not zero.

What statistical test would you use to test the hypothesis? And why?

To test the first set of hypotheses regarding safety rating across
manufacturers, a two-sample t-test would be appropriate since we are comparing
two samples from different populations (the population of MechaCars and the
population of one of the competitors cars), i.e. a pair t-test. Specifically our
t-test would be one-tailed since we are interested in the situation where the
MechaCar safety rating is greater than a similar design from the competition.

To test the second set of hypotheses, a linear regression analysis would be
appropriate since we are looking to see if a linear correlation between two
continuous variables, safety rating and maintenance cost, exists or not. The
goal would be to see if safety rating influences the cost of upkeep for cars of
a similar class.

The data that would be required for the first hypotheses would be a sample of
cars from the MechaCar population and a sample from a competitor's population
for a car of similar design. With these samples, safety ratings from testing
would need to be gathered yielding the distributions. From here, the t-tests can
be performed. For the second set of hypotheses, average safety ratings for cars
in a similar class and their corresponding estimated maintenance cost would be
needed for the regression analysis.
