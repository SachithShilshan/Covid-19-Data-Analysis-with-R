# AS2018555
# STA 326 2.0  Assignment 2

#Step 1
library(devtools)

#Step 2
devtools::install_github("thiyangt/sta3262")

#Step 3
library(sta3262)

#Step 4
get_assignment_data("AS2018555") # this displays the dataset

#Step 5
get_assignment_questions("2018555")


#$q1
#[1] "Obtain summary statistics for each variable and interpret the results.

mydata <- get_assignment_data("AS2018555")

summary(mydata)
plot(
  mydata$date,
  mydata$cases,
  xlab = "date",
  ylab = "cases",
  col = factor(mydata$type)
)

confirmed = mydata[type == "confirmed",]
confirmed
death = mydata[type == "death",]
death
recovered = mydata[type == "recovered",]
recovered

a <- cbind(confirmed$cases, death$cases, recovered$cases)
colnames(a) <- c("confirmed cases", "death cases", "recovered cases")   # change all colnames
stat.desc(a)


#$q2
#[1] "Draw a timeseries plot using the qplot function to visualize changes in Covid-19 death casesover time."

library(ggplot2)
qplot(data = death,
      y = cases,
      x = date,
      geom = "line") + ylab("death cases")


#$q3
#[1] "Draw a scatterplot using the qplot to visualize the relationship between Covid-19 confirmed cases and recovered cases. Compute the corresponding Pearson's correlation coefficient."

qplot(confirmed$cases, recovered$cases) + xlab("confirmed cases") + ylab("recovered cases")
cor(confirmed$cases, recovered$cases)


#$q4
#[1] "In the year 2021, which date has the lowest number of death cases?"

death2021 <- death[death$date >= "2021-01-01" & death$date <=  "2021-05-27", ]
death2021
print(death2021[death2021$cases == min(death2021$cases),])

#$q5
#[1] "In 2021, which dates are almost the same in the number of Covid-19 recovered cases?"

recovered2021 <- recovered[recovered$date >= "2021-01-01" & recovered$date <=  "2021-05-27", ]
recovered2021

for (i in 1:max(recovered2021$cases)) {
  s <- recovered2021[recovered2021$cases == i,]
  if (nrow(s) > 1) {
    out <- paste0("In 2021, the number of Covid-19 recovered cases = ", i, ".")
    print(out)
    print(s)
  }
}


#$q6
#[1] "Create a new dataframe called "newcovid", that contains rows of only recovered cases."

newcovid <- data.frame(recovered)
newcovid
