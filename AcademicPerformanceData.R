
#Read the file 
EduData = read.csv('xAPI-Edu-Data.csv')

#Check data types of  attributes
lapply(EduData, class)

#Check Summary statistics of EduData 
summary(EduData)

#How much % of the students constitute high-level students
nrow(EduData[EduData$Class == 'H',])/nrow(EduData)

#How much % of the students constitute mid-level students
nrow(EduData[EduData$Class == 'M',])/nrow(EduData) 

#How much % of the students constitute mid-level students
nrow(EduData[EduData$Class == 'L',])/nrow(EduData)

#Mid-level students comprises of 44% of the population, High-level students comprises of 29.5% of the population, Low-level students comprises of 26.4% of the population

#Check stats of low level students (class = 'L'). So, take a subset with low-level class students
EduDataLow = EduData[EduData$Class == 'L',]

#Check correlation between the 2 categorical variables using Chi-squared test
table(EduData$StudentAbsenceDays, EduData$Class)
chisq.test(EduData$StudentAbsenceDays, EduData$Class)
#From the above test, we can deduce that relation correlation between Student Absence Days and Class is strong. So, we could possibly hypothesize that students who have more absent days are low-level students, while the ones with lower absent days are high-level students. Let's verify this.

#Cramer's V test
CVfunction = function(nrows,ncols)
{
  CV = sqrt(chisq.test(EduData$StudentAbsenceDays, EduData$Class)$statistic)/length()
}

length(unique(EduData$StudentAbsenceDays))
