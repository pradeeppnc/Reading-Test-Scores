pisaTrain = read.csv(choose.files())
pisaTest = read.csv(choose.files())
tapply(pisaTrain$readingScore, pisaTrain$male, mean)
summary(pisaTrain)
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
str(pisaTrain$male)
str(pisaTrain$raceeth)
str(pisaTrain$grade)
table(pisaTrain$grade)
sort(table(pisaTrain$raceeth))
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
str(pisaTrain$raceeth)
str(pisaTrain)
lmScore = lm(readingScore ~ grade + male + raceeth + preschool + expectBachelors + motherHS + motherBachelors + motherWork + fatherHS + fatherBachelors + fatherWork + selfBornUS + motherBornUS + fatherBornUS + englishAtHome + computerForSchoolwork + read30MinsADay + minutesPerWeekEnglish + studentsInEnglish + schoolHasLibrary + publicSchool + urban + schoolSize + readingScore, data = pisaTrain)
lmScore = lm(readingScore ~., data = pisaTrain)
summary(lmScore)
SSE = sum(lmScore$residuals^2)
SSE
RMSE = sqrt(SSE/nrow(pisaTrain))
RMSE
predTest = predict(lmScore, newdata = pisaTrain)
summary(predTest)
range(predTest)
SSE = sum((predTest-pisaTest$readingScore)^2)
SSE
RMSE = sqrt(mean((predTest-pisaTest$readingScore)^2))
RMSE

AVG = mean(pisaTrain$readingScore)
AVG
SST =sum((AVG-pisaTest$readingScore)^2)
1-AVG/SST