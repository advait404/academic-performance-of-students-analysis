# Advait Sinha
# TP054575


# Package Installation and Loading
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readr")
install.packages("plotrix")
install.packages("crayon")
install.packages("tibble")

library(ggplot2)
library(dplyr)
library(readr)
library(plotrix)
library(crayon)
library(tibble)


# read_delim saves CSV as a tibble
student <- read_delim(file = "C:\\Users\\advai\\Downloads\\APU\\Degree Year 2\\Semister 1\\Programming For Data Analysis\\Assignment\\student.csv", delim = ";", quote = "")

#initialization
tempcol = c()
student_perf = data.frame(index = 1:nrow(student))

# Replace all double quote by empty string
for(col in 1:length(student)){
  tempcol = c()
  for(row in student[,col]){
    temp = gsub ("\"","", row)
    tempcol = c(tempcol,temp)
  }
  student_perf = cbind(student_perf, tempcol)
}


# Adding column name of cleaned data frame
colname = c("index", names(student))
names(student_perf) = colname
student = student_perf


#correcting data type of each attribute
student$school <- factor(student$school, levels=c("GP","MS"), labels=c("GP","MS"))
student$sex <- factor(student$sex, levels=c("M","F"), labels=c("M","F"))
student$age <- as.integer(student$age)
student$address <- factor(student$address, levels=c("U","R"), labels=c("U","R"))
student$famsize <- factor(student$famsize , levels=c("LE3","GT3"), labels=c("LE3","GT3"))
student$Pstatus <- factor(student$Pstatus , levels=c("T","A"), labels=c("T","A"))
student$Medu <- as.integer(student$Medu)
student$Fedu <- as.integer(student$Fedu)
student$Mjob <- factor(student$Mjob , levels=c("teacher","health","services","at_home","other"), labels=c("teacher","health","services","at_home","other"))
student$Fjob <- factor(student$Fjob , levels=c("teacher","health","services","at_home","other"), labels=c("teacher","health","services","at_home","other"))
student$reason <- factor(student$reason,levels=c("home","reputation","course","other"), labels=c("home","reputation","course","other"))
student$guardian <- factor(student$guardian, levels=c("mother","father","other"), labels=c("mother","father","other"))
student$traveltime<- as.integer(student$traveltime)
student$studytime <- as.integer(student$studytime)
student$failures <- as.integer(student$failures)
student$schoolsup <- factor(student$schoolsup, levels=c("yes","no"), labels=c("yes","no"))
student$famsup <- factor(student$famsup, levels=c("yes","no"), labels=c("yes","no"))
student$paid <- factor(student$paid, levels=c("yes","no"), labels=c("yes","no"))
student$activities <- factor(student$activities, levels=c("yes","no"), labels=c("yes","no"))
student$nursery <- factor(student$nursery, levels=c("yes","no"), labels=c("yes","no"))
student$higher <- factor(student$higher, levels=c("yes","no"), labels=c("yes","no"))
student$internet <- factor(student$internet, levels=c("yes","no"), labels=c("yes","no"))
student$romantic <- factor(student$romantic, levels=c("yes","no"), labels=c("yes","no"))
student$famrel <- as.integer(student$famrel)
student$freetime <- as.integer(student$freetime)
student$goout <- as.integer(student$goout)
student$Dalc <- as.integer(student$Dalc)
student$Walc <- as.integer(student$Walc)
student$health <- as.integer(student$health)
student$absences <- as.integer(student$absences)
student$G1 <- as.integer(student$G1)
student$G2 <- as.integer(student$G2)
student$G3 <- as.integer(student$G3)


#Renaming column names
colnames(student) <- c("index","uni_name","gender","age","address_type","family_size","parent_living_status","mother_edu","father_edu","mother_job","father_job","enrollment_reason","guardian","travel_time","study_time","class_failures","uni_support","family_support","extra_classes","extra_activities","attended_nursery","higher_edu_interest","internet_access","romantic_relationship","family_relationship","free_time","outgoing","weekday_alcohol","weekend_alcohol","health","uni_absences","grades_year_1","grades_year_2","grades_year_3")


#overall_grade col averaging G1,G2,G3 marks
overall_grade = round((student$grades_year_1+student$grades_year_2+student$grades_year_3)/3)
student=cbind(student,overall_grade)

#alcohol_consumption col averaging weekday_alcohol and weekend_alcohol
alcohol_consumption = round((student$weekday_alcohol+student$weekend_alcohol)/2,3)
student=cbind(student,alcohol_consumption)

#grade_type col categorizing overall_grade into distinction, normal, failure 
student <- student %>%
  add_column(grade_type = NA)
student$grade_type[]<- 'N' 
student$grade_type[student$overall_grade<=5]<- 'F'
student$grade_type[student$overall_grade>=15]<- 'D' 
student$grade_type <- factor(student$grade_type, levels=c("D","N","F"), labels=c("D","N","F"))


#Testing; extras for preprocessing
ggplot(student,aes(x=overall_grade)) +
  geom_freqpoly(stat="bin",binwidth=0.6)

View(student)
head(student,4)
summary(student)
names(student)

class(student)
length(student)
nrow(student)
select(student,c(overall_grade,alcohol_consumption,grade_type))





# Question 1: Does address_type affect distinction or failure rates among students
# Analysis 1-1:  Finding the relationship between overall_grade and address_type
ggplot(student, aes(y=overall_grade, x=address_type)) + 
  geom_boxplot(aes(fill=address_type),color="purple",show.legend=TRUE)


# Analysis 1-2:  Comparing the observation number of overall_grade and address_type
count_address_type = c()
count_address_type[1] <- (count(student,address_type=="U"))[2,2]
count_address_type[2] <- (count(student,address_type=="R"))[2,2]
labels_count_address_type = c(paste("U -",count_address_type[1]),paste("R -",count_address_type[2]))
pie(count_address_type,main="uni_name",labels=labels_count_address_type,col=c("red","blue"))


# Analysis 1.3: Plot address_type with distinction grades
filter(student,grade_type=="D") %>%
ggplot(aes(x=overall_grade)) +
  geom_histogram(stat="bin",binwidth=0.5,color="purple",aes(fill=address_type))


# Analysis 1.4: plot address_type with failure overall_grade
filter(student,grade_type=="F") %>%
ggplot(aes(x=overall_grade)) +
  geom_histogram(stat="bin",binwidth=0.5,color="purple",aes(fill=address_type))





# Question 2:	what factors in rural address_type negatively affect grades
# Analysis 2-1: relationship between internet access and address_type
ggplot(student %>% group_by(address_type,internet_access) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=internet_access, y=perc,fill=as.factor(internet_access))) +
  geom_bar(stat="identity",color="purple",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1.2))+
  facet_grid(~address_type)


# Analysis 2-2:  Relationship between uni_support and address_type
ggplot(student %>% group_by(address_type,uni_support) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=uni_support, y=perc,fill=as.factor(uni_support))) +
  geom_bar(stat="identity",color="purple",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1.2))+
  facet_grid(~address_type)


# Analysis 2-3:  Relationship between family_support and address_type famsup
ggplot(student %>% group_by(address_type,family_support) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=family_support, y=perc,fill=as.factor(family_support))) +
  geom_bar(stat="identity",color="purple",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1.2))+
  facet_grid(~address_type)


# Analysis 2-4: Relationship between study_time and address_type 
student %>%  
  group_by(address_type)%>%  
  summarise(mean_study_time = mean(study_time))

ggplot(student %>% group_by(address_type,study_time) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=study_time, y=perc,fill=as.factor(study_time))) +
  geom_bar(stat="identity",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1.2))+
  facet_grid(~address_type)


# Analysis 2-5:  Relationship between travel_time and address_type 
student %>%  
  group_by(address_type)%>%  
  summarise(mean_travel_time = mean(travel_time))

ggplot(student %>% group_by(address_type,travel_time) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=travel_time, y=perc,fill=as.factor(travel_time))) +
  geom_bar(stat="identity",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1.2))+
  facet_grid(~address_type)


# Analysis 2-6:  Relationship between extra_classes and address_type
ggplot(student %>% group_by(address_type,extra_classes) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=extra_classes, y=perc,fill=as.factor(extra_classes))) +
  geom_bar(stat="identity",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1.2))+
  facet_grid(~address_type)


# Analysis 2-7:  Relationship between age and address_type 
ggplot(student, aes(y=overall_grade, x=age)) + 
  geom_count(aes(color=address_type))+ 
  facet_grid(~address_type)+
  labs(title = 'Counting Overlapping Points using Scatterplot of Overall Grade and Age', x= 'Age of Students', y = 'Overall Grade of Students')


# Analysis 2-8:  Relationship between health and address_type 
student %>%  
  group_by(address_type)%>%  
  summarise(mean_health = mean(health))

ggplot(student %>% group_by(address_type,health) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=health, y=perc,fill=as.factor(health))) +
  geom_bar(stat="identity",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1.2))+
  facet_grid(~address_type)


# Analysis 2.9: attended_nursery
ggplot(student %>% group_by(address_type,attended_nursery) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=attended_nursery, y=perc,fill=as.factor(attended_nursery))) +
  geom_bar(stat="identity",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1.2))+
  facet_grid(~address_type)





# Question 3: does uni_name affect distinction or failure rates among students
# Analysis 3-1:  Finding the relationship between overall_grade and uni_name
ggplot(student, aes(y=overall_grade, x=uni_name)) + 
  geom_boxplot(aes(fill=uni_name),color="purple",show.legend=TRUE)


# Analysis 3-2: Comparing the observation number of overall_grade and uni_name
count_uni_name = c()
count_uni_name[1] <- (count(student,uni_name=="GP"))[2,2]
count_uni_name[2] <- (count(student,uni_name=="MS"))[2,2]
labels_count_uni_name = c(paste("GP -",count_uni_name[1]),paste("MS -",count_uni_name[2]))
pie(count_uni_name,main="uni_name",labels=labels_count_uni_name,col=c("red","blue"))


# Analysis 3-3:  Plot grades of GP uni_name
filter(student,uni_name=="GP" & grade_type=='D' ) %>%
  count()%>%
  message(" - Distinction Gabriel Pereira Students Count")
filter(student,uni_name=="GP" & grade_type=='F') %>%
  count()%>%
  message(" - Failure Gabriel Pereira Students Count")

ggplot(filter(student,uni_name=="GP") %>%
         group_by(grade_type) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x = "", y = perc,fill=grade_type)) +
  geom_bar(stat="identity",color="black")+
  geom_label(aes(label = perc),position = position_stack(0.5))+
  coord_polar(theta = "y")


# Analysis 3-4:  Plot grades of MS uni_name
filter(student,uni_name=="MS" & grade_type=='D' ) %>%
  count()%>%
  message(" - Distinction Mousinho da Silveira Students Count")
filter(student,uni_name=="MS" & grade_type=='F') %>%
  count()%>%
  message(" - Failure Mousinho da Silveira Students Count")

ggplot(filter(student,uni_name=="MS") %>%
         group_by(grade_type) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x = "", y = perc,fill=grade_type)) +
  geom_bar(stat="identity",color="black")+
  geom_label(aes(label = perc),position = position_stack(0.5))+
  coord_polar(theta = "y")





# Question 4:	What factors in Mousinho da Silveira uni_name postitvely affect grades
# Analysis 4-1: Relationship between address_type and uni_name
ggplot(student %>% group_by(uni_name,address_type) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=address_type, y=perc,fill=as.factor(address_type))) +
  geom_bar(stat="identity",color="purple",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1.2))+
  facet_grid(~uni_name)


# Analysis 4-2: Relationship between class_failures and uni_name 
student %>%  
  group_by(uni_name)%>%  
  summarise(mean_class_failures = mean(class_failures))

ggplot(student %>% group_by(uni_name,class_failures) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=class_failures, y=perc,fill=as.factor(class_failures))) +
  geom_bar(stat="identity",color="purple",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1.2))+
  facet_grid(~uni_name)


# Analysis 4-3: Relationship between enrollment_reason and uni_name 
ggplot(student %>% group_by(uni_name,enrollment_reason) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=enrollment_reason, y=perc)) +
  geom_line(aes(group=uni_name,color=uni_name))+
  geom_label(aes(label = perc))
  

# Analysis 4-4: Relationship between extra_activities and uni_name
ggplot(student %>% group_by(uni_name,extra_activities) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=extra_activities, y=perc,fill=as.factor(extra_activities))) +
  geom_bar(stat="identity",color="purple",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1.2))+
  facet_grid(~uni_name)


# Analysis 4-5: Relationship between extra_classes and uni_name 
ggplot(student %>% group_by(uni_name,extra_classes) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=extra_classes, y=perc,fill=as.factor(extra_classes))) +
  geom_bar(stat="identity",color="purple",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1.2))+
  facet_grid(~uni_name)


# Analysis 4-6: Relationship between higher_edu_interest and uni_name 
ggplot(student %>% group_by(uni_name,higher_edu_interest) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=higher_edu_interest, y=perc,fill=as.factor(higher_edu_interest))) +
  geom_bar(stat="identity",color="purple",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1.2))+
  facet_grid(~uni_name)


# Analysis 4-7: Relationship between uni_support and uni_name 
ggplot(student %>% group_by(uni_name,uni_support) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=uni_support, y=perc,fill=as.factor(uni_support))) +
  geom_bar(stat="identity",color="purple",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1.2))+
  facet_grid(~uni_name)


# Analysis 4-8: Relationship between uni_absences and uni_name 
ggplot(student %>% group_by(uni_name,uni_absences) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=uni_absences, y=perc)) +
  geom_point(aes(group=uni_name,color=uni_name))+
  stat_smooth(color = "green")+
  facet_grid(~uni_name)





# Question 5: Does parent_living_status affect distinction or failure rates among students 
# Analysis 5-1: Finding the relationship between overall_grade and parent_living_status
ggplot(student, aes(y=overall_grade, x=parent_living_status)) + 
  geom_boxplot(aes(fill=parent_living_status),color="purple",show.legend=TRUE)


# Analysis 5.2: observation number comparison of overall_grade and uni_name
count_parent_status = c()
count_parent_status[1] <- (count(student,parent_living_status=="T"))[2,2]
count_parent_status[2] <- (count(student,parent_living_status=="A"))[2,2]
labels_count_parent_status = c(paste("T -",count_parent_status[1]),paste("A -",count_parent_status[2]))
pie3D(count_parent_status,theta=1.3,main="uni_name",labels=labels_count_parent_status)


# Analysis 5-3: Plot grades of T parent_living_status
filter(student,parent_living_status=="T" & grade_type=='D' ) %>%
  count()%>%
  message(" - Distinction Count of Students with parents together")
filter(student,parent_living_status=="T" & grade_type=='F') %>%
  count()%>%
  message(" - Failure Count of Students with parents together")

ggplot(filter(student,parent_living_status=="T") %>%
         group_by(grade_type) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x = "", y = perc,fill=grade_type)) +
  geom_bar(stat="identity",color="black")+
  geom_label(aes(label = perc),position = position_stack(0.5))+
  coord_polar(theta = "y")


# Analysis 5-4: Plot grades of A parent_living_status
filter(student,parent_living_status=="A" & grade_type=='D' ) %>%
  count()%>%
  message(" - Distinction Count of Students with parents apart")
filter(student,parent_living_status=="A" & grade_type=='F') %>%
  count()%>%
  message(" - Failure Count of Students with parents apart")

ggplot(filter(student,parent_living_status=="A") %>%
         group_by(grade_type) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x = "", y = perc,fill=grade_type)) +
  geom_bar(stat="identity",color="black")+
  geom_label(aes(label = perc),position = position_stack(0.5))+
  coord_polar(theta = "y")





# Question 6: What factors in parents apart parent_living_status are positively affecting grades
# Analysis 6-1: Relationship between family_relationship and parent_living_ status
student %>%  
  group_by(parent_living_status)%>%  
  summarise(mean_family_relationship = mean(family_relationship))

ggplot(student %>% group_by(parent_living_status,family_relationship) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=family_relationship, y=perc,fill=as.factor(family_relationship))) +
  geom_bar(stat="identity",color="purple",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1))+
  facet_grid(~parent_living_status)


# Analysis 6-2: Relationship between family_support and parent_living_status
ggplot(student %>% group_by(parent_living_status,family_support) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=family_support, y=perc,fill=as.factor(family_support))) +
  geom_bar(stat="identity",color="purple",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1.2))+
  facet_grid(~parent_living_status)


# Analysis 6-3: Relationship between family_size and parent_living_status
ggplot(student %>% group_by(parent_living_status,family_size) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=family_size, y=perc,fill=as.factor(family_size))) +
  geom_bar(stat="identity",color="purple",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1.2))+
  facet_grid(~parent_living_status)


# Analysis 6-4: Relationship between guardian and parent_living_status
ggplot(student %>% group_by(parent_living_status,guardian) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=guardian, y=perc)) +
  geom_line(aes(group=parent_living_status,color=parent_living_status))+
  geom_label(aes(label = perc))


# Analysis 6-5: Relationship between free_time and parent_living_status
student %>%  
  group_by(parent_living_status)%>%  
  summarise(mean_free_time = mean(free_time))

ggplot(student %>% group_by(parent_living_status,free_time) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=free_time, y=perc,fill=as.factor(free_time))) +
  geom_bar(stat="identity",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1))+
  facet_grid(~parent_living_status)


# Analysis 6-6: Relationship between class_failures and parent_living_status
student %>%  
  group_by(parent_living_status)%>%  
  summarise(mean_class_failures = mean(class_failures))

ggplot(student %>% group_by(parent_living_status,class_failures) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=class_failures, y=perc,fill=as.factor(class_failures))) +
  geom_bar(stat="identity",color="purple",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1.2))+
  facet_grid(~parent_living_status)


# Analysis 6-7: Relationship between uni_absences and parent_living_status
ggplot(student %>% group_by(parent_living_status,uni_absences) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=uni_absences, y=perc)) +
  geom_point(aes(group=parent_living_status,color=parent_living_status))





# Question 7: Does gender affect distinction or failure rates among students 
# Analysis 7-1: Finding the relationship between overall_grade and gender
ggplot(student, aes(y=overall_grade, x=gender)) + 
  geom_boxplot(aes(fill=gender),color="purple",show.legend=TRUE)


# Analysis 7-2: Comparing the observation number of overall_grade and gender
count_gender = c()
count_gender[1] <- (count(student,gender=="M"))[2,2]
count_gender[2] <- (count(student,gender=="F"))[2,2]
labels_count_gender = c(paste("M -",count_gender[1]),paste("F -",count_gender[2]))
dotchart(count_gender,main="gender",labels=labels_count_gender,col=c("red","blue"))


# Analysis 7-3: Plot grades of M gender
filter(student,gender=="M" & grade_type=='D' ) %>%
  count()%>%
  message(" - Distinction Count of Male Students")
filter(student,gender=="M" & grade_type=='F') %>%
  count()%>%
  message(" - Failure Count of Male Students")

ggplot(filter(student,gender=="M") %>%
         group_by(grade_type) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x = "", y = perc,fill=grade_type)) +
  geom_bar(stat="identity",color="black")+
  geom_label(aes(label = perc),position = position_stack(0.5))+
  coord_polar(theta = "y")


# Analysis 7.4: plot overall_grade for F gender
filter(student,gender=="F" & grade_type=='D' ) %>%
  count()%>%
  message(" - Distinction Count of Female Students")
filter(student,gender=="F" & grade_type=='F') %>%
  count()%>%
  message(" - Failure Count of Female Students")

ggplot(filter(student,gender=="F") %>%
         group_by(grade_type) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x = "", y = perc,fill=grade_type)) +
  geom_bar(stat="identity",color="black")+
  geom_label(aes(label = perc),position = position_stack(0.5))+
  coord_polar(theta = "y")





# Question 8: What factors in male gender are positively affecting grades
# Analysis 8-1: Relationship between outgoing and gender
student %>%  
  group_by(gender)%>%  
  summarise(mean_outgoing = mean(outgoing))

ggplot(student %>% group_by(gender,outgoing) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=outgoing, y=perc,fill=as.factor(outgoing))) +
  geom_bar(stat="identity",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1))+
  facet_grid(~gender)


# Analysis 8-2: Relationship between romantic_relationship and gender
ggplot(student %>% group_by(gender,romantic_relationship) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=romantic_relationship, y=perc,fill=as.factor(romantic_relationship))) +
  geom_bar(stat="identity",color="purple",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1.2))+
  facet_grid(~gender)


# Analysis 8-3: Relationship between health and gender
student %>%  
  group_by(gender)%>%  
  summarise(mean_health = mean(health))

ggplot(student %>% group_by(gender,health) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=health, y=perc,fill=as.factor(health))) +
  geom_bar(stat="identity",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1.2))+
  facet_grid(~gender)


# Analysis 8-4: Relationship between alcohol_consumption and gender
ggplot(student %>% group_by(gender,alcohol_consumption) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=alcohol_consumption, y=perc)) +
  geom_line(aes(group=gender,color=gender))+
  geom_label(aes(label = perc))


# Analysis 8-5: Relationship between enrollment_reason and gender
ggplot(student %>% group_by(gender,enrollment_reason) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=enrollment_reason, y=perc)) +
  geom_line(aes(group=gender,color=gender))+
  geom_label(aes(label = perc),position = position_jitter(width=0.06))






# Question 9: Does parent's background affect student grades
# Analysis 9-1: Relationship between grade_type and mother_edu
ggplot(student %>% group_by(mother_edu,grade_type) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=grade_type, y=perc,fill=as.factor(grade_type))) +
  geom_bar(stat="identity",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1))+
  facet_grid(~mother_edu)+
  labs(x= 'mother_edu')


# Analysis 9-2: Relationship between grade_type and father_edu
ggplot(student %>% group_by(father_edu,grade_type) %>% 
         summarise (n = n()) %>% 
         mutate(perc = round(n / sum(n),3) * 100), 
       aes(x=grade_type, y=perc,fill=as.factor(grade_type))) +
  geom_bar(stat="identity",linetype=5) + 
  geom_label(aes(label = perc),position = position_stack(1))+
  facet_grid(~father_edu)+
  labs(x= 'father_edu')


# Analysis 9-3: Relationship between grade_type and mother_job 
ggplot(student, aes(y=overall_grade, x=mother_job)) + 
  geom_boxplot(aes(fill=mother_job),color="purple",show.legend=TRUE)


# Analysis 9-4: Relationship between grade_type and father_job
ggplot(student, aes(y=overall_grade, x=father_job)) + 
  geom_boxplot(aes(fill=father_job),color="purple",show.legend=TRUE)



