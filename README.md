# Evaluating open-note exams: student perceptions and preparation methods in an undergraduate biology class
 
Emily P. Driessen, Abby E. Beatty, & Cissy J. Ballen  
*Corresponding author: epd0016@auburn.edu

This repository holds all supplemental files for "Evaluating open-note exams: student perceptions and preparation methods in an undergraduate biology class"

## Abstract: 
> **Background** 

> Although closed-note exams have traditionally been used to evaluate students in undergraduate biology classes, open-note exams are becoming increasingly common, though little is known about how students prepare for these types of exams. We investigated student preparation habits for open-note versus closed-note exams. Specifically, we investigated the following two research questions: (1a) How do students perceive open-note exams impact their exam scores, anxiety, the amount they studied, and the amount their peers studied? (1b) How do these perceptions relate to performance outcomes?  (2a) How do students prepare for open-note exams compared to previous experiences with closed-note exams? (2b) How do these preparation methods relate to performance outcomes? 

> **Results** 

> Results demonstrate students perceived increased exam scores, decreased exam-anxiety, decreased study time spent personally and decreased study time spent by their peers for open-note exams as compared to past experiences with closed-note exams. Open-ended survey responses analyzed through first- and second-cycle analyses showed many students adapted their study habits by focusing on note preparation and broad conceptual understanding rather than rote memorization. Using linear mixed effects models to assess student performance, we found students who focused on note preparation and utilized outside resources outperformed students who did not report those study habits. 

> **Conclusions** 

> As institutions shift towards flexible and scalable assessments that can be used in face-to-face or online environments, open-note exams are becoming increasingly common. While previous research has investigated whether open-note exams are comparable to closed-note exams in terms of student performance, none have focused on the differences in how students prepare for open-note exams. We conclude that, with intentional guidance from the instructor, open-note exams have the potential to promote effective study habits and reward higher-order thinking. Our results highlight how studies into student preparation for exams are an important part of understanding student performance. 


### Quick Key to File Directory: Detailed Descriptions of file use can be found below.

Note: The final data set used in analysis is available for public use. Additionally, deidentified raw survey data is available here. Due to IRB Restrictions all data files used in analysis that contain institutional information (Grades, GPA, etc.) prior to the final merged and deidientified data are available upon direct request. Following approval, all deidentified data including institutional info will be shared directly.


Analysis and File Names| Brief Description | Link to File
-------------------------------------|------------------------------------ | -----------------------------------------------------


# Statistical and Data Visualization Code

#Merging of quantitative and qualitative data to produce data sheet used in analysis
```ruby
#Download Survey Data
#survey=read.csv("StudentData2.csv")
#Change all emails to capital letters for merge
#survey$Email<-toupper(survey$Email) 

#Download Grade data
#grades=read.csv("Grades.csv")
#Change all emails to capital letters for merge
#grades$Email<-toupper(grades$Email) 

#Merge data sets
#total <- merge(survey,grades, by="Email")

#Export for additional formatting in Excel
#write.csv(total, "merged.csv")

#Redo with dropped exam column

#Merge data sets
#total2 <- merge(survey,grades, by="Email")

#Export for additional formatting in Excel
#write.csv(total2, "merged.examdrop.csv")
```


```ruby

#load all necessary packages

library(ggplot2)
library(reshape2)
library(nlme)
library(plyr)
library(emmeans)
library(devtools)
library(tidyverse)
library(corrplot)
library(GPArotation)
library(semPlot)
library(lavaan)
library(cowplot)
library(readr)
library(tidyr)
library(Hmisc)
library(RColorBrewer)
library(PupillometryR)
library(likert)
library(ggalt)
library(ggforce)


dodge <- position_dodge(width = 0.6)

#design a raincloud theme for plotting  data. 
raincloud_theme = theme(
text = element_text(size = 10),
axis.title.x = element_text(size = 16),
axis.title.y = element_text(size = 16),
axis.text = element_text(size = 12),
#axis.text.x = element_text(angle = 45, vjust = 0.5),
legend.title=element_text(size=16),
legend.text=element_text(size=16),
legend.position = "right",
plot.title = element_text(lineheight=.8, face="bold", size = 16),
panel.border = element_blank(),
#panel.grid.minor = element_blank(),
#panel.grid.major = element_blank(),
axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))



#read in final data file
## note that this is the file created in the first chunk of code, after merging the quanitative and qualitative data.
dat=read.csv("merged_formatted2.csv", check.names = F)
#Remove all scores of "0", which indicates cheating, or that they did not take the exam.
dat=subset(dat, Exam_Score != "0")

```

#Evaluation of Student Performance
```ruby
#Performance removing students scores of "0"

#test effect of performance across exam numbers, including student email as a random identifier to account for repeated sampling
lm.perf=lme(Exam_Score~Exam, na.action=na.omit, random = ~1|Email, data=dat)
anova(lm.perf)

#use emmeans package to get pairwise comparisons between exam number
emmeans(lm.perf, list(pairwise ~ Exam), adjust = "tukey")

```

#Percentages of each response (cumulative)
```ruby
#Read in data sheet containing the frequency of qualitatively coded responses
perc=read.csv("percentages.csv")

#Calculate the average for each qualtiative thematic category
average=subset(perc, Exam == "Overall" & Category != "Exclude")
#reorder responses to be ordered by frequency
positions <- c("Prepared notes", "Studied less", "Understanding", "Studied same", "Less anxious", "Did not study", "External resources", "Studied more", "No notes")

#plot frequency of qualtitative responses
p1=ggplot(average, aes(fill=Category, y=Response, x=Category)) + 
    geom_bar(position="dodge", stat="identity") +
scale_x_discrete(limits = positions) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    scale_fill_viridis_d() +
  theme(legend.position = "none") +
  ylab("Percentage") +
  xlab("Response") +
  coord_flip()

p1

#save file as png for publication
ggsave(p1, file="responses.png", height=5, width=3, dpi = 300)

```

> Responses are ordered by frequency on the plot. Here I summarize things that were mentioned by more than 15% of students. Students were most likely to increase note preparedness, study less or the same amount, and focus more on understanding. Additionally, approximately 11% of students said it makes them less anxious.


## Plot of responses by exam faceted by Category

```ruby

#subset data to only include categories of interest (in this case, categories where response frequency changed visually over time)
changes=subset(exams, Category == "External resources" | Category == "Less anxious" | Category == "Prepared notes" | Category == "Studied less" | Category == "Understanding")

#plot response categories of interest by exam and group by response category
p3=ggplot(changes, aes(fill=Category, y=Response, x=Exam)) + 
    geom_bar(position="dodge", stat="identity") +
 theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
scale_fill_manual(values=c( '#472D7B', '#3B528B', '#21908C', '#27AD81','#FDE725')) +
  facet_wrap(~Category, scales = "free", strip.position = "top", nrow=3)  +
    ylab("Percentage") +
  xlab("Exam")

p3

#save plot as png file for publication quality at 600dpi
ggsave(p3, file="changes.png", width=5, height=5, dpi=600)

```

#Produce plot of likert scale questions related to student perception
```ruby
#subset all likert scale questions for analysis
lik=dat[c(2,17:21)]
#gather likert scale items in a string
lik_long <- gather(lik, question, response, ST1Q01:ST1Q04, factor_key=TRUE)
#remove all "NA" responses for the data set
lik_long <- na.omit(lik_long) 
lik <- na.omit(lik) 

#produce a table containing number of observations per exam. Be sure that his does not shows "0" in any category.
table(lik$Exam)

items <- lik[,substr(names(lik), 1,4) == 'ST1Q']
head(items); ncol(items)

mylevels <- c('-2', '-1', '0', '1', '2')

sapply(items, class) 
sapply(items, function(x) { length(levels(x)) } )

for(i in seq_along(items)) {
	items[,i] <- factor(items[,i], levels=mylevels)
}

#provide survey item questions as labels
names(items) <- c(
			ST1Q01="Since I had the option to use notes on this exam, my score....",
			ST1Q02="Since I had the option to use notes on this exam, my anxiety...",
			ST1Q03="Since I was allowed to take this exam using my notes, I think the amount I studied....",
			ST1Q04="Since students in our class were allowed to take this test using our notes, I think the amount of time other students studied...")

pl1 <- likert(items)
xtable(pl1)

#produce plot
plot(pl1)

```

#Perform linear model tests examining the impact of each categorical response on performance. Note, due to other analysis choices and secondary factors related to how final grades are calculated, we used exam_score as the sole reporting measure.
```ruby

model1=lme(Stuided_less ~ Exam_Score + FinalGrade + FinalExam, random=list(~1|Email, ~1|Exam), data=dat)
anova(model1)

model2=lme(Did_not_study ~ Exam_Score + FinalGrade + FinalExam, random=list(~1|Email), data=dat)
anova(model2)

model3=lme(Studied_same ~ Exam_Score + FinalGrade + FinalExam, random=list(~1|Email, ~1|Exam), data=dat)
anova(model3)

model4=lme(Less_anxious ~ Exam_Score + FinalGrade + FinalExam, random=list(~1|Email, ~1|Exam), data=dat)
anova(model4)

model6=lme(No_Notes ~ Exam_Score + FinalGrade + FinalExam, random=list(~1|Email), data=dat)
anova(model6)

###Significant
#Final exam is cumulative- Too many notes to look through within the time period? So it worked for exam performance, but not for their final because not enough time to use even well structured notes

model7=lme(Prepared_Notes ~ Exam_Score + FinalGrade + FinalExam, random=list(~1|Email, ~1|Exam), data=dat)
anova(model7)

model8=lme(Understanding ~ Exam_Score + FinalGrade + FinalExam, random=list(~1|Email, ~1|Exam), data=dat)
anova(model8)

model5=lme(External_resources ~ Exam_Score + FinalGrade + FinalExam, random=list(~1|Email), data=dat)
anova(model5)

```


```ruby

#install.packages("devtools")
#devtools::install_github("jaredhuling/jcolors")
library(jcolors)

#Set all categorical response themes as "factored" responses for plotting
dat$Prepared_Notes=as.factor(dat$Prepared_Notes)
dat$Understanding=as.factor(dat$Understanding)
dat$External_resources=as.factor(dat$External_resources)
dat$Less_anxious=as.factor(dat$Less_anxious)
dat$Studied_same=as.factor(dat$Studied_same)
dat$Stuided_less=as.factor(dat$Stuided_less)


```

#Plot performance by format (significant relatioships only). Raincloud plots were designed to show a point distribution of data with an overlaying boxplot alongside a density plot distribution.


##Prepared Notes
```ruby

#Prepared Notes 
notes=ggplot(data = dat, aes(y = Exam_Score, x = Prepared_Notes, fill = Prepared_Notes)) +
geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, width=.4) +
geom_point(aes(y = Exam_Score, color = Prepared_Notes), position = position_jitter(width = .15), size = 1, alpha = 0.6) +
geom_boxplot(width = .15, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
expand_limits(x = 2) +
guides(fill = FALSE) +
guides(color = FALSE) +
  scale_fill_manual(values = c("#277f8e", "#277f8e")) +
  scale_color_manual(values = c("#277f8e", "#277f8e")) +
  theme(strip.text.x = element_text(size = 16, face = "bold"))+
raincloud_theme  +
  scale_x_discrete(labels=c("0"="No", "1"="Yes")) +
  ylab("Performance") +
  xlab("Focused on Note Preparation") +
  coord_flip()

ggsave(notes, file="notes.png", height=2, width=4, dpi = 300)

```
##Focus on Understanding

```ruby
#Focus on Understanding
under=ggplot(data = dat, aes(y = Exam_Score, x = Understanding, fill = Understanding)) +
geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, width=.4) +
geom_point(aes(y = Exam_Score, color = Understanding), position = position_jitter(width = .15), size = 1, alpha = 0.6) +
geom_boxplot(width = .15, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
expand_limits(x = 2) +
guides(fill = FALSE) +
guides(color = FALSE) +
  scale_fill_manual(values = c("#fde725", "#fde725")) +
  scale_color_manual(values = c("#fde725", "#fde725")) +
  theme(strip.text.x = element_text(size = 16, face = "bold"))+
raincloud_theme  +
  scale_x_discrete(labels=c("0"="No", "1"="Yes")) +
  ylab("Performance") +
  xlab("Focused on Understanding") +
  coord_flip()

ggsave(under, file="under.png", height=2, width=4, dpi = 300)

```

##Utilizing External Resources
```ruby
#External Resources
res=ggplot(data = dat, aes(y = Exam_Score, x = External_resources, fill = External_resources)) +
geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, width=.4) +
geom_point(aes(y = Exam_Score, color = External_resources), position = position_jitter(width = .15), size = 1, alpha = 0.6) +
geom_boxplot(width = .15, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
expand_limits(x = 2) +
guides(fill = FALSE) +
guides(color = FALSE) +
  scale_fill_manual(values = c("#46327e", "#46327e")) +
  scale_color_manual(values = c("#46327e", "#46327e")) +
  theme(strip.text.x = element_text(size = 16, face = "bold"))+
raincloud_theme  +
  scale_x_discrete(labels=c("0"="No", "1"="Yes")) +
  ylab("Performance") +
  xlab("Utilization of External Resources")+
  coord_flip()

ggsave(res, file="res.png", height=2, width=4, dpi = 300)

```

#Muliple Choices Responses- Effect on Performance
```ruby
unique(dat$Anxiety.SS)
#Set factor level order to go from "Greatly Reduced" to "Greatly Raised"
dat$Anxiety.SS <- factor(dat$Anxiety.SS,levels = c("Greatly Reduced", "Slightly Reduced", "No Effect", "Slightly Raised", "Greatly Raised"))

#Run linear model evaluating the relationship between exam performance and student reported anxiety.
model.anx=(lme(Exam_Score~Anxiety.SS, random=~1|Email, data=dat, na.action = na.omit))
anova(model.anx)
emmeans(model.anx, list(pairwise ~ Anxiety.SS), adjust = "tukey")

#Plot relationship
p9=ggplot(data = subset(dat, Anxiety.SS != "NA" & Anxiety.SS != ""), aes(y = Exam_Score, x = Anxiety.SS, fill = Anxiety.SS)) +
geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, width=.4) +
geom_point(aes(y = Exam_Score, color = Anxiety.SS), position = position_jitter(width = .15), size = 1, alpha = 0.6) +
geom_boxplot(width = .15, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
expand_limits(x = 2) +
guides(fill = FALSE) +
guides(color = FALSE) +
  theme(strip.text.x = element_text(size = 16, face = "bold"))+
raincloud_theme  +
  scale_x_discrete(labels=c("0"="No", "1"="Yes")) +
scale_fill_manual(values=c("#1A2E47", "#2D4E79", "#39669D", "#5687C2", "#82A7D3" )) +
   scale_color_manual(values=c("#1A2E47", "#2D4E79", "#39669D", "#5687C2", "#82A7D3" )) +
  ylab("Performance") +
  coord_flip() +
  xlab("Effect on Anxiety")

p9

ggsave(p9, file="anxiety.png", height=5, width=8, dpi = 300)
```
