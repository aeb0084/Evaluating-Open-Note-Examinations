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


```ruby
#Download Survey Data
survey=read.csv("StudentData2.csv")
#Change all emails to capital letters for merge
survey$Email<-toupper(survey$Email) 

#Download Grade data
grades=read.csv("Grades.csv")
#Change all emails to capital letters for merge
grades$Email<-toupper(grades$Email) 

#Merge data sets
total <- merge(survey,grades, by="Email")

#Export for additional formatting in Excel
write.csv(total, "merged.csv")


#Redo with dropped exam column

#Merge data sets
total2 <- merge(survey,grades, by="Email")

#Export for additional formatting in Excel
write.csv(total2, "merged.examdrop.csv")
```


```ruby

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

dat=read.csv("merged_formatted2.csv", check.names = F)
#Remove all scores of "0", which indicates cheating, or that they did not take the exam.
dat=subset(dat, Exam_Score != "0")

```

> Density plots less variation and overall higher grades on exam 1. Very little variation in exams 2 and 3

#Percentages of each response (cumulative)
```ruby
perc=read.csv("percentages.csv")

average=subset(perc, Exam == "Overall" & Category != "Exclude")
positions <- c("Prepared notes", "Studied less", "Understanding", "Studied same", "Less anxious", "Did not study", "External resources", "Studied more", "No notes")

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

ggsave(p1, file="responses.png", height=5, width=3, dpi = 300)

```

> Responses are ordered by frequency on the plot. Here I summarize things that were mentioned by more than 10% of students. Students were most likely to increase note preparedness, study less or the same amount, and focus more on understanding. Additionally, approximately 7% of students said it makes them less anxious.


## Plot of responses by exam faceted by Category
```ruby

#Subset data to include three individual exam scores without the overall score, and remove all excluded data points.
exams=subset(perc, Exam != "Overall" & Category != "Exclude")

#plot the frequency student responses by exam and subset by response category
ggplot(exams, aes(fill=Category, y=Response, x=Exam)) + 
  #designate plot type as barplot
    geom_bar(position="dodge", stat="identity") +
  #angle axis text labels
 theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  #set color scale
   scale_fill_viridis_d()+
  #facet plot by response category
  facet_grid(Category~.)  


```
> This is hard to visualize this way. We then decided to identify the ones that vary over time (External Resource use, anxiety, preparing of notes, studying less, and focusing on understanding). Selected categories are then plotted below. 


```ruby
#subset data to only include categories of interest
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


```ruby
dodge <- position_dodge(width = 0.6)

#design a raincloud theme for plotting performance data. 
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

#Set calculations for upper and lower boundaries
lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)

#use ddply package to calculate means, median, lower and upper boundaries for Performance of students, for each format.
sumld<- ddply(dat, ~Exam, summarise, mean = mean(Exam_Score), median = median(Exam_Score), lower = lb(Exam_Score), upper = ub(Exam_Score))


#Plot performance by format. Raincloud plots were designed to show a point distribution of data with an overlaying boxplot alongside a density plot distribution.

dat$Exam <- factor(dat$Exam,levels = c("Exam 3", "Exam 2", "Exam 1"))


p4=ggplot(data = dat, aes(y = Exam_Score, x = Exam, fill = Exam)) +
geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, width=.4) +
geom_point(aes(y = Exam_Score, color = Exam), position = position_jitter(width = .15), size = 1, alpha = 0.6) +
geom_boxplot(width = .15, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
expand_limits(x = 2) +
guides(fill = FALSE) +
guides(color = FALSE) +
  scale_fill_manual(values = c("gray81", "gray61", "grey45")) +
  scale_color_manual(values = c("gray81", "gray61", "grey45")) +
  theme(strip.text.x = element_text(size = 16, face = "bold"))+
raincloud_theme  +
  coord_flip() +
  ylab("Performance") +
  xlab("Exam Number")

#save plot as png file for publication quality image
ggsave(p4, file="performance.png", width=5, height=3, dpi=600)

p4


#test effect of performance across exam numbers, including student email as a random identifier to account for repeated sampling
lm.perf=lme(Exam_Score~Exam, na.action=na.omit, random = ~1|Email, data=dat)
summary(lm.perf)
anova(lm.perf)

#use emmeans package to get pairwise comparisons between exam number
emmeans(lm.perf, list(pairwise ~ Exam), adjust = "tukey")

```


#Performance removing students scores of "0"

```ruby

#Remove all students who scored a "zero" on the exam, as this is not an indicator of performance, but rather a student was examined in any capacity. 
exam.nozero=subset(dat, Exam_Score != "0")

#use ddply package to calculate means, median, lower and upper boundaries for Performance of students, for each format.
sumld<- ddply(exam.nozero, ~Exam, summarise, mean = mean(Exam_Score), median = median(Exam_Score), lower = lb(Exam_Score), upper = ub(Exam_Score))


#Plot performance by format. Raincloud plots were designed to show a point distribution of data with an overlaying boxplot alongside a density plot distribution.

p5=ggplot(data = exam.nozero, aes(y = Exam_Score, x = Exam, fill = Exam)) +
geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, width=.4) +
geom_point(aes(y = Exam_Score, color = Exam), position = position_jitter(width = .15), size = 1, alpha = 0.6) +
geom_boxplot(width = .15, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
expand_limits(x = 2) +
guides(fill = FALSE) +
guides(color = FALSE) +
  scale_fill_manual(values = c("springgreen4", "springgreen3", "springgreen")) +
  scale_color_manual(values = c("springgreen4", "springgreen3", "springgreen")) +
  theme(strip.text.x = element_text(size = 16, face = "bold"))+
raincloud_theme  +
  ylab("Performance") +
  xlab("Exam Number")

p5

#test effect of performance across exam numbers, including student email as a random identifier to account for repeated sampling
lm.perf=lme(Exam_Score~Exam, na.action=na.omit, random = ~1|Email, data=exam.nozero)
summary(lm.perf)
anova(lm.perf)

#use emmeans package to get pairwise comparisons between exam number
emmeans(lm.perf, list(pairwise ~ Exam), adjust = "tukey")


```

#Produce plot of likert scale questions related to student perception
```{r}
#subset all likert scale questions for analysis
lik=dat[c(2,17:21)]
#gather likert scale items in a string
lik_long <- gather(lik, question, response, ST1Q01:ST1Q05, factor_key=TRUE)
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


names(items) <- c(
			ST1Q01="Remove.",
			ST1Q02="Since I had the option to use notes on this exam, my score....",
			ST1Q03="Since I had the option to use notes on this exam, my anxiety...",
			ST1Q04="Since I was allowed to take this exam using my notes, I think the amount I studied....",
			ST1Q05="Since students in our class were allowed to take this test using our notes, I think the amount of time other students studied...")

pl1 <- likert(items)
xtable(pl1)
plot(pl1)


P4=  ggplot(lik_long, aes(x=question, color=as.factor(response), group=as.factor(response), fill=as.factor(response))) +
  geom_histogram(position="fill", stat="count", width=0.6)+
  theme(legend.position="top")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme_bw()+
  scale_fill_brewer(palette="Purples")+
    scale_color_brewer(palette="Purples")+
  coord_flip()

facet_grid(rows = vars(question), scales="free") 

P4

#print plot as publication quality image at 300dpi 
ggsave(P4, file="add_quest.png", height=8, width=5, dpi = 300)

```


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

> For a dichotomous categorical variable and a continuous variable you can calculate a Pearson correlation if the categorical variable has a 0/1-coding for the categories. This correlation is then also known as a point-biserial correlation coefficient.

> Both Preparing Notes and Focusing on understanding were positively correlated with Final Exam scores, Final Grades, and Individual Exam scores. Which indicates that these two actions positively impact retention and performance, even on open note exams.


```ruby

ggplot(dat, aes(x=as.factor(Prepared_Notes), y=Exam_Score, colour=as.factor(Prepared_Notes))) +
  geom_line() + geom_jitter(width=0.1) 
```

```ruby

#install.packages("devtools")
#devtools::install_github("jaredhuling/jcolors")
library(jcolors)

dat$Prepared_Notes=as.factor(dat$Prepared_Notes)
dat$Understanding=as.factor(dat$Understanding)
dat$External_resources=as.factor(dat$External_resources)
dat$Less_anxious=as.factor(dat$Less_anxious)
dat$Studied_same=as.factor(dat$Studied_same)
dat$Stuided_less=as.factor(dat$Stuided_less)



#Plot performance by format. Raincloud plots were designed to show a point distribution of data with an overlaying boxplot alongside a density plot distribution.

#Teal 
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

```ruby
#Yellow
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

```ruby
#Purple
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

#multiple choice responses
```ruby
unique(dat$Anxiety.SS)
dat$Anxiety.SS <- factor(dat$Anxiety.SS,levels = c("Greatly Reduced", "Slightly Reduced", "No Effect", "Slightly Raised", "Greatly Raised"))


model.anx=(lme(Exam_Score~Anxiety.SS, random=~1|Email, data=dat, na.action = na.omit))
anova(model.anx)
emmeans(model.anx, list(pairwise ~ Anxiety.SS), adjust = "tukey")

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

ggsave(p9, file="anxiety.png", height=5, width=8, dpi = 300)
```


```ruby

unique(dat$Study.SS)
dat$Study.SS <- factor(dat$Study.SS,levels = c("Much Less", "Slightly Less", "Same Amount", "Slightly More", "Much More"))

model.study=(lme(Exam_Score~Study.SS, random=~1|Email, data=dat, na.action = na.omit))
anova(model.study)
emmeans(model.study, list(pairwise ~ Study.SS), adjust = "tukey")

p10=ggplot(data = subset(dat, Study.SS != "NA" & Study.SS != ""), aes(y = Exam_Score, x = Study.SS, fill = Study.SS)) +
geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, width=.4) +
geom_point(aes(y = Exam_Score, color = Study.SS), position = position_jitter(width = .15), size = 1, alpha = 0.6) +
geom_boxplot(width = .15, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
expand_limits(x = 2) +
guides(fill = FALSE) +
guides(color = FALSE) +
  theme(strip.text.x = element_text(size = 16, face = "bold"))+
raincloud_theme  +
  scale_x_discrete(labels=c("0"="No", "1"="Yes")) +
   scale_fill_manual(values=c("#1BA78D", "#4FBC7B", "#81C77F", "#A6D6B0", "#C2E0D6" )) +
   scale_color_manual(values=c("#1BA78D", "#4FBC7B", "#81C77F", "#A6D6B0", "#C2E0D6"  )) +
  ylab("Performance") +
  coord_flip() +
  xlab("Studying Investment")

ggsave(p10, file="study.png", height=5, width=8, dpi = 300)
```


#Stats to reaffirm that the likert scale items did not differ over time
```ruby

anova(lme(Exam_num~Study.SS, random=~1|Email, data=dat, na.action = na.omit))
anova(lme(Exam_num~Anxiety.SS, random=~1|Email, data=dat, na.action = na.omit))
anova(lme(Exam_num~Score.SS, random=~1|Email, data=dat, na.action = na.omit))
anova(lme(Exam_num~ClassStudy.SS, random=~1|Email, data=dat, na.action = na.omit))
anova(lme(Exam_num~Proctoring.SS, random=~1|Email, data=dat, na.action = na.omit))

```

