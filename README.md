# Evaluating open-note exams: student perceptions and preparation methods in an undergraduate biology class
 
Emily P. Driessen, Abby E. Beatty, & Cissy J. Ballen  
*Corresponding author: epd0016@auburn.edu

This repository holds all supplemental files for "Evaluating open-note exams: student perceptions and preparation methods in an undergraduate biology class"

## Abstract: 
> **Background** 

> Although closed-note  exams  have  traditionally been used to evaluate  students  in undergraduate biology classes, open-note  exams  are  becoming increasingly common, though little  is  known about  how  students  prepare  for these  types  of exams. We  investigated student  perceptions  of and their preparation habits  for open-note  exams  in an undergraduate  biology class, as  compared to their previous  experiences  with closed-note  exams  in other classes. Specifically, we  explored the following two research questions:  (1a) How  do students  perceive  open-note  exams  impact  their exam  scores, their anxiety, the  amount  they studied, and the  amount  their peers  studied?  (1b) How do these  perceptions  relate  to performance  outcomes?    (2a) How  do students  prepare  for open-note  exams?  (2b) How  do these  preparation methods  relate  to performance  outcomes? 

> **Results** 

> Results  demonstrate  students  perceived increased exam  scores, decreased exam-anxiety, decreased  study time  spent  personally and decreased study time  spent  by their peers  for opennote  exams  as  compared to past  experiences  with closed-note  exams. Open-ended survey responses  analyzed through first-  and second-cycle  analyses  showed many students  adapted their study habits  by focusing on note  preparation and broad conceptual  understanding rather than rote memorization. Using linear mixed effects  models  to assess  student  performance, we  found students  who focused on note  preparation and utilized outside  resources  outperformed students who did not  report  those  study habits. 

> **Conclusions** 

> As  institutions  shift  towards  flexible  and scalable  assessments  that  can be  used in face-to-face  or online  environments, open-note  exams  are  becoming  increasingly common. While  previous research has  investigated whether open-note  exams  are  comparable  to closed-note  exams  in terms  of student  performance, none  have  focused on the  differences  in how  students  prepare  for open-note  exams. We  conclude  that, with intentional  guidance  from  the  instructor, open-note exams  have  the  potential  to promote  effective  study habits  and reward higher-order thinking. Our results  highlight  how  studies  into student  preparation for exams  are  an important  part  of understanding  student  performance. 


### Quick Key to File Directory: Detailed Descriptions of file use can be found below.

Note: The final data set used in analysis is available for public use. Additionally, deidentified raw survey data is available here. Due to IRB Restrictions all data files used in analysis that contain institutional information (Grades, GPA, etc.) prior to the final merged and deidientified data are available upon direct request. Following approval, all deidentified data including institutional info will be shared directly.


Analysis and File Names| Brief Description | Link to File
-------------------------------------|------------------------------------ | -----------------------------------------------------
Final Data File                | data file containing merged quant and qual data |   [Combined Data File](merged_formatted2.csv)
Qualitative Thematic Breakdown | data file containing the frequency of qualitatively coded responses |  [Qualitative Frequencies](percentages.csv)
Data Analysis File             | R Markdown file containing all analysis and data visualization code  | [Code File](OpenNoteCode_Github_currated.Rmd)

# Statistical and Data Visualization Code

```ruby
#clear environment
rm(list=ls())

#Import Survey Data and set working directory
dat <- read.csv("~/Box/Research Projects/Open-Note/Open Note Data.csv", header=TRUE)
setwd("~/Box/Research Projects/Open-Note")
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
library(lme4)

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
#Remove all scores of "0", which indicates cheating, or that they did not take the exam.
dat=subset(dat, Exam_Score != "0")
```

Student Study Habits - Descriptive

```ruby
#Percentages of each response (cumulative)
#Read in data sheet containing the frequency of qualitatively coded responses
perc=read.csv("~/Box/Research Projects/Open-Note/Percent.csv")
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
changes=subset(perc, Category == "External resources" | Category == "Less anxious" | Category == "Prepared notes" | Category == "Studied less" | Category == "Understanding")
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

#Remove all scores of "0", which indicates cheating, or that they did not take the exam.
dat=subset(dat, Exam_Score != "0")
#remove any NAs from Exam number
 dat<-dat%>%
   filter(!is.na(Exam_num))
 
  dat<-dat%>%
   select(Email, Exam, Exam_Score, Score.SS, Anxiety.SS:External_resources) #remove unnecessary columns
```

#Plot performance by format (significant relationships only). Raincloud plots were designed to show a point distribution of data with an overlaying boxplot alongside a density plot distribution.

##Prepared Notes
```ruby
dat<-dat%>%
      filter(!is.na(Prepared_Notes))

#Prepared Notes 
notes=ggplot(data = dat, aes(y = Exam_Score, x = Prepared_Notes, fill = Prepared_Notes)) +
geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, width=.4) +
geom_point(aes(y = Exam_Score, color = Prepared_Notes), position = position_jitter(width = .15), size = 1, alpha = 0.6) +
geom_boxplot(width = .15, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
expand_limits(x = 2) +
guides(fill = "none") +
guides(color = "none") +
  scale_fill_manual(values = c("#277f8e", "#277f8e")) +
  scale_color_manual(values = c("#277f8e", "#277f8e")) +
  theme(strip.text.x = element_text(size = 16, face = "bold"))+
raincloud_theme  +
  scale_x_discrete(labels=c("0"="No", "1"="Yes")) +
  ylab("Performance") +
  xlab("Focused on Note Preparation") +
  coord_flip()
notes
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
under
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
guides(fill = "none") +
guides(color = "none") +
  scale_fill_manual(values = c("#46327e", "#46327e")) +
  scale_color_manual(values = c("#46327e", "#46327e")) +
  theme(strip.text.x = element_text(size = 16, face = "bold"))+
raincloud_theme  +
  scale_x_discrete(labels=c("0"="No", "1"="Yes")) +
  ylab("Performance") +
  xlab("Utilization of External Resources")+
  coord_flip()
res
ggsave(res, file="res.png", height=2, width=4, dpi = 300)
```


#Perform linear model tests examining the impact of each categorical response on performance. Note, due to other analysis choices and secondary factors related to how final grades are calculated, we used exam_score as the sole reporting measure. As the coded responses were either mentioned (1) or not mentioned (0) by a student, these were binary outcomes. 
```ruby
#First, we can examine all of the study habits together. We have good justification for putting all of these study habits in the model since how students prepare for exams impacts their student performance, according to previous literature. We use ANOVA to generate one p-value because running multiple t-tests increases your risk of committing type 1 error – rejecting the null when it is actually true. If the anova p-value is significant, then we can run a post hoc test to look for differences among the groups. We usually use Tukey’s, but there are many because they all balance error.
model1=lme(Exam_Score ~ Did_not_study + Studied_same + Less_anxious + No_Notes + Prepared_Notes + Understanding + External_resources + Studied_more, random=list(~1|Email), data=dat, na.action=na.omit)
anova(model1) #This shows prepared_notes, understanding, and external_resources have a significant effect on student exam performance. 
AIC(model1) #3344


#If we want to do model selection... I'll remove the least significant fixed effect from model 20 which is studied_more
model2= lme(Exam_Score ~ Did_not_study + Studied_same + Less_anxious + No_Notes + Prepared_Notes + Understanding + External_resources, random=list(~1|Email), data=dat, na.action=na.omit)
anova(model2) #This shows prepared_notes, understanding, and external_resources have a significant effect on student exam performance. 
AIC(model2) #3349 This is a higher AIC score, but not by much, so I'll continue on removing insignifican variables...

#remove no_notes
model3= lme(Exam_Score ~ Did_not_study + Studied_same + Less_anxious + Prepared_Notes + Understanding + External_resources, random=list(~1|Email), data=dat, na.action=na.omit)
anova(model3) #This shows prepared_notes, understanding, and external_resources have a significant effect on student exam performance. 
AIC(model3) #3354 This is higher again, and I believe out of the range where it is negligible, so we will use the model with all of the study habits included in it. Model 1 is the winner. 

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
notes
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
under
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
res
ggsave(res, file="res.png", height=2, width=4, dpi = 300)
```

Student Perceptions

```ruby
#Produce plot of likert scale questions related to student perception
##I think the anxiety and score labels are switched around, but I'm not sure how to fix this. Ask Abby
dat <- read.csv("~/Box/Research Projects/Open-Note/Open Note Data.csv", header=TRUE)
dat=subset(dat, Exam_Score != "0")%>%
   filter(!is.na(Exam_num))%>%
   select(Email, Exam, Exam_Score, Score.SS, Anxiety.SS:External_resources) #remove unnecessary columns
#subset all likert scale questions for analysis
lik=dat%>%
select(Email, ST1Q02:ST1Q05) #remove unnecessary columns
  
  ############
#gather likert scale items in a string
lik_long <- gather(lik, question, response, ST1Q02:ST1Q05, factor_key=TRUE)
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
#names(items) <- c(
			#ST1Q02="Since I had the option to use notes on this exam, my score....",
			#ST1Q03="Since I had the option to use notes on this exam, my anxiety...",
			#ST1Q04="Since I was allowed to take this exam using my notes, I think the amount I studied....",
      #ST1Q05= "Since students in our class were allowed to take this test using our notes, I think the amount of time other students studied...")
pl1 <- likert(items)
xtable(pl1)
#produce plot
plot1<-plot(pl1)
plot1
ggsave(plot1, file=" plot1.png",
       height = 4.5,
       width = 6,
       units = "in",
       dpi = 300)

##Below are checks of the variables#

dat %>% 
  group_by(Anxiety.SS) %>% #Anxiety is ST1Q03
  summarise(n=n()) #-2 = 145, -1 = 158, 0 = 57, 1=28, 2=18, NA =15

dat %>% #Score is ST1Q02
  group_by(Score.SS) %>% 
  summarise(n=n()) #-2 = 4, -1 = 11, 0 = 51, 1=154, 2=187, NA =15

dat %>% 
  group_by(Study.SS) %>% #Personal study is ST1Q04
  summarise(n=n()) #-2 = 59, -1 = 144, 0 = 169, 1=25, 2=9, NA =16

dat %>% 
  group_by(ClassStudy.SS) %>% #Class study is ST1Q05
  summarise(n=n()) #-2 = 98, -1 = 181, 0 = 107, 1=15, 2=5, NA =17

dat %>% 
  group_by(ST1Q01) %>% 
  summarise(n=n()) #-2 = 97, -1 = 94, 0 = 129, 1=74, 2=14, NA =17

dat %>% 
  group_by(ST1Q02) %>% 
  summarise(n=n()) #-2 = 4, -1 =11, 0 = 51, 1=154, 2=187, NA =18

dat %>% 
  group_by(ST1Q03) %>% 
  summarise(n=n())#-2 = 145, -1 =158, 0 = 57, 1=28, 2=18, NA =19

dat %>% 
  group_by(ST1Q04) %>% 
  summarise(n=n())#-2 = 59, -1 =144, 0 = 169, 1=25, 2=9, NA =19

dat %>% 
  group_by(ST1Q05) %>% 
  summarise(n=n()) #-2 = 98, -1 =181, 0 = 107, 1=15, 2=5, NA =19 # what is this fifth likert scale variable? The numbers dont' match any of the other ones, but we only measured four perceptions...
```

Student perceptions as related to exam performance
```ruby
#All perceptions in one model
model.perceptions1=(lme(Exam_Score~Study.SS + Anxiety.SS + Score.SS + ClassStudy.SS, random=~1|Email, data=dat, na.action = na.omit))
anova(model.perceptions1)
model.perceptions1
AIC(model.perceptions1) #3120.585

#Now remove least significant fixed effect to check for model selection. Remove ClassStudy.SS
model.perceptions2=(lme(Exam_Score~Study.SS + Anxiety.SS + Score.SS, random=~1|Email, data=dat, na.action = na.omit))
anova(model.perceptions2)
AIC(model.perceptions2) #3138. This is a higher AIC score, but we want the lowest AIC score, so we select model.perceptions1 model, which contains all of the student perceptions in the model. Now, how do we check for differences between the categories with emmeans when all of these are in the model together?
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


```ruby
unique(dat$Study.SS)
dat$Study.SS <- factor(dat$Study.SS,levels = c("Much Less", "Slightly Less", "Same Amount", "Slightly More", "Much More"))
#Run linear model evaluating the relationship between exam performance and student reported studying
model.study=(lme(Exam_Score~Study.SS, random=~1|Email, data=dat, na.action = na.omit))
anova(model.study)
model.study
emmeans(model.study, list(pairwise ~ Study.SS), adjust = "tukey")
p10=ggplot(data = subset(dat, Study.SS != "NA" & Study.SS != ""), aes(y = Exam_Score, x = Study.SS, fill = Study.SS)) +
geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, width=.4) +
geom_point(aes(y = Exam_Score, color = Study.SS), position = position_jitter(width = .15), size = 1, alpha = 0.6) +
geom_boxplot(width = .15, guides = "none", outlier.shape = NA, alpha = 0.5) +
expand_limits(x = 2) +
guides(fill = "none") +
guides(color = "none") +
  theme(strip.text.x = element_text(size = 16, face = "bold"))+
raincloud_theme  +
  scale_x_discrete(labels=c("0"="No", "1"="Yes")) +
   scale_fill_manual(values=c("#1BA78D", "#4FBC7B", "#81C77F", "#A6D6B0", "#C2E0D6" )) +
   scale_color_manual(values=c("#1BA78D", "#4FBC7B", "#81C77F", "#A6D6B0", "#C2E0D6"  )) +
  ylab("Performance") +
  coord_flip() +
  xlab("Studying Investment")
p10
ggsave(p10, file="study.png", height=5, width=8, dpi = 300)
```

```ruby
dat <- read.csv("~/Box/Research Projects/Open-Note/Open Note Data.csv", header=TRUE)
dat=subset(dat, Exam_Score != "0")
#Remove all scores of "0", which indicates cheating, or that they did not take the exam.
dat=subset(dat, Exam_Score != "0")%>%
   filter(!is.na(Exam_num))%>%
   select(Email, Exam, Exam_Score, Score.SS, Anxiety.SS:External_resources) #remove unnecessary columns

#unique(dat$Score.SS)
#dat$Score.SS <- factor(dat$Score.SS, levels = c("Much Less", "Slightly Less", "Same Amount", "Slightly More", "Much More"))
#Run linear model evaluating the relationship between exam performance and student reported studying
dat<-dat%>%
mutate(Score.SS=fct_relevel(Score.SS, "Greatly decreased my exam score", "Slightly decreased my exam score", "Had no effect on my exam score", "Slightly Improved my exam score", "Greatly improved my exam score")) 
model.score=(lme(Exam_Score~Score.SS, random=~1|Email, data=dat, na.action = na.omit))
anova(model.score)
emmeans(model.score, list(pairwise ~ Score.SS), adjust = "tukey")
model.score=ggplot(data = subset(dat, Score.SS != "NA" & Score.SS != ""), aes(y = Exam_Score, x = Score.SS, fill = Score.SS)) +
geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8, width=.4) +
geom_point(aes(y = Exam_Score, color = Score.SS), position = position_jitter(width = .15), size = 1, alpha = 0.6) +
geom_boxplot(width = .15, guides = "none", outlier.shape = NA, alpha = 0.5) +
expand_limits(x = 2) +
guides(fill = "none") +
guides(color = "none") +
  theme(strip.text.x = element_text(size = 16, face = "bold"))+
raincloud_theme  +
  scale_x_discrete(labels=c("0"="No", "1"="Yes")) +
   scale_fill_manual(values=c("orange4", "orange3", "orange2", "orange1", "orange")) +
   scale_color_manual(values=c("orange4", "orange3", "orange2", "orange1", "orange")) +
  ylab("Performance") +
  coord_flip() +
  xlab("Effect on Exam Score ")
model.score
ggsave(model.score, file="model.score.png", height=5, width=8, dpi = 300)

```

#Stats to reaffirm that the likert scale items did not differ over time. Can switch the orders. 
```ruby

dat <- read.csv("~/Box/Research Projects/Open-Note/Open Note Data.csv", header=TRUE)
dat=subset(dat, Exam_Score != "0")%>%
   filter(!is.na(Exam_num))%>%
   select(Email, Exam, Exam_num, Exam_Score, Score.SS, Anxiety.SS:External_resources) 
  
#Check on study time over time
study1<-glmer(Exam_num~Study.SS + (1|Email), data=dat, family=poisson, na.action = na.omit)
summary(study1)
anova(study1)

#check on anxiety over time 
anxiety1<-glmer(Exam_num~Anxiety.SS + (1|Email), data=dat, family=poisson, na.action = na.omit)
summary(anxiety1)
anova(anxiety1)

score1<-glmer(Exam_num~Score.SS + (1|Email), data=dat, family=poisson, na.action = na.omit)
summary(score1)
anova(score1)

class1<-glmer(Exam_num~ClassStudy.SS + (1|Email), data=dat, family=poisson, na.action = na.omit)
summary(class1)
anova(class1)
```

