
data <- read.csv("Excel for statment validity data.csv") #load data

library(ggplot2)
library(tidyverse) #load packages



#simplify data
data <- data %>% select(-consent1,-please_rate, -consent_info,
                        -X_submitted_by, -X_notes, -X_validation_status, -X_tags, -X_index, 
                        -X_submission_time, -X_status, -X_uuid ) 
data <- na.omit(data)


#create dataframe for visualizing rating choice frequencey of participants per question
data1 <- gather(data, question, score, 2:35) 

ggplot(data1, aes(score)) + geom_histogram() + facet_wrap( .~question) + labs(title = "Distribution of Statment Ratings")



dataN <- data1 %>%
  group_by(question) %>% 
  summarise(mean = mean(score), sum = sum(score)) %>% 
  mutate(type = substr(question, 1,1)) 


Freq <- as.data.frame.matrix(table(data1$question, data1$score))
Freq <- Freq %>% 
  rownames_to_column("question")
  
colnames(Freq)  = c("question", "NegTwo", "NegOne", "Zero", "One", "Two")

Freq <- Freq %>% 
  mutate(V = NegOne + NegTwo, E = One + Two)



dataN <- left_join(dataN, Freq, by = "question") 


N <- dataN %>% 
  filter(type == "N") %>% 
  arrange(Zero)

V <- dataN %>% 
  filter(type == "V")

E <- dataN %>% 
  filter(type == "E")
 

#to save some of Dataframes as PDF
install.packages("gridExtra")
library(gridExtra)    
pdf("neutral_messages.pdf")
grid.table(N)  
dev.off()


pdf("V_messages.pdf")
grid.table(V)  
dev.off()

pdf("E_messages.pdf")
grid.table(N)  
dev.off()



 


