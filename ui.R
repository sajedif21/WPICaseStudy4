


txt1 <- "I looked at the dataset for Autistic Spectrum Disorder (ASD) Screening Data for Children because I have always found how doctors diagnose mental disorders fascinating. The dataset is made up of twenty columns with the first 10 being based on questions asked by an app called ASDQuiz created by Fadi Fayez Thabtah. The columns are names A(Question#)_Score. For example, the column for the first question is called A1_Score."
txt1.5<- "The questions were"
txt2<- "1) “S/he often notices small sounds when others do not”"
txt3<- "2) “S/he usually concentrates more on the whole picture, rather than the small details” "
txt4 <- "3) “In social group, s/he can easily keep track of several different people’s conversations”" 
txt5<- "4) “S/he finds it easy to go back and forth between different activities”"
txt6 <- "5) “S/he doesn’t know how to keep a conversation going with his/her peers”"
txt7<- "6) “S/he is good a social chit-chat”" 
txt8<- "7) “When s/he is read a story, s/he finds it difficult to work out the character’s intentions or feelings”"
txt9<- "8) “When s/he was in preschool, s/he used to enjoy playing games involving pretending with other children”"
txt10<- "9) “S/he finds it easy to work out what someone is thinking or feeling just by looking at their face”"
txt11<- "10) “S/he finds it hard to make new friends”"
txt11.5<- "Question 10 had to be pulled out because it produced NAs in the logistic regression"
txt12<- "The options for the answers were:"
txt13<- "1) “Definitely Agree”"
txt14<- "2) “Slightly Agree”" 
txt15<- "3) “Slightly Disagree”"
txt16<- "4) “Definitely Disagree”"

txt17<- "The answers in the dataset are binary variables, where 0 means no sign of ASD and 1 means there is a sign for ASD. The answers that give 0 and 1 differ for each question. 
The answers to these questions are put through an algorithm to determine if the subject has signs of ASD or not. This information is given in the last column of the dataset and is called “Class/ASD”. The addition of all of the answers gives a column called “result”. If “result” is a 6 or below, the subject does not show signs of ASD, but if the score adds up to be larger than 6 then the subject shows signs of ASD. 
The rest of the dataset is about the subject."
txt18<- "The columns are:"
txt19 <- "1) “age” (quantitative) ranging from 4 to 11 years old since this data is just for children"
txt20 <- "2) “gender”(binary, male/female)"
txt21<- "3) “Ethnicity” (categorical) "
txt22<- "4) “jundice” (binary, yes/no) represents if the subject was born with Jaundice"
txt23<- "5) “austim” (binary, yes/no) represents if an immediate family member has autism"
txt24<- "6) “Country_of_res” (categorical) which is where the subject is living"
txt25<- "7) “used_app_before” (binary, yes/no) asks if the test has been taken before for the subject"
txt26<- "8) “relation” (categorical) represents who took the test for the patient"
txt27<- "Austim and Country_of_res were taken out of the dataset because they produced NAs when used in logistic regression."




txt28<- "Classification is used to predict an answer by giving a probability of an event occurring based on other variables. In the case of the Autistic Spectrum Disorder (ASD) Screening Data for Children, the question that is being answered is “does the subject show signs of ASD?”. Here the question has a yes and no options for answers making it a binary classification. The inputs, which are the variables determining the answer can be continuous or discrete. The output is a coefficient for each input that tells the impact of the variable done through logistic regression. The use of this is to put subjects into classes. 
If the probability comes out as greater than .5, then under the standard case, the class would be yes to the question. The threshold can depend on the type of data that one is working with, so be careful. "

txt29<- "There is also multiple classification which is used when there are multiple options to the question being asked. For example, if the question was did one pass an exam, then a more complicated answer selection than yes/no would be yes, no, did not show up, or cheated. Since this is more complicated, one should try to see if yes and no would be sufficient by grouping did not show up and cheated into the no class. "
txt30<- "The area under the curve graphs which can be seen in the “Logistic Regression Given Data & Random Data” tab show how well the inputs determine the class. The closer the number is to 1 the better the model is. The goal is to get the highest number with the variables at hand."
txt31<- "The way machine learning works with classification is by using a training, test, and unknown set. The training set is used to find the coefficients for the impact of each input. The test set is to make sure that the model fits the data to the best of its ability. There can be a back and forth set between the training and test datasets. Finally, the unknown data is where machine learning proves itself. If the model is good, then the computer can predict which class the subject belongs to. This is the power behind classification."




ui <- navbarPage("Case Study 4- Autistic Spectrum Disorder Screening", 
                 
                 tabPanel("Looking at Variables", dataTableOutput("eth"), plotOutput("ageB"),
                          plotOutput("jun"), plotOutput("asd"), plotOutput("rel"), plotOutput("asdr")), 
                 tabPanel("Info on Dataset",  p(txt1), p(txt1.5), p(txt2), p(txt3), p(txt4), p(txt5), p(txt6), p(txt7), 
                          p(txt8), p(txt9), p(txt10), p(txt11),p(txt11.5), p(txt12), p(txt13), p(txt14), p(txt15), p(txt16), p(txt17), p(txt18),
                          p(txt19), p(txt20), p(txt21), p(txt22), p(txt23), p(txt24), p(txt25), p(txt26), p(txt27)), 
                 tabPanel("Info on Classification and Machine Learning", p(txt28), p(txt29), p(txt30), p(txt31)),
                 tabPanel("General Logistic Regression",
                          sidebarLayout(sidebarPanel(
                            uiOutput("other_var_select")),
                            mainPanel(verbatimTextOutput("other_val_show")))), 
                 tabPanel("Logistic Regression Given Data & Random Data",
                          sidebarLayout(sidebarPanel(
                            uiOutput("other_var_select2")),
                            mainPanel(verbatimTextOutput("train_show"), plotOutput("g2"), plotOutput("g3"), plotOutput("g")))))
