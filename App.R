#######BRISTISH ELECTION DATA APP
#Required libraries
library(descr) #for crosstab
library(knitr)
library(sjlabelled)
library(foreign)
library(Hmisc)
library(ggplot2)
library(gridExtra)
library(grid)
library(data.table)
library(stargazer)
library(haven)
library(Hmisc)
library(ggplot2)
library(gridExtra)
library(grid)
library(data.table)
library(shiny)
library(rsconnect)
library(shinythemes)
library(RColorBrewer)
library(ggpubr)
library(png)
library(shinydashboard)
library(leaflet)
library(scales)
library(lattice)
library(plyr)
library(dplyr)
library(magrittr)
library(rgdal)
library(purrr)
library(sjmisc)
library(tables)
library(DescTools)
library(MASS)
library(gmodels)

#CSV DATA CLEANING
#the dataset is already cleaned and has the correct variables so I just need to import it.
dataBES1 <- read.csv("bes_short.csv")
dataBES1$VoteEU2 <- car::recode(dataBES1$VoteEU2, "1=2;2=3;3=1")
dataBES1$BrexVote <- car::recode(dataBES1$VoteEU, "3=2")
dataBES1$GeneralPty <- as.numeric(dataBES1$GeneralPty)
dataBES1$LeftRight <- as.numeric(dataBES1$LeftRight)
dataBES1$ImmigEcon <- as.numeric(dataBES1$ImmigEcon)
dataBES1$VoteEU <- as.numeric(dataBES1$VoteEU)
dataBES1$EUOwn <- as.numeric(dataBES1$EUOwn)
dataBES1$RefDeal <- as.numeric(dataBES1$RefDeal)
dataBES1$Income <- as.numeric(dataBES1$Income)
dataBES1$Gender <- as.numeric(dataBES1$Gender)
dataBES1$AgeBand <- as.numeric(dataBES1$AgeBand)
dataBES1$Region <- as.numeric(dataBES1$Region)
dataBES1$EdLevel <- as.numeric(dataBES1$EdLevel)

#DTA file cleaning and preparing for app work.
dataBESX <- read_dta("2017 BES Small v12.dta")
names(dataBESX)
n <- ncol(dataBESX)
labels_list <- map(1:n, function(x) attr(dataBESX[[x]], "label") )

# if a vector of character strings is preferable
labels_vector <- map_chr(1:n, function(x) attr(dataBESX[[x]], "label") )

#Reducing the number of variables in the dta file.
myvars <- names(dataBESX) %in% c(
  "a01","a02","a03","m02_1","m02_2","m02_3","m02_4","m02_5","m02_6","b01",     
  "b04","b05","b06a","b07","b09","b12_1","b12_2","b12_3","b12_4",
  "b12_5","b12_6","b13_1","b13_2","b13_3","b13_4","b13_5","b13_6","c01","c02_1",
  "c02_2","c02_3","c02_4","d02","d03","d04","f01_1","f01_2","f01_3",
  "f01_4","f01_5","f01_6","f01_7","f01_8","f01_9","f01_10","f01_11","f01_12","f2",
  "g01_1","g01_2","g01_3","g01_4","g01_5","g01_6","g01_7","g01_8","h01",
  "i01_1","i01_2","i01_3","i01_4","i01_5","i01_6","i01_7",
  "j06","j05","j08","j10","j11","j12","k01","k02","k03","l01","l02","l03",
  "l04","l09","m01","n03","p03_2","p03_3","p03_4",
  "p03_5","p03_6","p03_7",  "p03_8","p03a", "q01_1","q01_2","q01_3","q01_4",
  "q01_5","q02_1","q02_2","q03_1","q03_2","q03_3","r03","r04","r05","r05a",
  "s01_1","s01_2","s01_3","s01_4","s01_5","s01_6","s01_7","q11","q12","t01_1",
  "t01_2","t02","u01","u03","u05","v01","v04","v05","v06","w01",
  "w02","w03","w08","w09","w11","w12","w13_1","w13_2","w15_1","w15_2",
  "w15_3","r6","x01_1","x01_2","x01_3","x01_4","x01_5","x01_6","x2","x3",
  "k06","k08","y03","y04_01","y04_02","y06","y07","y08",
  "Age", "y11", "y17", "y26")


newdataT <- dataBESX[!myvars]
names(newdataT)

#Label changing, dropping unecessary labels
newdataT$b02 <- set_labels( newdataT$b02, 
                            labels = c( "No party"=0,"Labour"=1, "Conservatives"=2, "Liberal Democrats"=3, "Scottish National Party (SNP)"=4,
                                        "Plaid Cymru"=5, "Green Party"=6, "United Kingdom Independence Party (UKIP)"=7, "Independent/Other"=8),
                            force.values = FALSE)

newdataT$d01 <- set_labels(newdataT$d01,
                           labels = c("No party/Uncommitted"=0,"Labour"=1, "Conservative"=2, " Liberal Democrat"=3, "Scottish National Party (SNP)"=4,
                                      "Plaid Cymru"=5, "Green Party"=6, "United Kingdom Independence Party (UKIP)"=7, "Independent/Other"=8),
                           force.values = FALSE)

newdataT$e01 <- set_labels(newdataT$e01, labels =c("Left" = 0, "1"=1, "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7, "8"=8, "9"=9, "Right"=10),
                           force.values = FALSE)

newdataT$j01 <- set_labels(newdataT$j01, labels= c("Bad for the economy", "2", "3", "4", "5", "6", "Good for the economy"),
                           force.values = FALSE)

newdataT$p01 <- set_labels(newdataT$p01, labels = c("I didn NOT vote"=1, "Leave the EU"=2, "Remain in the EU"=3),
                           force.values = FALSE)

#vote/not vote on Brexit Ref var
newdataT$p04 <- car::recode(newdataT$p01, "3=2")

newdataT$p04 <- set_labels(newdataT$p04, labels = c("I did NOT vote on Brexit"=1, "I did vote on Brexit"=2),
                           force.values = FALSE)

newdataT$p02 <- set_labels(newdataT$p02, labels = c("I would NOT vote"=1,"Leave the EU"=2, "Remain in the EU"=3),
                           force.values = FALSE)

newdataT$p03_1 <- set_labels(newdataT$p03_1, labels = c("It can to unite fully with the EU"=0,"1"=1,
                                                        "2"=2, "3"=3, "4"=4, "5"=5, "6"=6, "7"=7, "8"=8, "9"=9, 
                                                        "It can to protect its independence from the EU"=10),
                             force.values = FALSE)

newdataT$p03b <- set_labels(newdataT$p03b, labels = c("Yes", "No"),
                            force.values = FALSE)

newdataT$y09 <- set_labels(newdataT$y09, labels = c("Male", "Female"),
                           force.values = FALSE)

newdataT$y01 <- set_labels(newdataT$y01, labels = c("Under GBP 2,600", "GBP 2,600 - GBP 5,199","GBP 5,200 - GBP 10,399", 
                                                    "GBP 10,400 - GBP 15,599","GBP 15,600 - GBP 20,799","GBP 20,800 - GBP 25,999",
                                                    "GBP 26,000 - GBP 31,199","GBP 31,200 - GBP 36,399","GBP 36,400 - GBP 39,999",
                                                    "GBP 40,000 - GBP 44,999","GBP 45,000 - GBP 49,999","GBP 50,000 - GBP 59,999",
                                                    "GBP 60,000 - GBP 74,999","GBP 75,000 - GBP 99,999", " GBP 100,000 or more"),
                           force.values = FALSE)

newdataT$y10_banded <- set_labels(newdataT$y10_banded, labels = c("18-24","25-34","35-44","45-54",
                                                                  "55-64","65-74","75-84","85+" ),
                                  force.values = FALSE)

#so, the dta file has labels on it but it doesn't show them?
newdataT$region <- set_labels(newdataT$region, labels = c( "East Midlands"= "1","Eastern"=2,"London"=3,"North East"=4, 
                                                           "North West"=5, "Scotland"=6, "South East"=7, "South West"=8,
                                                           "Wales" =9, "West Midlands"=10, "Yorkshire & Humber"=11 ),
                              force.values = TRUE)

#the other two variables of education and region were already good so nothing to be done there.
#Now, for the app we'll use the clean dataset (csv) with the labels of the dta cleaned file (above)

#creating subsets of the DataBES1 variables and sorting them out into nominal and ordinal variables
#Nominal - no order
vars <- c(
  "Party Vote" = "VotePty",
  "General Party Voter" = "GeneralPty",
  "Brexit Vote" = "VoteEU",
  "Second Brexit Vote" = "VoteEU2",
  "Scottish Referndum" = "RefDeal",
  "EU View"="EUOwn",
  "Gender"="Gender",
  "Region"="Region"
)

# Ordinal - have a specific order
vars1 <- c(
  "Political Ideology" = "LeftRight",
  "Immigration & Economy" = "ImmigEcon",
  "College education" = "EUOwn",
  "Average Income" = "Income",
  "Age" = "AgeBand",
  "Education level"="EdLevel"
)


#####
ColAttr <- function(x, attrC, ifIsNull) {
  # Returns column attribute named in attrC, if present, else isNullC.
  atr <- attr(x, attrC, exact = TRUE)
  atr <- if (is.null(atr)) {ifIsNull} else {atr}
  atr
}
AtribLst <- function(df, attrC, isNullC){
  # Returns list of values of the col attribute attrC, if present, else isNullC
  lapply(df, ColAttr, attrC=attrC, ifIsNull=isNullC)
}
variables <- AtribLst(newdataT, attrC="labels", isNullC=NA)

my_max <- 5

# Define UI for application that draws a histogram
ui <-  dashboardPage(skin= "green",
                     #),
                     dashboardHeader(title ="British Elections Studies Survey",titleWidth = 550),
                     # Application title
                     #headerPanel("Scottish Index of Multiple Deprivation"),
                     #br(),
                     dashboardSidebar(width = 400,
                                      sidebarMenu(style="font-size:140%;",
                                                  menuItem("Information", tabName = "Info"),
                                                  menuItem("Summary Statistics and Distributions", tabName = "Stats"),
                                                  menuItem("Comparison of Variables", tabName = "Comparison"),
                                                  menuItem("Brexit Comparisons", tabName = "Brexit"),
                                                  menuItem("Crosstabs and Measures of Association", tabName = "Crosstabs")
                                      )
                     ),
                     
                     dashboardBody(width = 400,
                                   tags$head(
                                     
                                     tags$style(HTML("
                                         @import url('https://fonts.googleapis.com/css?family=Neucha|Cabin+Sketch');
                                         
                                         .main-header .logo {
                                         font-family: 'Papyrus', cursive;
                                         font-size: 200%;
                                         color: #003366;
                                         }
                                         
                                         ")),
                                     
                                     tags$style(HTML("
                                         @import url('https://fonts.googleapis.com/css?family=Neucha|Cabin+Sketch');
                                         .selectize-input {
                                         font-family: 'Perpetua', Times, serif;
                                         font-size: 160%;
                                         color: #212539;
                                         }
                                         
                                         "))
                                   ),
                                   
                                   tabItems(
                                     
                                     tabItem(tabName = "Info",
                                             tags$img(src = "qstep2.png", height = 110, width = 220), 
                                             tags$img(src = "university.png.png", height = 110, width = 250),
                                             br(),
                                             br(),
                                             p("This app will be using the British Election Survey of 2016 to showcase measures of central tendency, statistical tests such as chi-square and t-tests as well as measures of association.
                                   In terms of visualisations it has variable frequencies plots, histograms, and crosstabulations.",style="font-size:160%;"),
                                             br(),
                                             p("The aim of this app is to explain the different relationships between the variables of the British Election Survey. In particular it will look at the relationships between ordinal and nominal variables.
                                   The data obtained from the British Election Survey University Consortium. This organism has been collecting UK electoral behaviour data for over fifty years.",style="font-size:160%;"),
                                             br(),
                                             p("Developed by Cristina Chueca, Q-Step Graduate of the University of Glasgow 2018, 
                                   in collaboration with Dr. Brian Fogarty and Dr. Niccole Pamphilis.",style="font-size:160%;"),
                                             br()
                                     ),
                                     
                                     
                                     
                                     tabItem(tabName = "Stats", 
                                             
                                             sidebarPanel(selectInput("selection", "Covariates:", choices = names(dataBES1), selected = "WhichPty"), 
                                                          plotOutput("Explanation"), width = 4, verbatimTextOutput("Summary")),
                                             
                                             mainPanel(plotOutput("distPlot"), 
                                                       width = 7)
                                     ),
                                     
                                     tabItem(tabName ="Comparison",
                                             
                                             
                                             sidebarPanel(selectInput("cov1", "Covariate 1", choices = names(dataBES1)), selectInput("cov2", "Covariate 2", choices = names(dataBES1),selected = "GeneralPty"),
                                                          
                                                          verbatimTextOutput("Explanation1"), verbatimTextOutput("Explanation2"),
                                                          
                                                          checkboxInput(inputId = "addper",
                                                                        label = "Would you like to add percentages to the graph?",
                                                                        value = FALSE)),
                                             
                                             
                                             
                                             column(width = 7,
                                                    
                                                    mainPanel(plotOutput("compPlot")))
                                     ),
                                     
                                     
                                     tabItem(tabName ="Brexit", 
                                             
                                             fluidRow(
                                               
                                               
                                               
                                               #column(   
                                               
                                               sidebarPanel(width = 2, selectInput("cov3", "Brexit Vote (past)", choices = names(dataBES1),selected = "VoteEU"),
                                                            selectInput("cov4", "Covariate 2", choices = names(dataBES1)),
                                                            
                                                            checkboxInput(inputId = "addper1",
                                                                          label = "Would you like to add percentages to the graph?",
                                                                          value = FALSE)),
                                               #),
                                               
                                               column(width = 4,                          
                                                      
                                                      verbatimTextOutput("Explanation3"), verbatimTextOutput("Explanation4")),
                                               
                                               column(width = 6,
                                                      
                                                      mainPanel(plotOutput("compPlot2")))       
                                               
                                             ),
                                             
                                             fluidRow(
                                               
                                               # column(
                                               
                                               sidebarPanel(width = 2, selectInput("cov5", "Brexit Vote (future)", choices = names(dataBES1), selected = "VoteEU2"), 
                                                            selectInput("cov6", "Covariate 2", choices = names(dataBES1)),
                                                            
                                                            checkboxInput(inputId = "addper2",
                                                                          label = "Would you like to add percentages to the graph?",
                                                                          value = FALSE)),
                                               #),
                                               
                                               column(width = 4,    
                                                      
                                                      verbatimTextOutput("Explanation5"), verbatimTextOutput("Explanation6")),
                                               
                                               column(width = 6,                        
                                                      mainPanel(plotOutput("compPlot3")))
                                               
                                             )
                                             
                                     ),
                                     
                                     
                                     tabItem(tabName = "Crosstabs",
                                             
                                             fluidRow(width = 6,
                                                      
                                                      sidebarPanel(selectInput("nominal", "Nominal Variables", vars),
                                                                   selectInput("ordinal", "Ordinal Variables", vars1),
                                                                   verbatimTextOutput("ExplanationVar"),  verbatimTextOutput("ExplanationVar1")
                                                                   
                                                      ),
                                                      
                                                      
                                                      mainPanel(
                                                        
                                                        box(width = 12,title = "Measures of Association",
                                                            solidHeader = F, status = "primary",
                                                            
                                                            tabsetPanel(type = "tabs",
                                                                        tabPanel("Cramer's V and Chi Square Tests",
                                                                                 h4("Measures of association are used to examine the relationships between variables.
                                                            One of these measures is Cramer's V, which indicates the strength of the relationship between 
                                                             one nominal and one ordinal variable. It's a value bound between 0 and 1."), 
                                                                                 
                                                                                 checkboxInput(inputId = "Cramers", label = "Cramer's V", value = T), verbatimTextOutput("Cramer"),br(),
                                                                                 
                                                                                 h4("Chi Square is used to evaluate whether the relationship between categorical variables is statistically 
                                                           significant. It compares the observed frequencies from the crosstabulation against the expected frequencies."),
                                                                                 checkboxInput(inputId = "Chi", label = "Chi-Square", value = T), verbatimTextOutput("ChiS"))#,
                                                                        
                                                                        
                                                                        
                                                            )
                                                        ))),
                                             
                                             fluidRow(
                                               box(width = 12,title = "Crosstabulation ",
                                                   solidHeader = FALSE, status = "warning",
                                                   verbatimTextOutput("Explanation7"))
                                             )
                                             
                                             
                                             #)
                                             
                                             
                                     )        
                                   )
                     )
                     
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  selectedData<-reactive({
    as.numeric(unlist(dataBES1[,input$selection]))
  })
  
  output$Summary <- renderPrint({
    
    summary(selectedData())
    
  })
  
  output$distPlot <- renderPlot({
    plot.data<-round(selectedData(),2)
    barplot(prop.table(table(plot.data)), main = "Frequency bar plot", ylim = c(0, 1), xlab = "Categories", cex.axis = 2, cex.main = 2)
    mp<-barplot(prop.table(table(plot.data)), main = label(dataBES1[,input$selection]), ylim= c(0, 1),beside = TRUE, plot = FALSE)
    text(x=mp,y=prop.table(table(dataBES1[,input$selection])),
         labels=paste0(round(prop.table(table(as.numeric(unlist(dataBES1[,input$selection])))),3)*100, "%"), pos=3, adj = 2,xpd=NA)
  }, height = 600)
  
  
  
  output$Explanation<-renderPlot({
    id<-which(names(dataBES1) == input$selection)
    if(!is.na(variables[id])){
      grid.draw(tableGrob(data.frame(variables[id])))}
  })
  
  output$indplot<-renderPlot({
    plot(dataBES1[,input$indiv], dataBES1[,"vote"])
  })
  
  output$Explanation1<-renderPrint({
    variables <- AtribLst(newdataT, attrC="labels", isNullC=NA)
    id<-which(names(dataBES1) == input$cov1)
    if(!is.na(variables[id])){
      data.frame(variables[id])}
  })
  
  output$Explanation2<-renderPrint({
    variables <- AtribLst(newdataT, attrC="labels", isNullC=NA)
    id<-which(names(dataBES1) == input$cov2)
    if(!is.na(variables[id])){
      data.frame(variables[id])}
  })
  
  
  output$compPlot<-renderPlot({
    
    if (input$addper == "TRUE") {
      
      counts<-as.data.frame(table(unlist(dataBES1[,input$cov1]), unlist(dataBES1[,input$cov2])))
      names(counts)<-c(input$cov1, input$cov2, "frequency")
      counts1<-data.table(counts)
      counts1[, percen := sum(frequency), by=eval(input$cov1)]
      counts1[, percen := paste0(round(frequency/percen*100, 0), "%")]
      ggplot(counts1, aes_string(x = input$cov1, y = "frequency", fill = input$cov2,label = "percen"))+
        geom_bar(stat = "identity") + ggtitle(paste0(label(dataBES1[,input$cov1]), " Comparison ", label(dataBES1[, input$cov2])))+
        geom_text(size = 5, position = position_stack(vjust = 0.5))+
        theme(axis.text=element_text(size=12, face = "bold"),axis.title=element_text(size=14,face="bold"))+ theme(plot.title = element_text(size=22))
      
      
    } else if (input$addper == "FALSE") {
      counts<-as.data.frame(table(unlist(dataBES1[,input$cov1]), unlist(dataBES1[,input$cov2])))
      names(counts)<-c(input$cov1, input$cov2, "frequency")
      counts1<-data.table(counts)
      counts1[, percen := sum(frequency), by=eval(input$cov1)]
      counts1[, percen := paste0(round(frequency/percen*100, 0), "%")]
      ggplot(counts1, aes_string(x = input$cov1, y = "frequency", fill = input$cov2,label = "percen"))+
        geom_bar(stat = "identity") + ggtitle(paste0(label(dataBES1[,input$cov1]), " Comparison ", label(dataBES1[, input$cov2])))+
        theme(axis.text=element_text(size=12, face = "bold"),axis.title=element_text(size=14,face="bold"))+ theme(plot.title = element_text(size=22))
      
    }}, height = 600, width = 750)
  
  
  output$Explanation3<-renderPrint({
    variables <- AtribLst(newdataT, attrC="labels", isNullC=NA)
    id<-which(names(dataBES1) == input$cov3)
    if(!is.na(variables[id])){
      data.frame(variables[id])}
  })
  
  output$Explanation4<-renderPrint({
    variables <- AtribLst(newdataT, attrC="labels", isNullC=NA)
    id<-which(names(dataBES1) == input$cov4)
    if(!is.na(variables[id])){
      data.frame(variables[id])}
  })
  
  
  output$compPlot2<-renderPlot({
    
    if (input$addper1 == "TRUE") {
      counts<-as.data.frame(table(unlist(dataBES1[,input$cov3]), unlist(dataBES1[,input$cov4])))
      names(counts)<-c(input$cov3, input$cov4, "frequency")
      counts1<-data.table(counts)
      counts1[, percen := sum(frequency), by=eval(input$cov3)]
      counts1[, percen := paste0(round(frequency/percen*100, 0), "%")]
      ggplot(counts1, aes_string(x = input$cov3, y = "frequency", fill = input$cov4,label = "percen"))+
        geom_bar(stat = "identity") + ggtitle(paste0(label(dataBES1[,input$cov3]), " Comparison ", label(dataBES1[, input$cov4])))+
        geom_text(size = 5, position = position_stack(vjust = 0.5))+
        theme(axis.text=element_text(size=12, face = "bold"),axis.title=element_text(size=14,face="bold"))+
        theme(plot.title = element_text(size=22))
      
    } else if (input$addper1 == "FALSE") {
      counts<-as.data.frame(table(unlist(dataBES1[,input$cov3]), unlist(dataBES1[,input$cov4])))
      names(counts)<-c(input$cov3, input$cov4, "frequency")
      counts1<-data.table(counts)
      counts1[, percen := sum(frequency), by=eval(input$cov3)]
      counts1[, percen := paste0(round(frequency/percen*100, 0), "%")]
      ggplot(counts1, aes_string(x = input$cov3, y = "frequency", fill = input$cov4,label = "percen"))+
        geom_bar(stat = "identity") + ggtitle(paste0(label(dataBES1[,input$cov3]), " Comparison ", label(dataBES1[, input$cov4])))+
        theme(axis.text=element_text(size=12, face = "bold"),axis.title=element_text(size=14,face="bold"))+
        theme(plot.title = element_text(size=22))
      
      
    }}, height = 350, width = 550)
  
  output$Explanation5<-renderPrint({
    variables <- AtribLst(newdataT, attrC="labels", isNullC=NA)
    id<-which(names(dataBES1) == input$cov5)
    if(!is.na(variables[id])){
      data.frame(variables[id])}
  })
  
  output$Explanation6<-renderPrint({
    variables <- AtribLst(newdataT, attrC="labels", isNullC=NA)
    id<-which(names(dataBES1) == input$cov6)
    if(!is.na(variables[id])){
      data.frame(variables[id])}
  })
  
  output$compPlot3<-renderPlot({
    
    if (input$addper2 == "TRUE") {
      counts<-as.data.frame(table(unlist(dataBES1[,input$cov5]), unlist(dataBES1[,input$cov6])))
      names(counts)<-c(input$cov5, input$cov6, "frequency")
      counts1<-data.table(counts)
      counts1[, percen := sum(frequency), by=eval(input$cov5)]
      counts1[, percen := paste0(round(frequency/percen*100, 0), "%")]
      ggplot(counts1, aes_string(x = input$cov5, y = "frequency", fill = input$cov6,label = "percen"))+
        geom_bar(stat = "identity") + ggtitle(paste0(label(dataBES1[,input$cov5]), " Comparison ", label(dataBES1[, input$cov6])))+
        geom_text(size = 5, position = position_stack(vjust = 0.5))+
        theme(axis.text=element_text(size=12, face = "bold"),axis.title=element_text(size=14,face="bold"))+
        theme(plot.title = element_text(size=22))
      
    } else if (input$addper2 == "FALSE") { 
      counts<-as.data.frame(table(unlist(dataBES1[,input$cov5]), unlist(dataBES1[,input$cov6])))
      names(counts)<-c(input$cov5, input$cov6, "frequency")
      counts1<-data.table(counts)
      counts1[, percen := sum(frequency), by=eval(input$cov5)]
      counts1[, percen := paste0(round(frequency/percen*100, 0), "%")]
      ggplot(counts1, aes_string(x = input$cov5, y = "frequency", fill = input$cov6,label = "percen"))+
        geom_bar(stat = "identity") + ggtitle(paste0(label(dataBES1[,input$cov5]), " Comparison ", label(dataBES1[, input$cov6])))+
        theme(axis.text=element_text(size=12, face = "bold"),axis.title=element_text(size=14,face="bold"))+
        theme(plot.title = element_text(size=22))
      
    }}, height = 350, width = 550)
  
  #Crosstabs tab 
  
  output$ExplanationVar<-renderPrint({
    
    variables <- AtribLst(newdataT, attrC="labels", isNullC=NA)
    id<-which(names(dataBES1) == input$nominal)
    if(!is.na(variables[id])){
      data.frame(variables[id])}
  })
  
  output$ExplanationVar1<-renderPrint({
    
    variables <- AtribLst(newdataT, attrC="labels", isNullC=NA)
    id<-which(names(dataBES1) == input$ordinal)
    if(!is.na(variables[id])){
      data.frame(variables[id])}
  })
  
  #subsets  
  selectedData1<-reactive({
    
    as.numeric(unlist(dataBES1[,input$nominal]))
  })
  
  #subset 2
  selectedData2<-reactive({
    as.numeric(unlist(dataBES1[,input$ordinal]))
  })
  
  
  output$Explanation7<-renderPrint({
    
    CrossTable(selectedData1(), selectedData2(), prop.r = F, prop.c = F, prop.t = F,
               chisq = F)
    
  })
  
  output$txt <- renderPrint({ input$txt })
  
  
  output$Cramer <- renderPrint({
    
    if (input$Cramers == "TRUE") {
      
      CramerV(selectedData1(), selectedData2())
      
      
    } else if (input$Cramers == "FALSE") { }
    
  })
  
  output$ChiS <- renderPrint({
    
    if (input$Chi == "TRUE") {
      
      chisq.test(selectedData1(), selectedData2())
      
      
    } else if (input$Chi == "FALSE") { }
    
  })  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
