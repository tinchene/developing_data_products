#################
# shiny project
#################

# this program uses the input 

library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
    
    # Application title
    headerPanel("Interest Estimation"),
    
    sidebarPanel(
        numericInput('Capital', 'Capital', 1000, min=0, max = 1000000000, step=0.01),
        textInput("currency", "Currency", "€"),
        numericInput('Interest', 'Interest rate [%]', 5, min=0, max = 1000000000, step=0.01),
        
        radioButtons("Ip", "Interest rate per", choices=c("Year", "Month", "Day"), selected = "Year", inline=TRUE), 
        
        radioButtons("getmoney", "Interest Payment", choices=c("Yearly", "Monthly", "Daily"), inline=TRUE), 
        
        dateInput("date1", "Start Date"),
        dateInput("date2", "End Date"),
        
                
        radioButtons("IoI", "Interrest on Interest?", choices =c("Yes","No"), selected = "No", inline=TRUE),
        
        radioButtons("lpos", "Legend Position?", choices =c("topleft","topright", "bottomleft", "bottomright"), selected = "topleft")
        
        #checkboxInput("IoI", "Interrest on Interest?", TRUE)#c("Yes"="1"), inline=TRUE)
    ),
    
    mainPanel(

        tabsetPanel(type = "tabs", 
                    #tabPanel("Summary", h4(textOutput("test")),verbatimTextOutput("summary")),
                    tabPanel("Summary", 

                             h3('Parameters'),
                             
                             h4('Start capital'),
                             textOutput('Scapital'),
                             h4('Interest rate'),
                             textOutput('Interest'),
                             h4('Time'),
                             textOutput('Period'),
                             br(),
                             br(),
                             h3('Result'),

                             h4('Interests'),
                             textOutput('Einterest'),                             
                             h4('End Capital'),
                             strong(textOutput('Ecapital'))),
                    
                    tabPanel("Plots", 
                             plotOutput('plot')),
                    tabPanel("Documentation",
                             h3('Idea'),
                             div("This App should calculate how rich you can get
                                 with a certain capital and interest over a time period."),
                             h3('Using'),
                             div("In the first box you can fill your start capital (default 1000)
                                 and in the second box the currency of the start capital (default €). 
                                 The third box is the interest rate in percent, which could not be negative. You can decide, whether the interest rate
                                 is per year, month or day and you can decide, when the rate is payed, yearly, monthly and daily.
                                 The default start and end date is the acutal date. You can change this as you want, but negative time periods will not lead to an end capital. 
                                 You can also decide, whether there should be interests on interests or not.
                                 In the summary panel the results are shown numerically and in the plots panel as a barplot.
                                 For nicer view of the plot it is possible to define the position of the legend."),
                             h3('Assumptions'),
                             div("The used formulas are the standard procedure for
                                 estimating interests (I) and interests of interests (IoI):"),
                      
                             verbatimTextOutput("form1"),
                             verbatimTextOutput("form2"),
                             div("For details see the
                                 presentation. For estimating a year, month and a day, it is assumed that a year has
                                 365.25 days (respecting the leap years) and 12 months."))
                    
        )
    )
))

