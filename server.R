
library(shiny)

period.do <- function(date1, date2, dec){
    if(dec=="Yearly"){out <- as.numeric(date2-date1)/365.25}
    if(dec=="Monthly"){out <- (as.numeric(date2-date1)/365.25)*12}
    if(dec=="Daily"){out <- as.numeric(date2-date1)}
    c(out)
}

period2.do <- function(dec){
    if(dec=="Yearly"){out <- "Years"}
    if(dec=="Monthly"){out <- "Months"}
    if(dec=="Daily"){out <- "Days"}
    c(out)
}

Ip.do <- function(Interest, Ip){
    if(Ip=="Year"){out <- Interest}
    if(Ip=="Month"){out <- Interest*12}
    if(Ip=="Day"){out <- Interest*365.25}
    out
}


I.do <- function(date1, date2, dec, Capital, Interest, currency, ci=T){
    time <- floor(period.do(date1, date2, dec)[1])
    if(dec=="Yearly"){out <- (Capital*Interest*time)/100}
    if(dec=="Monthly"){out <- (Capital*Interest*time)/(100*12)}
    if(dec=="Daily"){out <- (Capital*Interest*time)/(100*365.25)}
    ifelse(missing(currency), return(out), 
           ifelse(ci==T, return(paste(round(Capital+out, digits=2), currency, sep=" ")), 
                  return(paste(round(out, digits=2), currency, sep=" ")))
    )
}

IoI.do <- function(date1, date2, dec, Capital, Interest, currency, ci=1){
    time <- round(period.do(date1, date2, dec)[1], digits=0)
    out <- IoI2.do(time, dec, Capital, Interest)
    ifelse(missing(currency), return(out), 
           ifelse(ci==1, return(paste(round(out, digits=2), currency, sep=" ")), 
                  return(paste(round(out-Capital, digits=2), currency, sep=" "))
                  )
           )
}

IoI2.do <- function(time, dec, Capital, Interest){
    if(dec=="Yearly"){out <- Capital*((100+(Interest/1))/100)^time}
    if(dec=="Monthly"){out <- Capital*((100+(Interest/12))/(100))^time}
    if(dec=="Daily"){out <- Capital*((100+(Interest/365.25))/(100))^time}
    out
}


I.plot.do <- function(date1, date2, dec, Capital, Interest){
    time <- floor(period.do(date1, date2, dec)[1])
    I <- I.do(date1, date2, dec, Capital, Interest)
    out0 <- numeric()
    for(i in 0:(time-1)){out0[i+1]<-(I/time)*i}
    out <- cbind(c(Capital, 0, 0), rbind(rep(Capital, time), out0, rep(I/time, time)))
    colnames(out)<- 0:time
    out
}

IoI.plot.do <- function(date1, date2, dec, Capital, Interest){
    time <- floor(period.do(date1, date2, dec)[1])
    I <- IoI.do(date1, date2, dec, Capital, Interest)
    out0 <- numeric()
    out1 <- numeric()
    out2 <- numeric()
    out0[1]<-Capital
    for(i in 1:time){
        out0[i+1]<-IoI2.do(i, dec, Capital, Interest)
        out1[i+1]<-out0[i]       
        out2[i+1]<-out0[i+1]-out0[i]
    }
    out <- cbind(c(Capital, 0), rbind(out1[-1], out2[-1]))
    colnames(out)<- 0:time
    out
}



# Define server logic required to plot various variables against mpg
shinyServer(
    function(input, output) {
        output$Scapital <- renderText({paste(input$Capital, input$currency, sep=" ")})
        output$Interest <- renderText({paste(Ip.do(input$Interest, input$Ip), "% (", input$getmoney, ")", sep=" ")})
        
        
        output$Period <- renderText({paste(round(period.do(input$date1, input$date2, input$getmoney), digits = 2), 
                                           period2.do(input$getmoney), sep=" ")})
        
        output$Einterest <- renderText({ifelse(period.do(input$date1, input$date2, input$getmoney)>=0,
                                               ifelse(input$IoI=="No", I.do(input$date1, input$date2, input$getmoney, 
                                                                            input$Capital, Ip.do(input$Interest, input$Ip), input$currency, ci=F),
                                                      IoI.do(input$date1, input$date2, input$getmoney, 
                                                             input$Capital, Ip.do(input$Interest, input$Ip), input$currency, ci=2)),
                                               "Estimation not possible.")})
        
        output$Ecapital <- renderText({ifelse(period.do(input$date1, input$date2, input$getmoney)>=0,
                                              ifelse(input$IoI=="No", I.do(input$date1, input$date2, input$getmoney, 
                                                                           input$Capital, Ip.do(input$Interest, input$Ip), input$currency),
                                                     IoI.do(input$date1, input$date2, input$getmoney, 
                                                            input$Capital, Ip.do(input$Interest, input$Ip), input$currency)), 
                                              "Estimation not possible.")})

        
        output$plot <- renderPlot({ifelse(period.do(input$date1, input$date2, input$getmoney)>=1,
                                          ifelse(input$IoI=="No",
                                                 barplot(I.plot.do(input$date1, input$date2, input$getmoney, input$Capital, Ip.do(input$Interest, input$Ip)),  
                                                         legend.text = c("Capital", "Gained Interests", "New gained Interests"),  args.legend = list(x = input$lpos), 
                                                         xlab = period2.do(input$getmoney), ylab=input$currency, main="Capital and Interests"),            
                                                 barplot(height = IoI.plot.do(input$date1, input$date2, input$getmoney, input$Capital, Ip.do(input$Interest, input$Ip)),  
                                                         legend.text = c("Capital", "New gained Interests"),  args.legend = list(x =  input$lpos), 
                                                         xlab = period2.do(input$getmoney), ylab=input$currency, main="Capital and Interests on Interests")),
                                          ifelse(period.do(input$date1, input$date2, input$getmoney)>=0, 
                                                 barplot(height=c('0'=input$Capital), legend.text = c("Capital"),  args.legend = list(x =  input$lpos), 
                                                         xlab = period2.do(input$getmoney), ylab=input$currency, main="Capital and Interests"), 
                                                 barplot(height=c('0'=input$Capital), legend.text = c("Capital"),  args.legend = list(x =  input$lpos), 
                                                         xlab = period2.do(input$getmoney), ylab=input$currency, main="Capital and Interests", 
                                                         sub="Estimation not possible.")
                                          )
                                   )                                 
        })
        
        output$form1 <- renderText({"I = (Capital*Interest*time)/100)"})
        output$form2 <- renderText({"IoI = Capital*((100+(Interest))/100)^time"})
    }
    
)

