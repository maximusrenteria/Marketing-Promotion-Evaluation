#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(car)
library("report")
library(lmtest)

# Define server logic required to draw a histogram
function(input, output, session) {
    ### data visualization
    ## data set
    data <- read.csv("WA_Marketing-Campaign.csv")
    output$mytable = DT::renderDataTable({
      read.csv("WA_Marketing-Campaign.csv")
    })
    
    # sales
    output$plot1 = renderPlot(ggplot(data %>%
                            group_by(Promotion) %>%
                            summarize(Sales=sum(SalesInThousands))%>%
                            mutate(Percentage=round(Sales/sum(Sales)*100)), aes(x="", y=Percentage, fill=Promotion)) +
                              geom_bar(width=1, stat="identity", position=position_fill()) +
                              geom_text(aes(x=1.25, label=Percentage), position=position_fill(vjust=0.5), color="white", size=10) +
                              coord_polar("y") +
                              ggtitle("Sales distribution by promotion, %") +
                              theme_classic() +
                              theme(legend.title = element_text(size = 20), legend.text = element_text(size = 20), plot.title = element_text(size = 30),
                                    axis.title.x = element_text(size = 25))
                            )
    
    # market size
    output$plot2 = renderPlot(ggplot(data %>%
                                       group_by(Promotion, MarketSize) %>%
                                       summarize(Count=n()), 
                                     aes(x=Promotion, y=Count, fill=MarketSize)) +
                                geom_bar(width=0.5, stat="identity", position="stack") +
                                ylab("Count") +
                                xlab("Promotion") +
                                ggtitle("Market sizes by promotion") +
                                theme(legend.text=element_text(size=20), plot.title = element_text(size=27), axis.title = element_text(size = 25)))
    
    # store age
    output$plot3 = renderPlot(ggplot(data %>%
                                       group_by(AgeOfStore) %>%
                                       summarize(Count=n()),
                                     aes(x=AgeOfStore, y=Count)) +
                                geom_bar(width=0.5, stat="identity", fill="orange") +
                                ylab("Count") +
                                xlab("Store Age") +
                                ggtitle("Store Age Distribution") +
                                theme(legend.text=element_text(size=20), plot.title = element_text(size=27), axis.title = element_text(size = 25))
                              )
    ##AB Testing
    
    # analysis of variance by plot
    
    output$plot4 = renderPlot(boxplot(SalesInThousands ~ Promotion, data
                                      ))
    
    output$plot5 = renderPlot(ggplot(data, aes(x=as.factor(data$Promotion), y=SalesInThousands)) + 
                                geom_dotplot(binaxis='y', stackdir='center',
                                             stackratio=0.5, dotsize=0.5))
    
    # levene test output
    Promotion <- as.character(data$Promotion)
    levtestoutput <- leveneTest(data$SalesInThousands ~ Promotion)
    output$LevenesTest = renderPrint(levtestoutput)
    
    # anova output
    
    output$anovaowm <- renderPrint(oneway.test(SalesInThousands ~ as.factor(data$Promotion),
                                               data = data,
                                               var.equal = TRUE # assuming equal variances
    ))
    
    output$anovaaov <- renderPrint(summary(aov(SalesInThousands ~ as.factor(data$Promotion),
                                               data = data
    )))
    
    output$aovreport <- renderPrint(report(aov(SalesInThousands ~ as.factor(data$Promotion),
                                               data = data
    )))
    
    # t test output
    promo_1 <- data[which(data$Promotion==1),]$SalesInThousands
    promo_2 <- data[which(data$Promotion==2),]$SalesInThousands
    promo_3 <- data[which(data$Promotion==3),]$SalesInThousands
    output$t_test12 <- renderPrint(t.test(promo_1, promo_2))
    output$t_test23 <- renderPrint(t.test(promo_2, promo_3))
    output$t_test13 <- renderPrint(t.test(promo_1, promo_3))
    output$conclusion <- renderText("Promo 1 and Promo 3 have similar effects and Promo 2 has a unique effect")
    
    ### Project Performance
    
    # Plots of Promotion Sales over Time and by Market Size
    ## Promotion 1
    Promotion1dta <- subset(data, Promotion == 1)
    
    ## Promotion 2
    Promotion2dta <- subset(data, Promotion == 2)
    
    ## Promotion 3
    Promotion3dta <- subset(data, Promotion == 3)
    # Promotion 1 Plot
    output$plot6 <- renderPlot(ggplot(Promotion1dta, aes(x=week, y=SalesInThousands, color=MarketSize, )) + 
                                 geom_point() + 
                                 geom_smooth(method=lm, aes(fill=week), fullrange=TRUE))
    # Promotion 2 Plot
    output$plot7 <- renderPlot(ggplot(Promotion2dta, aes(x=week, y=SalesInThousands, color=MarketSize, )) + 
                                 geom_point() + 
                                 geom_smooth(method=lm, aes(fill=week), fullrange=TRUE))
    # Promotion 3 Plot
    output$plot8 <- renderPlot(ggplot(Promotion3dta, aes(x=week, y=SalesInThousands, color=MarketSize, )) + 
                                 geom_point() + 
                                 geom_smooth(method=lm, aes(fill=week), fullrange=TRUE))
    
    # Regression
    output$regression <- renderPrint(summary(lm(SalesInThousands ~ as.factor(data$Promotion) + MarketSize + AgeOfStore, data)))
    
    #Plots of Promotion Sales over Time and by Market ID
    Promotion1dta$MarketID <- cut(Promotion1dta$MarketID, c(1,2, 3, 4, 5, 6, 7, 8, 9, 10), right=FALSE)
    Promotion2dta$MarketID <- cut(Promotion2dta$MarketID, c(1,2, 3, 4, 5, 6, 7, 8, 9, 10), right=FALSE)
    Promotion3dta$MarketID <- cut(Promotion3dta$MarketID, c(1,2, 3, 4, 5, 6, 7, 8, 9, 10), right=FALSE)
    #Promotion 1
    output$plot9 <- renderPlot(ggplot(Promotion1dta, aes(x=week, y=SalesInThousands, shape=MarketID, color=MarketID, )) + 
                                 geom_point() + 
                                 geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
    )
    # Promotion 2
    output$plot10 <- renderPlot(ggplot(Promotion2dta, aes(x=week, y=SalesInThousands, shape=MarketID, color=MarketID, )) + 
                                 geom_point() + 
                                 geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
    )
    # Promotion 3
    output$plot11 <- renderPlot(ggplot(Promotion3dta, aes(x=week, y=SalesInThousands, shape=MarketID, color=MarketID, )) + 
                                 geom_point() + 
                                 geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
    )
    


}
