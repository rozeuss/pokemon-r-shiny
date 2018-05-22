library(shiny)

library(lattice)
library(ggplot2)
mycol<-c("yellow3","burlywood4","mediumslateblue","orange","pink1","red3","chocolate1","plum3","plum4","green1","tan1","turquoise","navajowhite4",
		"maroon","deeppink","darkgoldenrod3","darkgray","cornflowerblue")
palette(mycol)
dataA <- read.csv(file="Pokemon.csv")
attach(dataA)

ui <- navbarPage(title = "PokeDex",
##############################################################################
tabPanel(title = "Stwórz wykres",

		fluidRow(
   			 column(3,
   			   h4("Pokemon Explorer"),
				h5("Wybierz zmienne:"),
				selectInput('xcol', 'Zmienna X', names(dataA[,c(5:13)])),
  				selectInput('ycol', 'Zmienna Y', names(dataA[,c(5:13)]),
     				selected = names(dataA)[[6]])
   				 )
 		 ),

  			hr(),
    			plotOutput('plot1'),
			br()

 		   ),	
###############################################################################
tabPanel(title = "Total",


	sidebarPanel(
		selectInput('yy1', 'Y1 Variable', names(dataA[,c(6:11)])),
		selectInput('yy2', 'Y2 Variable', names(dataA[,c(6:11)]),
					selected = names(dataA)[7]),
		selectInput('idName', 'Pokemon', unique(dataA$Name)),

	tags$style(type='text/css', '#selectedStats { font-size: 1em;color: black;}'), 

  		verbatimTextOutput("selectedStats"),
radioButtons("Generacja", label = h4("Generation"), 
	choices = c("1st" = 1, "2nd" = 2, "3rd" = 3,"4th" = 4,"5th" = 5,"6th" = 6, "All" = 10),
    selected = 1)
  	),
 	mainPanel(
    		plotOutput('unif')

  	),
h4("Top 5 stats (Y1 + Y2)"),
verbatimTextOutput("stats")
    ),

###############################################################################
tabPanel(title = "Histogram wybranej statystyki",

sidebarPanel(
selectInput('Distribution', 'Statystyka', choices=c("Attack","HP","Defense", "Sp.Atk","Sp.Def","Speed","Total")),
tags$div(
  HTML("
<strong>Total</strong>: suma statystyk<br>
<strong>HP</strong>: punkty ¿ywotnoœci<br>
<strong>Attack</strong>: zwyk³e ataki<br>
<strong>Defense</strong>: odpornoœæ na zwyk³e ataki<br>
<strong>SP Atk</strong>: specjalne umiejêtnoœci<br>
<strong>SP Def</strong>: odpornoœæ na specjalne umiejêtnoœci<br>
<strong>Speed</strong>: priotytet kolejnoœci ruchu podczas rundy<br>
")
)

  	),
mainPanel(
	tabsetPanel(type = "tabs", 
                    tabPanel("Histogram", plotOutput("chisq")),
                   tabPanel("Legendary*Generation N°", plotOutput("chisq22222"))
        )


      )
    ),
tabPanel("Inne wykresy",
			plotOutput('plot123',dblclick = "plot_dblclick",brush = "plot_brush"),
			h5("Double click:"),
			verbatimTextOutput("info"),
			h5("Brush:"),
			verbatimTextOutput("info2"),
			hr(),
			br(),
			plotOutput('plotTotalType1'),
			plotOutput('plotType1Legendary'))
)








server <- function(input, output) {
#####################################################################################
### TAB .1
  selectedData <- reactive({
    dataA[, c(input$xcol, input$ycol)]
  })

    output$plot1 <- renderPlot({
 par(mar = c(5.1, 4.1, 5, 1))
    plot(selectedData(), pch = 1, cex = 3, col=dataA$Type.1, main="")
	title("Twój wykres", line = 3,cex.main = 2)

	legend(x="top",inset=c(0,-0.12),legend=unique(dataA$Type.1),col=1:length(dataA$Type.1),xpd = TRUE,pch=1,horiz=T)
 })


  




######################################################################################
### TAB .2

  selectedData2 <- reactive({
   dataA[,input$yy1 ]

  })

  selectedData3 <- reactive({
	dataA[,input$yy2 ]

  })


  output$unif <- renderPlot({
     par(mar = c(5.1, 4.1, 2, 1))

genType <- input$Generacja
if(genType == 1 | genType == 2 | genType == 3 |genType == 4 |genType == 5 |genType == 6 ){
#plot(selectedData2()+selectedData3() ~Total,  type="p",  main="",xlab="Total", ylab="Y1 + Y2",col=c("red","green","blue"),cex=1.5)
plot(selectedData2()[Generation==input$Generacja]+selectedData3()[Generation==input$Generacja] ~Total[Generation==input$Generacja],  type="p",  main="Y1+Y2 vs Total",xlab="Total", ylab="Y1 + Y2",col=c("red","green","blue"),cex=1.5)
} else plot(selectedData2()+selectedData3() ~Total,  type="p",  main="Y1+Y2 vs Total",xlab="Total", ylab="Y1 + Y2",col=c("red","green","blue"),cex=1.5)

#plot(selectedData2()[c(Generation==input$Generacja),]+selectedData3()[c(Generation==input$Generacja),] ~Total[c(Generation==input$Generacja),],  type="p",  main="Title",ylab="Y1_var + Y2_var",col=c(1,2,3),cex=1.5)
legend(x="bottomright",legend=c('Y1','Y2','Total','Pokemon'),col=c("red","green","blue","black"),pch=c(1,1,1,3),cex=1.1)

points(selectedData2()[Name==input$idName] +  selectedData3()[Name==input$idName] ~Total[Name==input$idName],pch=3,lwd=2, cex=5)
    })



output$stats <- renderPrint({
head(dataA[order(selectedData2()+selectedData3(),decreasing=T),])
})



output$selectedStats <- renderPrint({

dataA[Name==input$idName,c(3,4,5,6,7,8,9,10,11,12)]

})

########################################################################################
### TAB .3

output$chisq <- renderPlot({
distType <- input$Distribution
if(distType == "Attack"){
    hist(Attack, col = 8, border = "white", prob=TRUE, las=1,
       main = "Attack")
	lines(density(Attack)) 
}
if(distType == "Total"){
    hist(Total, col = 2, border = "white", prob=TRUE, las=1,
       main = "Total")
	lines(density(Total)) 
}
if(distType == "HP"){
    hist(HP, col = 3, border = "white", prob=TRUE, las=1,
       main = "HP",ylim=c(0,0.02))
	lines(density(HP)) 
}
if(distType == "Defense"){
    hist(Defense, col = "brown", border = "white", prob=TRUE, las=1,
       main = "Defense")
	lines(density(Defense)) 
}
if(distType == "Sp.Atk"){
    hist(Sp..Atk, col = 5, border = "white", prob=TRUE, las=1,
       main = "Special Attack",ylim=c(0,0.015))
	lines(density(Sp..Atk)) 
}
if(distType == "Sp.Def"){
    hist(Sp..Def, col = 6, border = "white", prob=TRUE, las=1,
       main = "Special Defense")
	lines(density(Sp..Def)) 
}
if(distType == "Speed"){
    hist(Speed, col = 7, border = "white", prob=TRUE, las=1,
       main = "Speed")
	lines(density(Speed)) 
}
  })



output$chisq22222 <- renderPlot({
dataA$Generation = factor(dataA$Generation)
	distType <- input$Distribution
		if(distType == "Attack"){	

		histogram(~Attack|dataA$Generation*Legendary,dataA=dataA)
		} else {
		if(distType == "HP"){histogram(~HP|dataA$Generation*Legendary,dataA=dataA)
		} else {
		if(distType == "Defense"){histogram(~Defense|dataA$Generation*Legendary,dataA=dataA)
		} else {
		if(distType == "Sp.Atk"){histogram(~Sp..Atk|dataA$Generation*Legendary,dataA=dataA)
		} else {
		if(distType == "Sp.Def"){histogram(~Sp..Def|dataA$Generation*Legendary,dataA=dataA)
		}else {
		if(distType == "Speed"){histogram(~Speed|dataA$Generation*Legendary,dataA=dataA)
		}else {
		if(distType == "Total"){histogram(~Total|dataA$Generation*Legendary,dataA=dataA)
		}
		}
		}
		}
		}
	}


	}
})

########################################################################################
### TAB .4


  output$info <- renderPrint({
    nearPoints(dataA, input$plot_dblclick, threshold = 10, maxpoints = 1)
  })
  output$info2 <- renderPrint({
    brushedPoints(dataA, input$plot_brush)
  })

  output$plot123 <- renderPlot({
	p <- ggplot(dataA, aes(Total, Attack))
	p + geom_point(size=2,(aes(colour = Type.1))) + labs(title= "Attack vs Total") + theme(plot.title = element_text(lineheight=.8,hjust = 0.5, face="bold", size = rel(2)))
 })



     output$plotTotalType1 <- renderPlot({
	par(mar=c(7.1, 4.1, 4.1, 2.1))
  	plot( Total~Type.1, col=c("yellow3","burlywood4","mediumslateblue","orange","pink1","red3","chocolate1","plum3","plum4","green1","tan1","turquoise","navajowhite4",
		"maroon","deeppink","darkgoldenrod3","darkgray","cornflowerblue"), las=1, main="Wartoœci Total dla poszczególnych typów", cex.main=1.5,xlab="Types", ylab= "Total",cex.lab=1.5,
         cex = 3)
	title("", sub="Typ Dragon osi¹ga najlepsze œrednie wartoœci statystyk ogó³em.",cex.sub = 1.2,line=4.8, font.sub = 3, col.sub = "black")
    })


	output$plotType1Legendary <- renderPlot({
	par(mar=c(7.1, 4.1, 4.1, 2.1))
	tt <- table(Type.1, Legendary)
	bb<-barplot(tt, main="Iloœæ pokemonów poszczególnych typów z podzia³em na legendarne",cex.main=1.5, xlab="Legendary", ylim=c(0,120), ylab="Amount", col=c("yellow3","burlywood4","mediumslateblue","orange","pink1","red3","chocolate1","plum3","plum4","green1","tan1","turquoise","navajowhite4",
		"maroon","deeppink","darkgoldenrod3","darkgray","cornflowerblue"),beside=T,cex.lab=1.5,
	legend = rownames(tt),args.legend=list(x = "top", horiz=TRUE))
	text(bb, 4, tt, cex=1.5)
	title("", sub="Na wysok¹ wartoœæ Total typu Dragon wp³ywa iloœæ legendarnych Pokemonów tego typu. ",cex.sub = 1.2,line=4.8, font.sub = 3, col.sub = "black")
    
	box()
})

}
########################################################################################


shinyApp(server = server, ui = ui)