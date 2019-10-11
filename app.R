##########
#R CODE CA COUNTY PYRAMIDS
#
#EDDIE HUNSINGER, OCTOBER 2019
#https://edyhsgr.github.io/eddieh/
#
#IF YOU WOULD LIKE TO USE, SHARE OR REPRODUCE THIS CODE, BE SURE TO CITE THE SOURCE
#
#EXAMPLE DATA IS LINKED, SO YOU SHOULD BE ABLE TO SIMPLY COPY ALL AND PASTE INTO R
#
#THERE IS NO WARRANTY FOR THIS CODE
#THIS CODE HAS NOT BEEN TESTED AT ALL-- PLEASE LET ME KNOW IF YOU FIND ANY PROBLEMS (edyhsgr@gmail.com)
##########

library(shiny)
ui<-fluidPage(

	tags$h3("California State and County Population Pyramid Viewer"),
	p("California county population estimates and projections by age and sex for 1970 to 2050. Data developed by the California Department of Finance, and accessed via ",
	tags$a(href="https://data.ca.gov/dataset/california-population-projection-by-county-age-gender-and-ethnicity", "data.ca.gov")),
  
hr(),

sidebarLayout(
sidebarPanel(

 selectInput("Area", "Area",
c(
"California"="6",
"Alameda"="6001",
"Alpine"="6003",
"Amador"="6005",
"Butte"="6007",
"Calaveras"="6009",
"Colusa"="6011",
"Contra Costa"="6013",
"Del Norte"="6015",
"El Dorado"="6017",
"Fresno"="6019",
"Glenn"="6021",
"Humboldt"="6023",
"Imperial"="6025",
"Inyo"="6027",
"Kern"="6029",
"Kings"="6031",
"Lake"="6033",
"Lassen"="6035",
"Los Angeles"="6037",
"Madera"="6039",
"Marin"="6041",
"Mariposa"="6043",
"Mendocino"="6045",
"Merced"="6047",
"Modoc"="6049",
"Mono"="6051",
"Monterey"="6053",
"Napa"="6055",
"Nevada"="6057",
"Orange"="6059",
"Placer"="6061",
"Plumas"="6063",
"Riverside"="6065",
"Sacramento"="6067",
"San Benito"="6069",
"San Bernardino"="6071",
"San Diego"="6073",
"San Francisco"="6075",
"San Joaquin"="6077",
"San Luis Obispo"="6079",
"San Mateo"="6081",
"Santa Barbara"="6083",
"Santa Clara"="6085",
"Santa Cruz"="6087",
"Shasta"="6089",
"Sierra"="6091",
"Siskiyou"="6093",
"Solano"="6095",
"Sonoma"="6097",
"Stanislaus"="6099",
"Sutter"="6101",
"Tehama"="6103",
"Trinity"="6105",
"Tulare"="6107",
"Tuolumne"="6109",
"Ventura"="6111",
"Yolo"="6113",
"Yuba"="6115"
),
),

numericInput("YEAR_1","Year for fill",2010,1970,2050,step=1),

numericInput("YEAR_2","Year for outline",2000,1970,2050,step=1),

hr(),

selectInput("SetXAxes", "Set x-axes manually?",
c(
"No"="NO",
"Yes"="YES"
),
),

numericInput("XAxesMax","If yes, x-axes maximum",500000,0,1000000,step=1000),

hr(),

p("This interface was made with ",
tags$a(href="https://shiny.rstudio.com/", 
	"Shiny for R."),
tags$a(href="https://github.com/edyhsgr/CACountyPyramids", 
	"Related GitHub repository."),
"October 2019."),

width=3
),

mainPanel(
	
	plotOutput("plots")
))
)

choicevec<-c(
"California"="6",
"Alameda"="6001",
"Alpine"="6003",
"Amador"="6005",
"Butte"="6007",
"Calaveras"="6009",
"Colusa"="6011",
"Contra Costa"="6013",
"Del Norte"="6015",
"El Dorado"="6017",
"Fresno"="6019",
"Glenn"="6021",
"Humboldt"="6023",
"Imperial"="6025",
"Inyo"="6027",
"Kern"="6029",
"Kings"="6031",
"Lake"="6033",
"Lassen"="6035",
"Los Angeles"="6037",
"Madera"="6039",
"Marin"="6041",
"Mariposa"="6043",
"Mendocino"="6045",
"Merced"="6047",
"Modoc"="6049",
"Mono"="6051",
"Monterey"="6053",
"Napa"="6055",
"Nevada"="6057",
"Orange"="6059",
"Placer"="6061",
"Plumas"="6063",
"Riverside"="6065",
"Sacramento"="6067",
"San Benito"="6069",
"San Bernardino"="6071",
"San Diego"="6073",
"San Francisco"="6075",
"San Joaquin"="6077",
"San Luis Obispo"="6079",
"San Mateo"="6081",
"Santa Barbara"="6083",
"Santa Clara"="6085",
"Santa Cruz"="6087",
"Shasta"="6089",
"Sierra"="6091",
"Siskiyou"="6093",
"Solano"="6095",
"Sonoma"="6097",
"Stanislaus"="6099",
"Sutter"="6101",
"Tehama"="6103",
"Trinity"="6105",
"Tulare"="6107",
"Tuolumne"="6109",
"Ventura"="6111",
"Yolo"="6113",
"Yuba"="6115"
)

server<-function(input, output) {	
	output$plots<-renderPlot({
par(mfrow=c(1,2), mai=c(2,.575,.5,0.05))

#####
#####
##GRAPHING
#####
#####
Kx<-data.frame(read.table(file="https://raw.githubusercontent.com/edyhsgr/CACountyPyramids/master/population-estimates-and-projections-by-county-age-and-sex-california-1970-2050.csv",header=TRUE,sep=","))

cat<-read.table(file="https://raw.githubusercontent.com/edyhsgr/CACountyPyramids/master/agelabels.csv",sep=",",header=TRUE)
cat<-array(cat$x[1:101])

options(scipen = 999)

if(input$Area!=6){
select<-subset(Kx, Kx$fips==input$Area & Kx$year==input$YEAR_1)
male<-select$pop_male
if(input$SetXAxes=="NO") {barplot(male,horiz=T,names=cat,las=2,axes=FALSE,xlim=c(max(male)*1.5,0),col=rgb(0,.9,.6,1),border=rgb(0,.9,.6,1))}
if(input$SetXAxes=="YES") {barplot(male,horiz=T,names=cat,las=2,axes=FALSE,xlim=c(input$XAxesMax,0),col=rgb(0,.9,.6,1),border=rgb(0,.9,.6,1))}
par(new=TRUE)
select2<-subset(Kx, Kx$fips==input$Area & Kx$year==input$YEAR_2)
male2<-select2$pop_male
Placement1<-max(male)*1.5
Placement2<-input$XAxesMax
if(input$SetXAxes=="NO") {barplot(male2,horiz=T,names=F,cex.names=.8,space=0,las=2,axes=FALSE,xlim=c(Placement1,0),col=rgb(0,0,0,0))}
if(input$SetXAxes=="YES") {barplot(male2,horiz=T,names=F,cex.names=.8,space=0,las=2,axes=FALSE,xlim=c(Placement2,0),col=rgb(0,0,0,0))}
mtext(side=1,line=5,adj=.75,text=expression("Male"),font=1,cex=1.5)
axis(side=1,cex.axis=1.1,las=2)
}

if(input$Area==6){
male<-aggregate(Kx$pop_male, by=list(Kx$age,Kx$year), FUN=sum)
select<-subset(male, male$Group.2==input$YEAR_1)
male<-select$x
if(input$SetXAxes=="NO") {barplot(male,horiz=T,names=cat,las=2,axes=FALSE,xlim=c(max(male)*1.5,0),col=rgb(0,.9,.6,1),border=rgb(0,.9,.6,1))}
if(input$SetXAxes=="YES") {barplot(male,horiz=T,names=cat,las=2,axes=FALSE,xlim=c(input$XAxesMax,0),col=rgb(0,.9,.6,1),border=rgb(0,.9,.6,1))}
par(new=TRUE)
male2<-aggregate(Kx$pop_male, by=list(Kx$age,Kx$year), FUN=sum)
select2<-subset(male2, male2$Group.2==input$YEAR_2)
male2<-select2$x
Placement1<-max(male)*1.5
Placement2<-input$XAxesMax
if(input$SetXAxes=="NO") {barplot(male2,horiz=T,names=F,cex.names=.8,space=0,las=2,axes=FALSE,xlim=c(Placement1,0),col=rgb(0,0,0,0))}
if(input$SetXAxes=="YES") {barplot(male2,horiz=T,names=F,cex.names=.8,space=0,las=2,axes=FALSE,xlim=c(Placement2,0),col=rgb(0,0,0,0))}
mtext(side=1,line=5,adj=.75,text=expression("Male"),font=1,cex=1.5)
axis(side=1,cex.axis=1.1,las=2)
}

mtext(side=1,line=-44,adj=-.1,text="Age",font=1,cex=1)
if(input$Area!=6){mtext(side=1,line=-45,at=3,text=paste(c("Population by Age and Sex, ", names(choicevec[choicevec == input$Area]), " County"),collapse=""),font=1,cex=1.75)}
if(input$Area==6){mtext(side=1,line=-45,at=3,text=paste(c("Population by Age and Sex, ", names(choicevec[choicevec == input$Area])),collapse=""),font=1,cex=1.75)}
mtext(side=1,line=8,adj=0,text=paste(c("Source: California Department of Finance, August 2019. Accessed via data.ca.gov."),collapse=""),font=1,cex=1)

if(input$SetXAxes=="NO") {legend(Placement1*.85, 100, legend=c(input$YEAR_1,input$YEAR_2), col=c(rgb(0,.9,.6,1),rgb(0,1,1,0)), pt.cex=2, pch=15, cex=1.5, bty ="n", y.intersp=1.25)}
if(input$SetXAxes=="NO") {legend(Placement1*.85, 100, legend=c("",""), col=c(rgb(0,.9,.6,1), rgb(0,0,0)), pt.cex=2, pch=0, cex=1.5, bty ="n", y.intersp=1.25)}

if(input$SetXAxes=="YES") {legend(Placement2*.85, 100, legend=c(input$YEAR_1,input$YEAR_2), col=c(rgb(0,.9,.6,1),rgb(0,1,1,0)), pt.cex=2, pch=15, cex=1.5, bty ="n", y.intersp=1.25)}
if(input$SetXAxes=="YES") {legend(Placement2*.85, 100, legend=c("",""), col=c(rgb(0,.9,.6,1), rgb(0,0,0)), pt.cex=2, pch=0, cex=1.5, bty ="n", y.intersp=1.25)}

if(input$Area!=6){
select3<-subset(Kx, Kx$fips==input$Area & Kx$year==input$YEAR_1)
female<-select3$pop_female
if(input$SetXAxes=="NO") {barplot(female,horiz=T,names=F,cex.names=.8,space=0,las=2,axes=FALSE,xlim=c(0,max(male)*1.5),col=rgb(0,.9,.6,1),border=NA)}
if(input$SetXAxes=="YES") {barplot(female,horiz=T,names=F,cex.names=.8,space=0,las=2,axes=FALSE,xlim=c(0,input$XAxesMax),col=rgb(0,.9,.6,1),border=NA)}
par(new=TRUE)
select4<-subset(Kx, Kx$fips==input$Area & Kx$year==input$YEAR_2)
female2<-select4$pop_female
if(input$SetXAxes=="NO") {barplot(female2,horiz=T,names=F,cex.names=.8,space=0,las=2,axes=FALSE,xlim=c(0,max(male)*1.5),col=rgb(0,0,0,0))}
if(input$SetXAxes=="YES") {barplot(female2,horiz=T,names=F,cex.names=.8,space=0,las=2,axes=FALSE,xlim=c(0,input$XAxesMax),col=rgb(0,0,0,0))}
mtext(side=1,line=5,adj=.25,text=expression("Female"),font=1,cex=1.5)
axis(side=1,cex.axis=1.1,las=2)
}

if(input$Area==6){
female<-aggregate(Kx$pop_female, by=list(Kx$age,Kx$year), FUN=sum)
select3<-subset(female, female$Group.2==input$YEAR_1)
female<-select$x
if(input$SetXAxes=="NO") {barplot(female,horiz=T,names=F,cex.names=.8,space=0,las=2,axes=FALSE,xlim=c(0,max(male)*1.5),col=rgb(0,.9,.6,1),border=NA)}
if(input$SetXAxes=="YES") {barplot(female,horiz=T,names=F,cex.names=.8,space=0,las=2,axes=FALSE,xlim=c(0,input$XAxesMax),col=rgb(0,.9,.6,1),border=NA)}
par(new=TRUE)
female2<-aggregate(Kx$pop_female, by=list(Kx$age,Kx$year), FUN=sum)
select4<-subset(female2, female2$Group.2==input$YEAR_2)
female2<-select2$x
if(input$SetXAxes=="NO") {barplot(female2,horiz=T,names=F,cex.names=.8,space=0,las=2,axes=FALSE,xlim=c(0,max(male)*1.5),col=rgb(0,0,0,0))}
if(input$SetXAxes=="YES") {barplot(female2,horiz=T,names=F,cex.names=.8,space=0,las=2,axes=FALSE,xlim=c(0,input$XAxesMax),col=rgb(0,0,0,0))}
mtext(side=1,line=5,adj=.25,text=expression("Female"),font=1,cex=1.5)
axis(side=1,cex.axis=1.1,las=2)
}

#####

},height=800,width=800)
		
}

shinyApp(ui = ui, server = server)
