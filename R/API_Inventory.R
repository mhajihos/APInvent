#Inventory Project

API_Inventory=function()
{

Pack=c("shiny","shinythemes","gridExtra","ggplot2","ggpubr","shinyalert","shinydashboard","shinydashboardPlus",
	"shinyWidgets","plyr","dplyr","readxl","reshape2","data.table","openxlsx")

suppressPackageStartupMessages(lapply(Pack, require, character.only = TRUE))

## ui ##
ui <- dashboardPage(
	dashboardHeader(title="The Inventory ",titleWidth =200),
	#collapsed = TRUE,
	  dashboardSidebar(width=280,
		sidebarMenu(tags$style(HTML(".main-sidebar { font-size: 16px; color: white;}")),
				menuItem(HTML("Data Entry and <br/> Descriptive Statistics"), tabName = "Data"),
				
				fileInput(inputId = "file", label = "Please input CSV File", accept='.xlsx'),

					downloadButton('download1', HTML('<FONT size="3pt">Download Table</FONT></FONT><br>'),class = "butt1"),
							tags$head(tags$style(".butt1{background-color:white;} .butt1{color: black;}")),

				radioButtons("ItemNo",
				label = HTML('<FONT size="3pt">Item Number:</FONT></FONT><br>'),
                         choices = "Updating",
               			inline = T,
               			width = "100%"),


				radioButtons("BatchNo",
				label = HTML('<FONT size="3pt">Batch Number:</FONT></FONT><br>'),
                         choices = "Updating",
               			inline = T,
               			width = "100%"),

				
				radioButtons("RecDate",
				label = HTML('<FONT size="3pt">Received Date:</FONT></FONT><br>'),
                         choices = "Updating",
               			inline = T,
               			width = "100%"),
	
				radioButtons("ExpDate",
				label = HTML('<FONT size="3pt">Expiration Date:</FONT></FONT><br>'),
                         choices = "Updating",
               			inline = T,
               			width = "100%")

				)),

		dashboardBody(
				tags$head(tags$style(HTML('
                                .content-wrapper, .right-side {
                                background-color: #FFFFFF;
                                }'))),

			tabItem(tabName = "Data",
				box(id="box1",title = "Add Items",background = "light-blue",solidHeader = TRUE, collapsible = TRUE,
					textInput('RecDaten', 'Enter Received Date YY-MM-DD',value ="0"),
					numericInput('Numn', 'Enter Item_No', 0),
					numericInput('Bacthn', 'Enter Bacth_No', 0),
					numericInput('QIn', 'Enter QI_Quantity', 0),
					numericInput('PRn', 'Enter Provisional_Release', 0),
					numericInput('CRn', 'Enter Conditional_Release', 0),
					numericInput('Rn', 'Enter Released', 0)),


			tabItem(tabName = "DataNew",
				box(id="box2",title = "Add Items",background = "light-blue",solidHeader = TRUE, collapsible = TRUE,
					
					numericInput('Qn', 'Enter Quarantine', 0),
					numericInput('RDn', 'Enter Reject_Destroy', 0),
					numericInput('RRn', 'Enter Reject_Return', 0),
					numericInput('Dn', 'Enter Destroyed', 0),
					numericInput('RVn', 'Enter Returned_Vendor', 0),
					numericInput('SHn', 'Enter Shipped_Quantity', 0),
					textInput('Exn', 'Enter Expiration Date YY-MM-DD',value ="0"),

					actionButton("do", "Add"),
					downloadButton('download2', 'Download New Data')),	


				),
		
		mainPanel(
		tags$style(type="text/css",
      	".shiny-output-error { visibility: hidden; }",
      	".shiny-output-error:before { visibility: hidden; }"
    		),
 		tableOutput("table"),
 		plotOutput("plot1", width="1000px",height="800px")))

))





#Server
server=function(input, output, session) 
{

options(shiny.maxRequestSize=30*1024^2)
my_data <- reactiveVal(NULL)

# Read the input file and fix the names

observeEvent(input$file, {

if (!is.null(input$file)){
    	data=read_excel(input$file$datapath)
	data=data.frame(data)

if("Received.Date" %in% names(data))
{
data2= data %>% select(Received_Date=Received.Date,Item_No=API.Item.No.,Batch=Batch...Lot,QI=Received.in.QI,Provisional_Release=Provisional.Release,
				Conditional_Release=Conditional.Release,Released=Released,Quarantine=Quarantine,
				Reject_Destroy=Reject...Destroy,Reject_Return=Reject...Return,Destroyed=Destroyed,
				Returned_Vendor=Returned.to.Vendor,Shipped=Shipped,Expiration_Date=Expiration.Date)
}else if(!("Received.Date" %in% names(data)))
	{data2=data}

data2$Received_Date=as.character(data2$Received_Date)
data2$Expiration_Date=as.character(data2$Expiration_Date)
data2=data.frame(data2)
}

my_data(data2)
})


observeEvent(input$do,{
if(input$Numn!="0"){
       newrow = data.table(Received_Date=input$RecDaten,Item_No=input$Numn,Batch=input$Bacthn,QI=input$QIn,Provisional_Release=input$PRn,
 			Conditional_Release=input$CRn,Released=input$Rn,Quarantine=input$Qn,
 			Reject_Destroy=input$RDn,Reject_Return=input$RRn,Destroyed=input$Dn,
 			Returned_Vendor=input$RVn,Shipped=input$SHn,Expiration_Date=input$Exn)
newrow =data.frame(newrow)

my_data(rbind(my_data(),newrow))
}else{my_data()}

})


observe({
data=my_data()
data=data.frame(data)
			Options=unique(data$Item_No)
	updateRadioButtons(session,"ItemNo",choices=Options)

})



observeEvent(input$ItemNo,{
data=my_data()
data=data.frame(data)


			Options2=c(unique(data$Batch[data$Item_No==input$ItemNo]),"All")
			updateRadioButtons(session,"BatchNo",choices=Options2)

})


observeEvent(input$BatchNo,{
data=my_data()
data=data.frame(data)

if(input$BatchNo=="All")
{

			Options3=c(unique(data$Received_Date[data$Item_No==input$ItemNo]),"All")
}else if(input$BatchNo!="All"){

				Options3=c(unique(data$Received_Date[data$Item_No==input$ItemNo & data$Batch==input$BatchNo]),"All")
	
					}
					updateRadioButtons(session,"RecDate",choices=Options3)
})


observeEvent(input$BatchNo,{
data=my_data()
data=data.frame(data)



if(input$BatchNo=="All")
{

			Options4=c(unique(data$Expiration_Date[data$Item_No==input$ItemNo]),"All")
}else if(input$BatchNo!="All"){
			
			Options4=c(unique(data$Expiration_Date[data$Item_No==input$ItemNo & data$Batch==input$BatchNo]),"All")
}
			updateRadioButtons(session,"ExpDate",choices=Options4)
})


#TABLE DESCRIPTIVE
TableInput <- reactive({
data=my_data()
data=data.frame(data)


if(input$BatchNo=="All" & input$RecDate=="All" & input$ExpDate=="All")
	{
			data2=data[data$Item_No==input$ItemNo,]
}else if(input$BatchNo=="All" & input$RecDate!="All" & input$ExpDate=="All")
	{
			data2=data[(data$Received_Date==input$RecDate & data$Item_No==input$ItemNo),]
}else if(input$BatchNo=="All" & input$RecDate!="All" & input$ExpDate!="All")
	{	
			data2=data[(data$Expiration_Date==input$ExpDate & data$Received_Date==input$RecDate & data$Item_No==input$ItemNo),]
}else if(input$BatchNo=="All" & input$RecDate=="All" & input$ExpDate!="All")
	{
			data2=data[(data$Expiration_Date==input$ExpDate & data$Item_No==input$ItemNo),]
}else if(input$BatchNo!="All" & input$RecDate=="All" & input$ExpDate=="All")
	{
			data2=data[(data$Item_No==input$ItemNo & data$Batch==input$BatchNo),]

			Tot=apply(data2[4:13],2,sum)
			Total=c(paste0("Total for Batch ",input$BatchNo),input$ItemNo,input$BatchNo,Tot,paste0("NA"))
			data2=rbind(data2,Total)		

}else if(input$BatchNo!="All" & input$RecDate!="All" & input$ExpDate=="All")
	{
			data2=data[(data$Received_Date==input$RecDate & data$Item_No==input$ItemNo & data$Batch==input$BatchNo),]
			
			Tot=apply(data2[4:13],2,sum)
			Total=c(paste0("Total for Batch ",input$BatchNo),input$ItemNo,input$BatchNo,Tot,paste0("NA"))
			data2=rbind(data2,Total)

}else if(input$BatchNo!="All" & input$RecDate=="All" & input$ExpDate!="All")
	{
			data2=data[(data$Expiration_Date==input$ExpDate & data$Item_No==input$ItemNo & data$Batch==input$BatchNo),]

			Tot=apply(data2[4:13],2,sum)
			Total=c(paste0("Total for Batch ",input$BatchNo),input$ItemNo,input$BatchNo,Tot,paste0("NA"))
			data2=rbind(data2,Total)

}else{
			data2=data[(data$Expiration_Date==input$ExpDate & data$Received_Date==input$RecDate & data$Item_No==input$ItemNo & data$Batch==input$BatchNo),]

			Tot=apply(data2[4:13],2,sum)
			Total=c(paste0("Total for Batch ",input$BatchNo),input$ItemNo,input$BatchNo,Tot,paste0("NA"))
			data2=rbind(data2,Total)
	}

return(data2)

})

#For output
output$table <- renderTable({
print(TableInput())
},
bordered = TRUE)



#Downloads
output$download1<-downloadHandler(

filename ="Inventory_Descriptive_Table.csv",

content = function(con) {
inFile=input$file
	  setwd("~/")
 	  file.create("report1.csv")
        tempTemplate <<- file.path(getwd(), "report1.csv")
        file.copy("Inventory_Report.csv", tempTemplate, overwrite = TRUE)
	   
write.csv(TableInput(), con, row.names = FALSE)

})


output$download2<-downloadHandler(

filename ="Inventory_Updated_Data.xlsx",

content = function(con) {
inFile=input$file
	  setwd("~/")
 	  file.create("report2.xlsx")
        tempTemplate <<- file.path(getwd(), "report2.xlsx")
        file.copy("Inventory_Updated_Data.xlsx", tempTemplate, overwrite = TRUE)
	   
write.xlsx(my_data(), con, rowNames = FALSE)

})

}




shinyApp(ui = ui, server = server)

}


API_Inventory()


