library(tidyverse)
library(stringr)
library(xml2)
library(shiny)
library(DT)
library(broom)
library(plotly)
data<-read_xml("Desktop/VernierFiles/white light.qmbl")


#find all data columns and the cell contents
data %>%
	xml_find_all("//ColumnCells")%>%
	xml_text() %>%
	str_replace_all("\\n",",") %>%
	str_split(",") %>%
	map(as.numeric) %>%
	set_names(~str_c("val",seq_along(.)))%>%
	as_data_frame()
	#map_dfc(as_data_frame) %>%
	View()
	tail(-1)%>%
	head(-1)%>%
	set_names(~str_c("val",seq_along(.)))%>%



### number of runs/data sets in file		
data %>%
	xml_find_all("//DataSetName")%>%
	xml_text()->names

## axes
	data %>%
		xml_find_all("//DataSet")%>%
		map(~xml_find_all(.,".//DataObjectName")) %>%
		map(~xml_text(.)) %>%
		setNames(names) %>%
		map(~bind_cols(x=.[[1]],
									 y=.[[2]])) %>%
		enframe() %>%
		unnest()->axes

## axis units		
data %>%
	xml_find_all("//DataSet") %>%
	map(~xml_find_all(.,".//ColumnUnits")) %>%
	map(~xml_text(.)) %>%
	setNames(names) %>%
	map(~bind_cols(x=.[[1]],
								 y=.[[2]])) %>%
	enframe() %>%
	unnest()->units

## axis labels
full_join(axes,units,by="name") %>%
	mutate(x_axis=str_c(x.x," (",x.y,")")) %>%
	mutate(y_axis=str_c(y.x," (",y.y,")")) %>%
	select(name,x_axis,y_axis) %>%
	filter(name=="Run 1")->axis_labels

	
data %>%
	xml_find_all("//DataSet") %>%
	map(~xml_find_all(.,".//ColumnCells"))%>%
	map(~xml_text(.)) %>%
	map(~str_replace_all(.,"\\n",",")) %>%
	map(~str_split(.,","))%>%
	set_names(names)%>%
	map(~bind_cols(x=.[[1]],
								 y=.[[2]])) %>%
	enframe() %>%
	filter(name=="Run 1") %>%
	unnest() %>%
	mutate(x=as.numeric(x),
				 y=as.numeric(y))%>%
	filter(!is.na(x)) %>%

	ggplot(aes(x=x,y=y))+
	geom_point()+
	geom_line()+
	geom_smooth(method = "lm",se=F,color="red")
	
	data %>%
		xml_find_all("//DataSet") %>%
		map(~xml_find_all(.,".//ColumnCells"))%>%
		map(~xml_text(.)) %>%
		map(~str_replace_all(.,"\\n",",")) %>%
		map(~str_split(.,","))%>%
		set_names(names)%>%
		map(~bind_cols(x=.[[1]],
									 y=.[[2]])) %>%
		enframe() %>%
		filter(name=="Run 1") %>%
		unnest() %>%
		mutate(x=as.numeric(x),
					 y=as.numeric(y))%>%
		filter(!is.na(x))	%>%
		lm(formula=x~y,data = .)%>%
		pluck(.,"coefficients")->x
		
	str_c("slope= ",round(x[2],digits = 2))
	str_c("y-interecept= ",round(x[1],digits = 2))
	
	

 tibble(x=sample(1:10,10,T),
			 y=sample(1:10,10,T),
			 g=c(rep("in",10),rep("out",0)))->x

x %>%
	filter(x==6 & y==8) %>%
	setdiff(x,.)


%>%
	ggplot(aes(x=x,y=y,color=g))+
	geom_point()+
	geom_smooth()


#### SHINY APP ####
ui<-fluidPage(
	titlePanel("Upload Vernier File"),
	sidebarLayout(
		sidebarPanel(
			fileInput('file1', 'Choose file to upload',
								accept = c(
									'text/plain'
								)
			),
			tags$hr(),
			uiOutput("run_num"),
			tags$hr(),
			checkboxInput("reg","Add Linear Regression?"),
			uiOutput("reg",),
			tags$hr(),
			downloadButton("dl", "Download Graph"),
			tags$hr()
			
		),
		mainPanel(
			plotlyOutput('plot'),
			#dataTableOutput('contents'),
			tags$hr(),
			verbatimTextOutput("zoom"),
			verbatimTextOutput("click"),
			verbatimTextOutput("click2")
			
		)
	)
)	

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 9MB.
options(shiny.maxRequestSize = 9*1024^2)

server<-function(input, output, session) {
	
	inFile <- reactive({
		inFile <- input$file1
		if (is.null(inFile)) {
			# User has not uploaded a file yet
			return(NULL)
		}
		read_xml(inFile$datapath)
	})
	
	rv<-reactiveValues(i=1,j=1)
	
	output$run_num<-renderUI({
		df<-inFile()
		if (is.null(df)) return(NULL)
		rv$names<-df %>%
			xml_find_all("//DataSetName")%>%
			xml_text()
		selectInput("run_num","Run Number",rv$names)
	})

	
	##subset data
	subsetdata  <- reactive({
		df <- inFile()
		if (is.null(df)) return(NULL)
		
		df %>%
			xml_find_all("//DataSet") %>%
			map(~xml_find_all(.,".//ColumnCells"))%>%
			map(~xml_text(.)) %>%
			map(~str_replace_all(.,"\\n",",")) %>%
			map(~str_split(.,","))%>%
			set_names(rv$names)%>%
			map(~bind_cols(x=.[[1]],
										 y=.[[2]])) %>%
			enframe() %>%
			filter(name==input$run_num) %>%
			unnest() %>%
			mutate(x=as.numeric(x),
						 y=as.numeric(y))%>%
			filter(!is.na(x)) %>%
			select(x,y)
	})
	
	observe({
		df<-inFile()
		if (is.null(df)) return(NULL)
		## axes
		df %>%
			xml_find_all("//DataSet")%>%
			map(~xml_find_all(.,".//DataObjectName")) %>%
			map(~xml_text(.)) %>%
			setNames(rv$names) %>%
			map(~bind_cols(x=.[[1]],
										 y=.[[2]])) %>%
			enframe() %>%
			unnest()->rv$axes
		
		## axis units		
		df %>%
			xml_find_all("//DataSet") %>%
			map(~xml_find_all(.,".//ColumnUnits")) %>%
			map(~xml_text(.)) %>%
			setNames(rv$names) %>%
			map(~bind_cols(x=.[[1]],
										 y=.[[2]])) %>%
			enframe() %>%
			unnest()->rv$units
		
		## axis labels
		full_join(rv$axes,rv$units,by="name") %>%
			mutate(x_axis=str_c("\n",x.x," (",x.y,")","\n")) %>%
			mutate(y_axis=str_c("\n",y.x," (",y.y,")","\n")) %>%
			select(name,x_axis,y_axis)->rv$axis_labels
		
	})
	
	
	output$contents <- renderDataTable({
		# input$file1 will be NULL initially. After the user selects
		# and uploads a file, it will be a data frame with 'name',
		# 'size', 'type', and 'datapath' columns. The 'datapath'
		# column will contain the local filenames where the data can
		# be found.
		subsetdata()
	})
	
	observeEvent(event_data("plotly_click"),{
		rv$d <- event_data("plotly_click")
		if (rv$i>=1){
			rv$d %>%
				select(x,y) %>%
				bind_rows(rv$discard,.)->rv$discard
		}
		else {
			rv$d %>%
				select(x,y)->rv$discard
		}
		rv$i<-rv$i+1
	})
	
	observeEvent(event_data("plotly_selected"),{
		rv$e <- event_data("plotly_selected")
		if (rv$i>=1){
			rv$e %>%
				select(x,y) %>%
				bind_rows(rv$discard,.)->rv$discard
		}
		else {
			rv$e %>%
				select(x,y)->rv$discard
		}
		rv$i<-rv$i+1
	})
	
	
	#### RENDER PLOT ####	
	output$plot <-renderPlotly({
		
		if(is.null(subsetdata())) return()

		if(is.null(rv$discard)){
			rv$keep<-subsetdata()
			p <-ggplot() +
				geom_line(data=rv$keep,aes(x=x,y=y),size=0.5,color="grey50")+
				geom_point(data=rv$keep,aes(x=x,y=y),color="black")
		}
		
		else{
			subsetdata() %>% 
				setdiff(.,rv$discard)->rv$keep
			p <-ggplot() +
				geom_line(data=rv$keep,aes(x=x,y=y),size=0.5,color="grey50")+
				geom_point(data=rv$keep,aes(x=x,y=y),color="black")+
				geom_point(data=rv$discard,aes(x=x,y=y),color="black",shape=21,alpha=1/10)
				
			p$elementId <- NULL
		}
		
		if (input$reg){
			p <- p + geom_smooth(data = rv$keep,
													 aes(x=x,y=y),
													 method="lm",
													 color="grey",
													 se=F,
													 size=0.5)
		}
		# gg<-event_data("plotly_relayout")
		# if (!is.null(gg)) {
		# 
		# p<- p + xlim(c(gg[[1]],gg[[2]])) +
		# 				ylim(c(gg[[3]],gg[[4]]))
		# }
		
		rv$axis_labels %>%
			filter(name==input$run_num) ->axis
		
		p <- p + xlab(axis$x_axis) +
						ylab(axis$y_axis) +
						theme(axis.title = element_text(size=8),
									axis.text = element_text(size=8))
		rv$p<-p
		
		ggplotly(p)%>%
			layout(margin=list(l=60,pad=0))
	})
	
	#### LINEAR REGRESSION ####	
	
	output$reg<-renderUI({
		if(!input$reg) return (NULL)
		rv$keep %>%
			lm(formula=x~y,data = .)%>%
			pluck(.,"coefficients")->x
		
		return(str_c("Model: ","y = ",round(x[2],digits = 1),"x + "
								 ,round(x[1],digits = 1)))
		
	})
	
	output$click <- renderPrint({
		if (is.null(rv$units)) "units" 
		else rv$units
		
	})
	
	output$click2 <- renderPrint({
		if (is.null(rv$axes)) "axes" 
		else rv$axes
		
	})
	
	output$zoom <- renderPrint({
		if (is.null(rv$axis_labels)) "combined axis labels" 
		else rv$axis_labels
		
	})
	
	output$dl <- downloadHandler(
		filename = "plot.png",
		content = function(file) {
			ggsave(file,plot=rv$p,device="png")
		}
	)
	
}

#### RUN ####
shinyApp(ui = ui, server = server)
	