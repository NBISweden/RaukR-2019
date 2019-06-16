## NAMETAGGER
## R shinyapp to generate nametags
## 2019 Roy Mathew Francis

library(shiny)
library(shinyAce)
library(png)
library(ggplot2)

# validation
fn_validate <- function(input,message1,message2,message3)
{

  if(missing(message1)) message1 <- "Input is missing."
  gcheck <- length(grep("Argument \\'\\w+\\' missing",message1))
  if(gcheck == 1)
  {
    m1 <- sub("Argument ","",message1)
    m1 <- sub(" missing.","",m1)
  }

  if (all(is.null(input))) {
    if(missing(message1)) message1 <- "Input is missing."
    print(message1)
  } else if (is.numeric(input) | is.list(input)) {
    if(all(is.na(input)))
    {
      if(missing(message2))
      {
        if(gcheck==1) message2 <- paste0("Argument ",m1," is NA.",sep="")
        if(gcheck!=1) message2 <- "Input is NA."
      }
      print(message2)
    }
  } else if (is.character(input)) {
    if(all(nchar(input) == 0))
    {
      if(missing(message3))
      {
        if(gcheck==1) message3 <- paste0("Argument ",m1," is empty.",sep="")
        if(gcheck!=1) message3 <- "Input is empty."
      }
      print(message3)
    }
  } else {
    NULL
  }
}

# nametag_plot_page ------------------------------------------------------------

#' @title nametag_plot_page
#' @description Creates a page with 1-8 nametags.
#' @param dfr A data.frame. See details.
#' @param logo_right A raster logo to be placed on the right.
#' @param logo_right_scale A scale value. Typically 0.1-0.4.
#' @param logo_right_offset Logo offset from the edge. Around 0.01-0.1.
#' @param height Height of nametag in cm. Defaults to 5.5.
#' @param width Width of nametag in cm. Defaults to 9.
#' @details Argument 'dfr' is a data.frame that must have columns
#' label1, label1_x, label1_y, page, row and col.
#'
nametag_plot_page <- function(dfr,logo_right=NULL,logo_right_scale,logo_right_offset,height=5.5,width=9)
{
  if(missing(dfr)) stop("Input argument 'dfr' missing.")

  # check columns
  req_cols <- c("label1","label1_sz","label1_x","label1_y","page","row","col")
  chk_cols <- req_cols %in% colnames(dfr)
  if(any(!chk_cols)) stop(paste0("Input data is missing columns: ",paste0(req_cols[!chk_cols],collapse=","),"."))

  # add labels if any has length >0
  p <- ggplot(dfr)
  if(any(nchar(dfr$label1)!=0)) p <- p + geom_text(aes(x=label1_x,y=label1_y,label=label1),size=dfr$label1_sz[1],fontface="bold")

  if("label2" %in% colnames(dfr)) {
    req_cols <- c("label2_sz","label2_x","label2_y")
    chk_cols <- req_cols %in% colnames(dfr)
    if(any(!chk_cols)) stop(paste0("Input data contains column 'label2' but missing columns: ",paste0(req_cols[!chk_cols],collapse=","),"."))

    if(any(nchar(dfr$label2)!=0)) p <- p + geom_text(aes(x=label2_x,y=label2_y,label=label2),size=dfr$label2_sz[1])
  }

  if("label3" %in% colnames(dfr)) {
    req_cols <- c("label3_sz","label3_x","label3_y")
    chk_cols <- req_cols %in% colnames(dfr)
    if(any(!chk_cols)) stop(paste0("Input data contains column 'label2' but missing columns: ",paste0(req_cols[!chk_cols],collapse=","),"."))

    if(any(nchar(dfr$label3)!=0)) p <- p + geom_text(aes(x=label3_x,y=label3_y,label=label3),size=dfr$label3_sz[1])
  }

  p <- p + scale_x_continuous(limits=c(0,1),expand=c(0,0))+
    scale_y_continuous(limits=c(0,1),expand=c(0,0))+
    labs(x=NULL,y=NULL)

  w_scaler <- width/height

  # check and add right logo
  if(!is.null(logo_right)) {

    # height scaling multiplier 1.6
    logo_right_height <- ((logo_right_scale*nrow(logo_right))/ncol(logo_right))*w_scaler

    # create logo positions
    logo_right_x2 <- 1-logo_right_offset
    logo_right_x1 <- logo_right_x2-logo_right_scale
    logo_right_y2 <- 1-(logo_right_offset+(logo_right_offset*w_scaler))
    logo_right_y1 <- round(logo_right_y2-logo_right_height,3)

    # add right logo to plot
    p <- p + annotation_raster(logo_right,xmin=logo_right_x1,xmax=logo_right_x2,ymin=logo_right_y1,ymax=logo_right_y2)
  }
  
  p <- p+
    facet_grid(row~col)+
    theme(axis.text=element_blank(),
          axis.title=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0,"lines"),
          strip.background=element_blank(),
          strip.text=element_blank(),
          axis.ticks=element_blank(),
          panel.background=element_rect(colour="grey75",fill="white",size=0.4,linetype="25"),
          plot.background=element_blank(),
          plot.margin=margin(0.1,0.1,0.1,0.1),
          axis.ticks.length=unit(0,"pt"))

  return(p)
}

# nametag ----------------------------------------------------------------------

#' @title nametag
#' @description Creates an A4 paper with 1-8 name tags.
#' @param dfr A data.frame with column 'label1'. Optionally 'label2' and 'label3'.
#' @param label1_sz Size of label on line 1.
#' @param label1_x X-axis coordinate for the label on line 1.
#' @param label1_y Y-axis coordinate for the label on line 1.
#' @param label2_sz Size of label on line 2.
#' @param label2_x X-axis coordinate for the label on line 2.
#' @param label2_y Y-axis coordinate for the label on line 2.
#' @param label3_sz Size of label on line 3.
#' @param label3_x X-axis coordinate for the label on line 3.
#' @param label3_y Y-axis coordinate for the label on line 3.
#' @param logo_right A raster logo for the right side.
#' @param logo_right_offset Logo offset from the edge. Around 0.01-0.1.
#' @param logo_right_scale A scale value. Typically 0.1-0.4.
#' @param filename A character denoting filename (prefix) of exported files.Defaults to 'nametag_' followed by page number and '.png'.
#' @param path A character path to the directory where file(s) are to be exported. Do not add / at the end of the path.
#' @param ftype Export file type. 'png' or 'pdf'. Defaults to 'png'.
#' @param height Height of nametag in cm. Defaults to 5.5.
#' @param width Width of nametag in cm. Defaults to 9.
#' @details A data.frame with one column 'label1' is the only mandatory input for this function.
#'
nametag <- function(dfr,label1_sz=8,label1_x=0.5,label1_y=0.54,
                    label2_sz=6.5,label2_x=0.5,label2_y=0.37,
                    label3_sz=6,label3_x=0.5,label3_y=0.22,
                    logo_right=NULL,logo_right_offset=0.04,logo_right_scale=0.2,
                    filename="nametag_",path=".",ftype="png",height=5.5,width=9)
{
  if(missing(dfr)) stop("Input argument 'dfr' is missing.")
  if(!is.data.frame(dfr)) stop("Input argument 'dfr' must be a data.frame.")
  if(nrow(dfr)<1) stop("Input data must have at least 1 row.")
  if(!("label1"  %in% colnames(dfr))) stop("Input data must contain a column named 'label1'.")
  if("label3"  %in% colnames(dfr)) {if(!("label2"  %in% colnames(dfr))) stop("Column 'label3' is present, but 'label2' is missing. If 'label3' is used, 'label2' must be present." )}
  if(is.null(filename)) filename <- "nametag_"

  # compute tags and pages
  n <- nrow(dfr)
  npages <- ceiling(n/8)
  ntags <- npages*8
  filler <- ntags-n
  l1 <- c(dfr$label1,rep("",filler))
  if("label2"  %in% colnames(dfr)) l2 <- c(dfr$label2,rep("",filler))
  if("label3"  %in% colnames(dfr)) l3 <- c(dfr$label3,rep("",filler))
  nn <- length(l1)

  # create working df
  dfw <- data.frame(label1=l1,label1_sz=label1_sz,
                    label1_x=rep(label1_x,nn),label1_y=rep(label1_y,nn),
                    stringsAsFactors=F)

  if("label2"  %in% colnames(dfr)) {
    dfw$label2 <- l2
    dfw$label2_sz <- label2_sz
    dfw$label2_x <- rep(label2_x,nn)
    dfw$label2_y <- rep(label2_y,nn)
  }

  if("label3"  %in% colnames(dfr)) {
    dfw$label3 <- l3
    dfw$label3_sz <- label3_sz
    dfw$label3_x <- rep(label3_x,nn)
    dfw$label3_y <- rep(label3_y,nn)
  }

  dfw$page <- rep(1:npages,each=8)
  dfw$row <- rep(rep(1:4,each=2),npages)
  dfw$col <- rep(rep(c(1,2),4),npages)

  dflist <- split(dfw,dfw$page)
  ids <- names(dflist)

  # creates plots and saves ggplot objects to a list
  plist <- lapply(dflist,nametag_plot_page,logo_right,logo_right_scale,logo_right_offset,height=height,width=width)

  # function to export images
  efun <- function(p,id,height,width,filename,ftype) {
    if(ftype=="png") {
      fname <- paste0(path,"/",filename,id,".png")
      ggsave(filename=fname,plot=p,height=height*4,width=width*2,units="cm",dpi=300,device="png")
    }
    if(ftype=="pdf") {
      fname <- paste0(path,"/",filename,id,".pdf")
      ggsave(filename=fname,plot=p,height=height*4*0.3937,width=width*2*0.3937,device="pdf")
    }
  }

  # exports images
  mapply(function(p,id,height,width,filename,ftype) efun(p,id,height,width,filename,ftype),plist,ids,height,width,filename,ftype)
}


# UI ---------------------------------------------------------------------------


ui <- fluidPage(
  fixedRow(
      column(12,style="margin:15px;",
    h1("Nametagger"),
    fixedRow(
    column(3,style="max-width:300px;background:#ebedef;padding-top:15px;padding-bottom:15px;border-radius:4px;",
      fluidRow(
        column(6,style=list("padding-right:5px;"),
               selectInput("in_input","Input method",choices=c("Upload file","Paste text"),
                           selected="Paste text",multiple=FALSE)),
        column(6,style=list("padding-left: 5px;"),
               selectInput("in_data_format","File format",choices=c("tsv","csv","csv2"),selected="csv",multiple=FALSE))
        ),
      uiOutput("ui_input"),
      helpText("Input must contain column names. 'label1' is a mandatory column. Optional columns are 'label2' and 'label3'.",style="display:inline;"),
      selectInput("in_logo_right",label="Logo",c("None","NBIS Green","NBIS Blue","NBIS Orange","SciLifeLab Green","SciLifeLab Blue","SciLifeLab Orange","Elixir"),selected="None",multiple=FALSE),
      downloadButton("btn_download","Download"),
      checkboxInput("in_settings","Settings",value=FALSE),
      tags$hr(),
      helpText("2019 | NBIS")
    ),
    column(6,style="max-width:450px;min-width:400px;padding-top:15px;padding-bottom:15px;border-radius:4px;",
      textOutput("out_pagecount"),
      tags$br(),
      imageOutput("out_plot")
    ),
    uiOutput("ui_settings")
    )
  )
)
)

# SERVER -----------------------------------------------------------------------

server <- function(input, output, session) {

  ## UI: ui_input --------------------------------------------------------------
  ## ui to select input type: upload or paste
  output$ui_input <- renderUI({
    validate(fn_validate(input$in_input))

    if(input$in_input=="Upload file") {
      fileInput("in_data","Upload a text file.",multiple=FALSE)
    }else{
      shinyAce::aceEditor("in_data","label1,label2\nJohn Doe,Uppsala University\nMary Jane,Stockholm University",mode="text",theme="textmate",readOnly=FALSE,height="150px",fontSize=12)
    }
  })

  ## UI: ui_settings -----------------------------------------------------------
  ## ui to display settings
  output$ui_settings <- renderUI({
    validate(fn_validate(input$in_settings))

    if(input$in_settings) {
      column(3,style="max-width:300px;border-radius:4px;background:#ebedef;",
        h4("Settings"),
        div(
          tags$b("Label 1"),
          fluidRow(
            column(4,style=list("padding-right: 3px;"),
                   numericInput("in_label1_size",label="Size",value=8,min=4,max=12,step=0.5)
            ),
            column(4,style=list("padding-right: 3px; padding-left: 3px;"),
                   numericInput("in_label1_x",label="X pos",value=0.5,min=0,max=1,step=0.02)
            ),
            column(4,style=list("padding-left: 3px;"),
                   numericInput("in_label1_y",label="Y pos",value=0.54,min=0,max=1,step=0.02)
            )
          ),
          tags$b("Label 2"),
          fluidRow(
            column(4,style=list("padding-right: 3px;"),
                   numericInput("in_label2_size",label="Size",value=6.5,min=4,max=12,step=0.5)
            ),
            column(4,style=list("padding-right: 3px; padding-left: 3px;"),
                   numericInput("in_label2_x",label="X pos",value=0.5,min=0,max=1,step=0.02)
            ),
            column(4,style=list("padding-left: 3px;"),
                   numericInput("in_label2_y",label="Y pos",value=0.37,min=0,max=1,step=0.02)
            )
          ),
          tags$b("Label 3"),
          fluidRow(
            column(4,style=list("padding-right: 3px;"),
                   numericInput("in_label3_size",label="Size",value=6,min=4,max=12,step=0.5)
            ),
            column(4,style=list("padding-right: 3px; padding-left: 3px;"),
                   numericInput("in_label3_x",label="X pos",value=0.5,min=0,max=1,step=0.02)
            ),
            column(4,style=list("padding-left: 3px;"),
                   numericInput("in_label3_y",label="Y pos",value=0.22,min=0,max=1,step=0.02)
            )
          ),
          tags$b("Logo"),
          fluidRow(
            column(6,style=list("padding-right: 3px;"),
                   numericInput("in_logo_right_offset",label="Offset",value=0.02,min=0,max=0.2,step=0.01)
            ),
            column(6,style=list("padding-left: 3px;"),
                   numericInput("in_logo_right_scale",label="Width",value=0.2,min=0.1,max=0.6,step=0.01)
            )
          )
        )
      )
    }

  })

  ## FN: fn_input --------------------------------------------------------
  ## function to get input data

  fn_input <- reactive({
    validate(fn_validate(input$in_input))
    validate(fn_validate(try(input$in_data),message1="Upload a file or paste text."))
    validate(fn_validate(input$in_data_format))
    fr <- ifelse(input$in_data_format=="tsv","\t",ifelse(input$in_data_format=="csv",",",";"))

    if(input$in_input=="Upload file") {
      dfr <- read.delim(input$in_data$datapath,header=TRUE,sep=fr,stringsAsFactors=F)
    }
    if(input$in_input=="Paste text") {
      df1 <- as.data.frame(strsplit(as.character(unlist(strsplit(input$in_data,"\n"))),fr))
      cnames <- as.character(df1[,1])
      df1[,1] <- NULL
      dfr <- as.data.frame(t(df1),stringsAsFactors=F)
      colnames(dfr) <- cnames
      rownames(dfr) <- 1:nrow(dfr)
    }

    colnames(dfr) <- tolower(colnames(dfr))
    return(dfr)
  })

  ## FN: fn_params ------------------------------------------------------------
  ## function to get plot params

  fn_params <- reactive({

    validate(fn_validate(fn_input()))
    validate(fn_validate(input$in_settings))
    validate(fn_validate(input$in_logo_right))

    if("label1" %in% colnames(fn_input())) {l1 <- TRUE}else{l1 <- FALSE}
    if("label2" %in% colnames(fn_input())) {l2 <- TRUE}else{l2 <- FALSE}
    if("label3" %in% colnames(fn_input())) {l3 <- TRUE}else{l3 <- FALSE}

    # if values are available, use them, else use defaults
    if(is.null(input$in_label1_size)){l1s <- 8}else{l1s <- input$in_label1_size}
    if(is.null(input$in_label1_x)){l1x <- 0.5}else{l1x <- input$in_label1_x}
    if(is.null(input$in_label1_y)){l1y <- 0.54}else{l1y <- input$in_label1_y}
    if(is.null(input$in_label2_size)){l2s <- 6.5}else{l2s <- input$in_label2_size}
    if(is.null(input$in_label2_x)){l2x <- 0.5}else{l2x <- input$in_label2_x}
    if(is.null(input$in_label2_y)){l2y <- 0.37}else{l2y <- input$in_label2_y}
    if(is.null(input$in_label3_size)){l3s <- 6}else{l3s <- input$in_label3_size}
    if(is.null(input$in_label3_x)){l3x <- 0.5}else{l3x <- input$in_label3_x}
    if(is.null(input$in_label3_y)){l3y <- 0.22}else{l3y <- input$in_label3_y}
    if(is.null(input$in_logo_right_offset)){lro <- 0.04}else{lro <- input$in_logo_right_offset}
    if(is.null(input$in_logo_right_scale)){lrs <- 0.2}else{lrs <- input$in_logo_right_scale}

    # logos
    lr = switch(
      input$in_logo_right,
      "None"=NULL,
      "NBIS Green"="./www/nbis_200_green.png",
      "NBIS Blue"="./www/nbis_200_blue.png",
      "NBIS Orange"="./www/nbis_200_orange.png",
      "SciLifeLab Green"="./www/scilifelab_200_green.png",
      "SciLifeLab Blue"="./www/scilifelab_200_blue.png",
      "SciLifeLab Orange"="./www/scilifelab_200_orange.png",
      "Elixir"="./www/elixir_200.png"
    )
    if(!is.null(lr)) {lri <- readPNG(lr)}else{lri <- NULL}

    return(list(l1s=l1s,l1x=l1x,l1y=l1y,l2s=l2s,l2x=l2x,l2y=l2y,l3s=l3s,l3x=l3x,l3y=l3y,
                lro=lro,lrs=lrs,lri=lri))
  })

  ## OUT: out_plot ------------------------------------------------------------
  ## plots figure

  output$out_plot <- renderImage({

    validate(fn_validate(fn_input()))
    validate(fn_validate(fn_params()))
    p <- fn_params()

    nametag(dfr=fn_input(),label1_sz=p$l1s,label1_x=p$l1x,label1_y=p$l1y,
            label2_sz=p$l2s,label2_x=p$l2x,label2_y=p$l2y,
            label3_sz=p$l3s,label3_x=p$l3x,label3_y=p$l3y,
            logo_right=p$lri,logo_right_offset=p$lro,logo_right_scale=p$lrs)

    return(list(src="nametag_1.png",contentType="image/png",
                width=round(9*2*37.7*0.6,0),
                height=round(5.5*4*37.7*0.6,0),
                alt="nametagger_image"))
  })

  ## OUT: out_pagecount -------------------------------------------------------
  ## prints general variables for debugging

  output$out_pagecount <- renderText({

    req(fn_input())

    npages <- ceiling(nrow(fn_input())/8)
    paste0("Showing 1 of ",npages," pages.")
  })

  ## FN: fn_download -----------------------------------------------------------
  ## function to download a zipped file with images

  fn_download <- function(){

    validate(fn_validate(fn_input()))
    validate(fn_validate(fn_params()))

    p <- fn_params()
    nametag(dfr=fn_input(),label1_sz=p$l1s,label1_x=p$l1x,label1_y=p$l1y,
            label2_sz=p$l2s,label2_x=p$l2x,label2_y=p$l2y,
            label3_sz=p$l3s,label3_x=p$l3x,label3_y=p$l3y,
            logo_right=p$lri,logo_right_offset=p$lro,logo_right_scale=p$lrs,
            path=".")

    unlink("nametagger.zip")
    zip("nametagger.zip",files=list.files()[grep("nametag_[0-9]+[.png$|.pdf$]",list.files())])
    unlink(list.files()[grep("nametag_[0-9]+[.png$|.pdf$]",list.files())])
  }

  ## DHL: btn_download ---------------------------------------------------------
  ## download handler for downloading zipped file

  output$btn_download <- downloadHandler(
    filename="nametagger.zip",
    content=function(file) {
      fn_download()
      file.copy("nametagger.zip",file,overwrite=T)
    }
  )
}

shinyApp(ui=ui, server=server)
