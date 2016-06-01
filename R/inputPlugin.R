#' Input Plugin of Fuzzy Clustering on Rcmdr
#'
#' @description Graphical User Interface on Rcmdr Plugin.
#' This Plugin provide Interface to select variables of dataset that will be
#' used for Fuzzy Clustering, methods selection, and parameter specification
#' @export
pluginInput<-function(){
  require(tcltk2)
  require(tcltk)
  require(Rcmdr)
  #--------------------------#
  #  DATASET                 #
  #--------------------------#

  dataset <- ActiveDataSet()
  doItAndPrint(paste("data<-data.frame(",dataset,")"))
  var <- colnames(data)
  var.choice<-c()
  #--------------------------#
  #  INTERFACE               #
  #--------------------------#
  #--WINDOWS-----------------#
  win1 <- tktoplevel(width=750)
  tktitle(win1) <- "Fuzzy Clustering: Input GUI"
  #--FRAME DATA OPTIONS------#
  fontTitle<- tkfont.create(family = "Gentium Book Basic", size = 15,
                               weight = "bold", underline = T)
  fontCommands<- tkfont.create(family = "Gentium Basic", size = 10,weight="bold")
  tkgrid(
    tk2label(
      win1, text = "FUZZY CLUSTERING", justify = "right",font=fontTitle
    ),
    padx = 15, pady = c(5, 5),row=0,column=0
  )

  win1$frame1<-tk2frame(win1,borderwidth=2,relief="flat")
  tkgrid(win1$frame1,padx=0,pady=c(0,0),row=1,column=0)
  win1$var$list <-
    tk2listbox(
      win1$frame1,width = 35,height = 6,selectmode = "multiple"
    )
  tkgrid(
    tk2label(
      win1$frame1, text = "Select Variables:\n(Min. 2 Variables)", justify = "left",
      font=fontCommands
    ),
    padx = 15, pady = c(5, 5),sticky="w",row=0,column=0
  )
  tkgrid(win1$var$list, padx = 15, pady = c(0, 15),row=1,column=0,rowspan=2)
  for (i in var)
    tkinsert(win1$var$list,"end",i)
  tkselection.set(win1$var$list, 0)
  win1$rButton<- tk2button(win1$frame1, text = ">>",width=5)
  tkgrid(win1$rButton, padx = 5, pady = c(0,5),sticky="s",column=1,row=1)
  win1$lButton<- tk2button(win1$frame1, text = "<<",width=5)
  tkgrid(win1$lButton, padx = 5, pady = c(0,5),sticky="n",column=1,row=2)

  tkgrid(
    tk2label(
      win1$frame1, text = "Cluster Variables: \n(Min. 2 Variables)", justify = "left",
      font=fontCommands
    ),
    padx = 15, pady = c(5, 5),sticky="w",row=0,column=2
  )

  win1$var$cluster <-
    tk2listbox(
      win1$frame1,width = 35,height = 6,selectmode = "multiple"
    )
  tkgrid(win1$var$cluster, padx = 15, pady = c(0, 15),row=1,column=2,rowspan=2)

  onRight<-function(){
    tkdelete(win1$var$cluster,0,"end")
    var.choice<<- var[as.numeric(tkcurselection(win1$var$list))+1]
    for (i in var.choice)
      tkinsert(win1$var$cluster,"end",i)
    tkselection.set(win1$var$cluster, 0)
  }
  onLeft<-function(){
    var.remo.choice<<- var.choice[as.numeric(tkcurselection(win1$var$cluster))+1]
    tkdelete(win1$var$cluster,0,"end")
    var.choice<<-var.choice[-which(var.choice%in%var.remo.choice)]
    for (i in var.choice)
      tkinsert(win1$var$cluster,"end",i)
    tkselection.set(win1$var$cluster, 0)
    }
  tkconfigure(win1$rButton,command=onRight)
  tkconfigure(win1$lButton,command=onLeft)

  #--METHODS    OPTIONS------#
  win1$frame2<-tk2frame(win1,borderwidth=2,relief="flat")
  tkgrid(win1$frame2,padx=0,pady=c(0,0),row=2,column=0,sticky="w")
  fuzz.method <- c("Fuzzy C-Means","Gustafson Kessel")
  win1$method <-
    tk2combobox(win1$frame2,width = 15,values = fuzz.method)
  nCluster <- tk2spinbox(win1$frame2,width = 3,value = c(2:nrow(data)))
  fuzzifier.param <-
    tk2spinbox(
      win1$frame2,width = 5,value = seq(from = 1,to = 3, by = 0.1)
    )
  more.param<-c(1000,10^-13,0)
  more.Dialog <- function(parent,returnValOnCancel = c(1000,10^-13,0)) {
    dlg <- tktoplevel()
    tktitle(dlg)<-"More Parameter"
    tkwm.deiconify(dlg)
    tkgrab.set(dlg)
    tkfocus(dlg)
    returnVal<-returnValOnCancel

    MaxIterVal <- tclVar(returnVal[1])
    MaxIterEntry <- tk2entry(dlg, width = 8,
                             textvariable = MaxIterVal)
    tkgrid(tklabel(dlg, text = "Max iteration:",
                   font=fontCommands), MaxIterEntry, padx = 10, pady = 5,sticky="w")

    ThresholdVal <- tclVar(returnVal[2])
    ThresholdEntry <- tk2entry(dlg, width = 8,
                               textvariable = ThresholdVal)
    tkgrid(tklabel(dlg, text = "Threshold:",
                   font=fontCommands), ThresholdEntry, padx = 10, pady = 5,sticky="w")

    SeedVal <- tclVar(returnVal[3])
    SeedEntry <- tk2entry(dlg, width = 8,
                          textvariable = SeedVal)
    tkgrid(tklabel(dlg, text = "Seed:",
                   font=fontCommands), SeedEntry, padx = 10, pady = 5,sticky="w")

    onOK <- function() {
      returnVal[1] <<- tclvalue(MaxIterVal)
      returnVal[2] <<- tclvalue(ThresholdVal)
      returnVal[3] <<- tclvalue(SeedVal)
      tkgrab.release(dlg)
      tkdestroy(dlg)
      tkfocus(parent)
    }
    onCancel <- function() {
      returnVal <<- returnValOnCancel
      tkgrab.release(dlg)
      tkdestroy(dlg)
      tkfocus(parent)
    }
    butOK <- tk2button(dlg, text = "OK", width = -6, command = onOK)
    butCancel <- tk2button(dlg, text = "Cancel", width = -6, command = onCancel)
    tkgrid(butCancel, butOK, padx = 10, pady = c(0, 15))
    tkfocus(dlg)
    tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg); tkfocus(parent)})
    tkbind(MaxIterEntry, "<Return>", onOK)
    tkbind(ThresholdEntry, "<Return>", onOK)
    tkbind(SeedEntry, "<Return>", onOK)
    tkwait.window(dlg)
    returnVal
  }

  win1$moreDialog <- function() {
    more.param<<- more.Dialog(win1)
    print(more.param)
  }
  win1$moreButton<- tk2button(win1$frame2, text = "More...", width=9,
                              command = win1$moreDialog)

  tkgrid(
    tk2label(
      win1$frame2, text = "Method:", justify = "left",
      font=fontCommands
    ),
    win1$method,
    tk2label(
      win1$frame2, text = "N Cluster:", justify = "left",
      font=fontCommands
    ),
    nCluster,
    tk2label(
      win1$frame2, text = "Fuzzifier:", justify = "left",
      font=fontCommands
    ),
    fuzzifier.param,win1$moreButton,
    padx = 10,pady = c(5,5),sticky="w"
  )
  method <- tclVar("Fuzzy C-Means")
  tkconfigure(win1$method, textvariable = method)
  method.nCluster <- tclVar("2")
  tkconfigure(nCluster, textvariable = method.nCluster)
  method.fuzzifier <- tclVar("1")
  tkconfigure(fuzzifier.param, textvariable = method.fuzzifier)
  on.next<-function(){
    pb <- winProgressBar("Fuzzy Clustering Progress", "Clustering....",
                         0, 100, 0)
    Sys.sleep(0.5)
    setWinProgressBar(pb, 0, "Fuzzy Clustering Progress", "Prepare Data")
    cat("Prepare data\n")
    data.cluster<-managedata(var.choice)
    Sys.sleep(0.5)
    setWinProgressBar(pb, 20, "Fuzzy Clustering Progress", "Data OK")
    cat("Data for Clustering is OK\n")
    cat(paste("Method Used: ",tclvalue(method),"\n"))
    cat(paste("N cluster: ",tclvalue(method.nCluster),"\n"))
    cat(paste("Fuzzifier: ",tclvalue(method.fuzzifier),"\n"))
    cat(paste("Max Iteration: ",more.param[1],"\n"))
    cat(paste("Threshold: ",more.param[2],"\n"))
    typeof(tclvalue(method.nCluster))
    if(more.param[3]==0)
      cat(paste("NO Specific Seed"))
    else
      cat(paste("Seed: ",more.param[3],"\n"))
    Sys.sleep(1)
    setWinProgressBar(pb, 40, "Fuzzy Clustering Progress", "Clustering Process")
    data.cluster<-scale(data.cluster)
    if(tclvalue(method)=="Fuzzy C-Means")
    cluster<-fuzzy.CM(data.cluster,as.numeric(tclvalue(method.nCluster)),
                        as.numeric(tclvalue(method.fuzzifier)),
                        as.numeric(more.param[1]),
                        as.numeric(more.param[2]),
                        as.numeric(more.param[3]))
    else
    cluster<-fuzzy.GK(data.cluster,as.numeric(tclvalue(method.nCluster)),
                        as.numeric(tclvalue(method.fuzzifier)),
                        as.numeric(more.param[1]),
                        as.numeric(more.param[2]),
                        as.numeric(more.param[3]))
    Sys.sleep(1)
    setWinProgressBar(pb, 60, "Fuzzy Clustering Progress", "Clustering Validation")

    valid<-validation.index(cluster)
    Sys.sleep(1)
    setWinProgressBar(pb, 80, "Fuzzy Clustering Progress", "Clustering MANOVA")

    manov<-checkManova(cluster)
    Sys.sleep(1)
    setWinProgressBar(pb, 100, "Fuzzy Clustering Progress", "FINISH")
    Sys.sleep(1)
    close(pb)
    result.GUI(win1,cluster,valid,manov,tclvalue(method))
  }

  win1$frame3<-tk2frame(win1,borderwidth=2,relief="flat")
  tkgrid(win1$frame3,padx=25,pady=c(0,0),row=3,column=0)
  tkgrid(
    tk2button(win1$frame3, text = "Close",
              command =function(){
                tkdestroy(win1)
              }),
    tk2button(win1$frame3, text = "Next",command=on.next),
    padx = 5, pady = c(0,15))
  tk2theme("vista")
  tcl("ttk::style", "configure", "TFrame", background="white")
  tcl("ttk::style", "configure", "TLabel", background="white")
  tcl("ttk::style", "configure", "TButton",font=fontCommands)
  tcl("ttk::style", "map", "TButton", background=c("active", "blue"))

  tkfocus(win1)

}
