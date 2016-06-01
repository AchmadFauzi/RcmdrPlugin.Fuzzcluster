#' @export
result.GUI <- function(parent,cluster,valid,manov,method) {
  require(tcltk2)
  require(tcltk)
  require(tkrplot)
  require(ggplot2)
  result <- tktoplevel(background = "white")
  tktitle(result) <- "Result"
  fontTitle <-
    tkfont.create(
      family = "Gentium Book Basic", size = 15,
      weight = "bold", underline = T
    )
  fontCommand <- tkfont.create(family = "Gentium Basic", size = 10,
                               weight = "bold")
  fontLabel <- tkfont.create(
    family = "Gentium Basic", size = 10,
    weight = "bold",slant = "italic"
  )
  resultlabel <-
    paste("RESULT OF CLUSTER ANALYSIS\n\"",toupper(method),"\"")
  tkgrid(
    tk2label(
      result,text = resultlabel,justify = "center",background = "white",font =
        fontTitle
    ),row = 0,column = 0,pady = c(5,5),padx = 5
  )

  results <-
    tk2notebook(result,tabs = c("Cluster Plot","Cluster Description"))
  tkgrid(results,padx = 10)
  onOk <- function()
    tkdestroy(result)
  tkgrid(tk2button(result, text = "OK",
                   command = onOk),pady = c(8,3))
  #----------------------------------------------------#
  #   Tab. 1 Plot Cluster and Labeling                 #
  #----------------------------------------------------#
  result.tab <- tk2notetab(results,"Cluster Plot")
  result.tab.panel <-
    tk2frame(result.tab,borderwidth = 2,relief = "flat")
  plot.label <-
    tk2label(
      result.tab.panel,text = "Plot",justify =
        "left",font = fontLabel
    )
  labeling.label <-
    tk2label(
      result.tab.panel,text = "Label",justify = "left"
      ,font = fontLabel
    )
  tkgrid(result.tab.panel)
  tkgrid(
    plot.label,row = 0,column = 0,pady = c(5,0),padx = 2
  )
  tkgrid(
    labeling.label,row = 0,column = 25,pady = c(5,0),padx = 2
  )
  ####-> Prepare PCA Data and Plotting
  pp <- ncol(cluster$Clust.desc)
  data.clu <- cluster$Clust.desc[,1:pp - 1]
  data.PCA <- prcomp(data.clu,scale. = T)
  z1 <- as.data.frame(cbind(data.PCA$x[,1:2],cluster$Clust.desc[,pp]))
  windowsFonts(A=windowsFont("Gentium Basic"))
  ploting <- function() {
    datapc <- data.frame(varnames=rownames(data.PCA$rotation),
                         data.PCA$rotation)
    mult <- min(
      (max(data[,"PC2"]) - min(data[,"PC2"])/(max(datapc[,"PC2"])-min(datapc[,"PC2"]))),
      (max(data[,"PC1"]) - min(data[,"PC1"])/(max(datapc[,"PC1"])-min(datapc[,"PC1"])))
    )
    datapc <- transform(datapc,
                        v1 = .7 * mult * (get("PC1")),
                        v2 = .7 * mult * (get("PC2"))
    )

    ggplot(z1,aes(x = PC1,y = PC2,color=factor(V3))) +
      geom_point() +stat_ellipse()+
      xlab(paste("PC 1 \nVariance Explained: ",
                 round(summary(data.PCA)$importance[2,1] *100,2),"%")) +
      ylab(paste("PC 2 \nVariance Explained: ",
                 round(summary(data.PCA)$importance[2,2] *100,2),"%")) +
      theme_bw(base_size = 10,base_family = "A") + coord_equal()+
      geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 3, vjust=1, color="red") +
      geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="red")-> pl
    eval(substitute(print(pl)))
  }
  clusplot <- tkrplot(
    result.tab.panel, fun = ploting ,
    hscale = 1.5, vscale = 1.25
  )
  tkgrid(
    clusplot,row = 1,column = 0,sticky = "w",padx = 2,pady =
      c(0,5)
  )

  ####-> Labeling
  tclTable.Label <- tclArray()

  mat.L <-
    c("Cluster",cluster$Clust.desc[,ncol(cluster$Clust.desc)])
  mat.L <- as.matrix(mat.L)
  mat.L <- cbind(c("Obs",paste("",1:nrow(
    cluster$Clust.desc
  ))),mat.L)

  for (i in 1:nrow(mat.L))
    for (j in 1:ncol(mat.L))
      tclTable.Label[[i - 1, j - 1]] <- mat.L[i, j]
  cluslabel <-
    tk2table(
      result.tab.panel, variable = tclTable.Label,titlerows = 1,titlecols = 1,
      width = 10,height = 25,rows = nrow(mat.L),cols = ncol(mat.L),
      selectmode = "extended", colwidth = 12, background = "white",
      yscrollcommand = function(...)
        tkset(Lyscr,...)
    )
  Lyscr <- tk2scrollbar(
    result.tab.panel, orient = "vertical",
    command = function(...)
      tkyview(cluslabel, ...)
  )

  tkgrid(
    cluslabel,Lyscr,row = 1,column = 25,pady = c(0,5),padx = 5
  )
  tkgrid.configure(Lyscr, sticky = "nse")
  tkgrid.rowconfigure(cluslabel, 0, weight = 1)
  tkgrid.columnconfigure(cluslabel, 0, weight = 1)
  tkconfigure(cluslabel,state = "disable")


  #----------------------------------------------------#
  #        Tab. 2 Fuzzy Membership, Centroid,          #
  #          MANOVA, and Validation Index              #
  #----------------------------------------------------#

  desc.tab <- tk2notetab(results,"Cluster Description")
  desc.tab.pan1 <-
    tk2frame(desc.tab,borderwidth = 2,relief = "flat")
  tkgrid(desc.tab.pan1,sticky = "w",padx = 20)
  ####-> Membership
  tkgrid(
    tk2label(desc.tab.pan1,text = "Fuzzy Membership",justify = "left",font =
               fontLabel),pady = c(25,5),padx = 5,sticky = "w"
  )
  tclTableU <- tclArray()

  mat.U <- rbind(paste("Clus",1:ncol(cluster$U)),cluster$U)
  mat.U <- cbind(c("",paste("Obs",1:nrow(cluster$U))),mat.U)
  for (i in 1:nrow(mat.U))
    for (j in 1:ncol(mat.U))
      if (i < 2)
        tclTableU[[i - 1, j - 1]] <- strsplit(mat.U[i, j]," ",fixed = T)[[1]]
  else if (j < 2)
    tclTableU[[i - 1, j - 1]] <- strsplit(mat.U[i, j]," ",fixed = T)[[1]]
  else
    tclTableU[[i - 1, j - 1]] <- round(as.numeric(mat.U[i, j]),5)
  U.table <-
    tk2table(
      desc.tab.pan1, variable = tclTableU,titlerows = 1,titlecols = 1, width =
        4,height = 11,rows = nrow(mat.U),cols = ncol(mat.U),selectmode = "extended", colwidth = 12, background = "white",yscrollcommand = function(...)
          tkset(yscr,...),xscrollcommand = function(...)
            tkset(xscr, ...)
    )
  yscr <- tk2scrollbar(
    desc.tab.pan1, orient = "vertical",
    command = function(...)
      tkyview(U.table, ...)
  )
  xscr <- tk2scrollbar(
    desc.tab.pan1, orient = "horizontal",
    command = function(...)
      tkxview(U.table, ...)
  )
  tkgrid(
    U.table,yscr,sticky = "w",padx = 2,pady =
      c(0,5)
  )
  tkconfigure(U.table,state = "disable")
  tkgrid.configure(yscr, sticky = "nsw")
  tkgrid(xscr, sticky = "new")
  tkgrid.rowconfigure(U.table, 0, weight = 1)
  tkgrid.columnconfigure(U.table, 0, weight = 1)

  ####-> Centroid
  desc.tab.pan2 <-
    tk2frame(desc.tab,borderwidth = 2,relief = "flat")
  tkgrid(
    desc.tab.pan2,column = 1,row = 0,sticky = "nw",padx = 15
  )
  tkgrid(
    tk2label(desc.tab.pan2,text = "Cluster Centroid",justify = "left",font =
               fontLabel),pady = c(25,5),padx = 5,sticky = "w"
  )
  tclTableV <- tclArray()

  mat.V <- rbind(paste("Var ",1:ncol(cluster$V)),cluster$V)
  mat.V <- cbind(c("",paste("Cluster",1:nrow(cluster$V))),mat.V)
  mat.V <- t(mat.V)
  for (i in 1:nrow(mat.V))
    for (j in 1:ncol(mat.V))
      if (i < 2)
        tclTableV[[i - 1, j - 1]] <- strsplit(mat.V[i, j]," ",fixed = T)[[1]]
  else if (j < 2)
    tclTableV[[i - 1, j - 1]] <- strsplit(mat.V[i, j]," ",fixed = T)[[1]]
  else
    tclTableV[[i - 1, j - 1]] <- round(as.numeric(mat.V[i, j]),5)
  V.table <-
    tk2table(
      desc.tab.pan2, variable = tclTableV,titlerows = 1,titlecols = 1, width =
        4,height = 5,rows = nrow(mat.V),cols = ncol(mat.V),selectmode = "extended", colwidth = 12, background = "white",yscrollcommand = function(...)
          tkset(Vyscr,...),xscrollcommand = function(...)
            tkset(Vxscr, ...)
    )
  Vyscr <- tk2scrollbar(
    desc.tab.pan2, orient = "vertical",
    command = function(...)
      tkyview(V.table, ...)
  )
  Vxscr <- tk2scrollbar(
    desc.tab.pan2, orient = "horizontal",
    command = function(...)
      tkxview(V.table, ...)
  )
  tkgrid(
    V.table,Vyscr,sticky = "w",padx = 2,pady =
      c(0,0)
  )
  tkgrid.configure(Vyscr, sticky = "nsw")
  tkgrid(Vxscr, sticky = "new")
  tkgrid.rowconfigure(V.table, 0, weight = 1)
  tkgrid.columnconfigure(V.table, 0, weight = 1)
  tkconfigure(V.table,state = "disable")

  ####-Validation Index
  tkgrid(
    tk2label(desc.tab,text = "Validation Index",justify = "left",font =
               fontLabel),pady = c(25,5),padx = 20,sticky = "w"
  )
  tclValid <- tclArray()

  mat.val <- c("Index Value",valid[1],valid[2],valid[3],valid[4])
  mat.val <- as.matrix(mat.val)
  mat.val <-
    cbind(c("Index Name","Kwon Index","MPC Index","CE Index","XB index"),mat.val)
  mat.val <- t(mat.val)
  for (i in 1:nrow(mat.val))
    for (j in 1:ncol(mat.val))
      if (i < 2)
        tclValid[[i - 1, j - 1]] <- strsplit(mat.val[i, j]," ",fixed = T)[[1]]
  else if (j < 2)
    tclValid[[i - 1, j - 1]] <- strsplit(mat.val[i, j]," ",fixed = T)[[1]]
  else
    tclValid[[i - 1, j - 1]] <- round(as.numeric(mat.val[i, j]),5)
  Valid.table <-
    tk2table(
      desc.tab, variable = tclValid,titlerows = 1,titlecols = 1, width =
        5,height = 2,rows = nrow(mat.val),cols = ncol(mat.val),selectmode = "extended", colwidth = 15, background = "white"
    )
  tkgrid(
    Valid.table,sticky = "w",columnspan = 2,padx = 20
  )
  tkgrid.rowconfigure(Valid.table, 0, weight = 1)
  tkgrid.columnconfigure(Valid.table, 0, weight = 1)
  tkconfigure(Valid.table,state = "disable")

  ####- Manova
  tkgrid(
    tk2label(desc.tab,text = "MANOVA Analysis",justify = "left",font =
               fontLabel),pady = c(25,5),padx = 20,sticky = "w"
  )
  tclManov <- tclArray()

  mat.man <- rbind(colnames(manov),manov)
  mat.man <- cbind(c("",row.names(manov)),mat.man)

  for (i in 1:nrow(mat.man))
    for (j in 1:ncol(mat.man))
      if (i < 2)
        tclManov[[i - 1, j - 1]] <- strsplit(mat.man[i, j]," ",fixed = T)[[1]]
  else if (j < 2)
    tclManov[[i - 1, j - 1]] <- strsplit(mat.man[i, j]," ",fixed = T)[[1]]
  else if (is.na(mat.man[i,j]))
    tclManov[[i - 1,j - 1]] <- strsplit(mat.man[i, j]," ",fixed = T)[[1]]
  else
    tclManov[[i - 1, j - 1]] <- round(as.numeric(mat.man[i, j]),3)

  man.table <-
    tk2table(
      desc.tab, variable = tclManov,titlerows = 1,titlecols = 1, height =
        3,rows = nrow(mat.man),cols = ncol(mat.man),selectmode = "extended", colwidth = 12, background = "white"
    )
  tkgrid(man.table,sticky = "w",columnspan = 2,padx = 20)
  tkgrid.rowconfigure(man.table, 0, weight = 1)
  tkgrid.columnconfigure(man.table, 0, weight = 1)
  tkconfigure(man.table,state = "disable")

  tcl("ttk::style", "configure", "TNotebook", background = "white")
#   tcl("ttk::style", "configure", "TFrame", background = "white")
#   tcl("ttk::style", "configure", "TLabel", background = "white")
#   tcl("ttk::style", "configure", "TButton", background = "white",font =
#         fontCommand)
  tcl(
    "ttk::style", "configure", "TNotebook.Tab", background = "gray",font = fontCommand
  )
  tcl("ttk::style", "map", "TNotebook.Tab", background = c("active", "gray"))
  tcl("ttk::style", "map", "TNotebook.Tab", background = c("active", "gray"))
  tcl("ttk::style", "map", "TNotebook.Tab", background = c("selected",
                                                           "white"))
  tkfocus(result.tab)
  tkdestroy(parent)
}
