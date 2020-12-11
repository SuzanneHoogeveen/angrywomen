## Function calls order-diagram for example 1
fig.ex1 <- function(){
  names <- c("a-m", "a-f", "s-m", "s-f")
  cols <- c('#d6f5d6', 'lavender', '#cce6ff', "mistyrose1")
  
  layout(matrix(c(1, 1, 2, 2
                  , 3, 3, 4, 5
                  , 6, 6, 7, 7)
                , ncol = 4, byrow = T))
  
  par(mar = c(1, 0, 2, 0))
  
  ## No Prejudice Model
  openplotmat(main = "A. Gender Stereotyping")
  elpos <- coordinates (c(1, 2, 1), relsize = .9)
  ord <- c(1,3,4,2)
  names.n <- names[ord]
  cols.n <- cols[ord]
  
  straightarrow(from = elpos[1, ], to = elpos[2, ], arr.length = 0)
  straightarrow(from = elpos[1, ], to = elpos[3, ], arr.length = 0)
  straightarrow(from = elpos[2, ], to = elpos[4, ], arr.length = 0)
  straightarrow(from = elpos[3, ], to = elpos[4, ], arr.length = 0)
  #straightarrow(from = elpos[1, ], to = elpos[4, ], arr.length = 0)
  #straightarrow(from = elpos[2, ], to = elpos[3, ], arr.length = 0)
  for ( i in 1:4){
    textround (elpos[i, ], 0.04, 0.06, lab = names.n[i], cex = 1.5, shadow.size = 0, box.col = cols.n[i])
  }
  
  ## Prejudice Model
  openplotmat(main = "B. Status Signalling")
  elpos <- coordinates (c(1, 2, 1), relsize = .9)
  ord <- c(1,3,2,4)
  names.n <- names[ord]
  cols.n <- cols[ord]
  
  straightarrow(from = elpos[1, ], to = elpos[2, ], arr.length = 0)
  straightarrow(from = elpos[1, ], to = elpos[3, ], arr.length = 0)
  straightarrow(from = elpos[2, ], to = elpos[4, ], arr.length = 0)
  straightarrow(from = elpos[3, ], to = elpos[4, ], arr.length = 0)
  #straightarrow(from = elpos[1, ], to = elpos[4, ], arr.length = 0)
  
  for ( i in 1:4){
    textround (elpos[i, ], 0.04, 0.06, lab = names.n[i], cex = 1.5, shadow.size = 0, box.col = cols.n[i])
  }
  
  ## No Prejudice Model
  openplotmat(main = "C. Cultural Change /\nStudy Savviness")
  elpos <- coordinates (c(1, 3), relsize = .9)
  ord <- c(2,4,1,3)
  names.n <- names[ord]
  cols.n <- cols[ord]
  
  straightarrow(from = elpos[1, ], to = elpos[2, ], arr.length = 0)
  straightarrow(from = elpos[1, ], to = elpos[3, ], arr.length = 0)
  straightarrow(from = elpos[1, ], to = elpos[4, ], arr.length = 0)
  straightarrow(from = elpos[3, ], to = elpos[4, ], arr.length = 0)
  #straightarrow(from = elpos[1, ], to = elpos[4, ], arr.length = 0)
  #straightarrow(from = elpos[2, ], to = elpos[3, ], arr.length = 0)
  for ( i in 1:4){
    textround (elpos[i, ], 0.04, 0.06, lab = names.n[i], cex = 1.5, shadow.size = 0, box.col = cols.n[i])
  }
  
  ## No Prejudice Model
  #openplotmat(main = "C. Cultural Change /\nStudy Savviness")
  #elpos <- coordinates (c(1, 1, 2), relsize = .9)
  #ord <- c(2,4,1,3)
  #names.n <- names[ord]
  #cols.n <- cols[ord]
  
  #straightarrow(from = elpos[1, ], to = elpos[2, ], arr.length = 0)
  #straightarrow(from = elpos[2, ], to = elpos[4, ], arr.length = 0)
  #straightarrow(from = elpos[2, ], to = elpos[3, ], arr.length = 0)
  #straightarrow(from = elpos[3, ], to = elpos[4, ], arr.length = 0)
  #straightarrow(from = elpos[1, ], to = elpos[4, ], arr.length = 0)
  #straightarrow(from = elpos[2, ], to = elpos[3, ], arr.length = 0)
  #for ( i in 1:4){
  #   textround (elpos[i, ], 0.04, 0.06, lab = names.n[i], cex = 1.5, shadow.size = 0, box.col = cols.n[i])
  #}
  
  ## Cultural Differences
  openplotmat(main = "D1. Cultural Differences\n(Westerners)")
  elpos <- coordinates (c(2, 2), relsize = .9)
  ord <- 1:4
  names.n <- names[ord]
  cols.n <- cols[ord]
  
  straightarrow(from = elpos[1, ], to = elpos[3, ], arr.length = 0)
  straightarrow(from = elpos[2, ], to = elpos[4, ], arr.length = 0)
  #straightarrow(from = elpos[1, ], to = elpos[4, ], arr.length = 0)
  #straightarrow(from = elpos[2, ], to = elpos[3, ], arr.length = 0)
  for ( i in 1:4){
    textround (elpos[i, ], 0.1, 0.06, lab = names.n[i], cex = 1.5, shadow.size = 0, box.col = cols.n[i])
  }
  
  ## Cultural Differences B
  openplotmat(main = "D2. Cultural Differences\n(Easterners)")
  elpos <- coordinates (c(2,2), relsize = .9)
  ord <- c(3,4,1,2)
  names.n <- names[ord]
  cols.n <- cols[ord]
  
  straightarrow(from = elpos[1, ], to = elpos[3, ], arr.length = 0)
  straightarrow(from = elpos[2, ], to = elpos[4, ], arr.length = 0)
  #straightarrow(from = elpos[1, ], to = elpos[4, ], arr.length = 0)
  #straightarrow(from = elpos[2, ], to = elpos[3, ], arr.length = 0)
  for ( i in 1:4){
    textround (elpos[i, ], 0.1, 0.06, lab = names.n[i], cex = 1.5, shadow.size = 0, box.col = cols.n[i])
  }
  
  
  ## Null Model
  openplotmat(main = "E. Null Model")
  elpos <- coordinates (c(4), my = .3)
  ord <- 1:4
  names.n <- names[ord]
  cols.n <- cols[ord]
  
  straightarrow(from = elpos[1, ], to = elpos[2, ], arr.length = 0)
  straightarrow(from = elpos[2, ], to = elpos[3, ], arr.length = 0)
  straightarrow(from = elpos[3, ], to = elpos[4, ], arr.length = 0)
  for ( i in 1:4){
    textround (elpos[i, ], 0.04, 0.06, lab = names.n[i], cex = 1.5, shadow.size = 0, box.col = cols.n[i])
  }
  
  leglab <- c("Angry expression, male target", "Angry expression, female target", "Sad expression, male target", "Sad expression, female target")
  legend(.02, .55, legend = leglab, fill = cols.n, bty = "n", cex = 1.3)
  # legend(.5, .1, legend = leglab[3:4], fill = cols.n[3:4], bty = "n", cex = 1.3, horiz = T, xjust = .5)
  
  ## Unconstrained Model
  openplotmat(main = "F. Unconstrained Model")
  elpos <- cbind(c(.35, .65, .35, .65), c(.8, .6, .4, .2))
  ord <- 4:1
  names.n <- names[ord]
  cols.n <- cols[ord]
  
  for ( i in 1:4){
    textround (elpos[i, ], 0.04, 0.06, lab = names.n[i], cex = 1.5, shadow.size = 0, box.col = cols.n[i])
  }
}

######################################################################

## Function calls order-diagram for hypothetical partial weak and unconstrained order examples

fig.hypothetical <- function(){
  layout(matrix(1:2, ncol = 2))
  par(mar=c(.5, 1, .5, 1))
  names=c(1:6)
  M=matrix(nrow=length(names),byrow=F,ncol=length(names),data=0)
  M[1,2]=M[1,3]=''
  M[1,4]=M[1,5]=''
  M[2,6]=M[3,6]=''
  M[4,6]=M[5,6]=''
  M[2,3]=M[4,5]=''
  
  
  plotmat(M,name=names,pos=c(1,4,1),curve=0,box.type="round",box.size=.04,box.prop=1,box.col=c('#cce6ff'),arr.length=0,arr.type='triangle'
          , shadow.size = 0)
  # box()
  mtext(side=3,adj=0,line=-2,"A.",cex=1.2)
  
  M=matrix(nrow=length(names),byrow=F,ncol=length(names),data=0)
  plotmat(M,name=names,curve=0,box.type="round",box.size=.04,box.prop=1,box.col=c('#cce6ff'),arr.length=0,arr.type='triangle'
          , shadow.size = 0)
  # box()
  mtext(side=3,adj=0,line=-2,"B.",cex=1.2)
}


#################################

## Function calls order-diagram for example 2

fig.ex2 <- function(){
  #Code for Symbolic Distance 
  layout(matrix(ncol=3,1:3,byrow=T))
  par(mar=c(.5, 1, .5, 1),mgp=c(2,.3,0), cex = 1)
  myCol=c('lightblue','lavender','lightgreen')
  
  ## A. Analog-Representation
  openplotmat()
  elpos <- coordinates (c(2, 2, 2), relsize = .8)
  ord <- c(1,4,2,5,3,6)
  names.n <- c(4:2,6:8)[ord]
  cols.n <- rep(myCol,each=2)
  
  straightarrow(from = elpos[1, ], to = elpos[3, ], arr.length = 0)
  straightarrow(from = elpos[3, ], to = elpos[5, ], arr.length = 0)
  straightarrow(from = elpos[2, ], to = elpos[4, ], arr.length = 0)
  straightarrow(from = elpos[4, ], to = elpos[6, ], arr.length = 0)
  for ( i in 1:6){
    textround (elpos[i, ], 0.04, 0.06, lab = names.n[i], cex = 1.3, shadow.size = 0, box.col = cols.n[i])
  }
  axis(2,at=c(.2,.8),lab=c("Faster","Slower"),cex=1.3,line=-1.25, shadow.size = 0)
  mtext(side=3,adj=.5,line=-1,"A. Analog Rep.",cex=1.2)
  
  ## B. Propositional Representation
  openplotmat()
  elpos <- coordinates (c(3, 3), relsize = .8)
  names.n <- c(2:4,6:8)
  cols.n <- c(rev(myCol),myCol)
  
  straightarrow(from = elpos[1, ], to = elpos[2, ], arr.length = 0)
  straightarrow(from = elpos[2, ], to = elpos[3, ], arr.length = 0)
  straightarrow(from = elpos[4, ], to = elpos[5, ], arr.length = 0)
  straightarrow(from = elpos[5, ], to = elpos[6, ], arr.length = 0)
  for ( i in 1:6){
    textround (elpos[i, ], 0.04, 0.06, lab = names.n[i], cex = 1.3, shadow.size = 0, box.col = cols.n[i])
  }
  mtext(side=3,adj=.5,line=-1,"B. Propositional Rep.",cex=1.2)
  
  
  ## C. Priming + Spreading Activation
  openplotmat()
  elpos <- coordinates (c(2, 2, 2), relsize = .8)
  ord <- c(1,4,2,5,3,6)
  names.n <- c(2:4,8:6)[ord]
  cols.n <- myCol[c(3,3,2,2,1,1)]
  
  straightarrow(from = elpos[1, ], to = elpos[3, ], arr.length = 0)
  straightarrow(from = elpos[3, ], to = elpos[5, ], arr.length = 0)
  straightarrow(from = elpos[2, ], to = elpos[4, ], arr.length = 0)
  straightarrow(from = elpos[4, ], to = elpos[6, ], arr.length = 0)
  for ( i in 1:6){
    textround (elpos[i, ], 0.04, 0.06, lab = names.n[i], cex = 1.3, shadow.size = 0, box.col = cols.n[i])
  }
  mtext(side=3,adj=.5,line=-1,"C. Priming + Spreading Act.",cex=1.2)
}
