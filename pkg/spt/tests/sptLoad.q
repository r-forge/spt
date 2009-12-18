
path.to.spt <- "~/Files/Subversion/spt/pkg/spt/R"
pp <- function(...) print(paste(...))
for ( i in c("utilities.q","AllGenerics.q")) {
  pp("sourcing file:", paste(path.to.spt, i, sep="/"))
  source( paste(path.to.spt, i, sep="/") )
}
  
spt.dir.files <- list.files(path.to.spt)

#for (i in rev(spt.dir.files)){
#  if ( length(grep( "^Class-[0-9A-Za-z]*.q$", i))>0 ) {
#    pp("sourcing file:",paste( path.to.spt, i, sep="/"))
#    source(paste(path.to.spt, i, sep="/"))
#  }
#}
i <- "AllClasses.q"
pp("sourcing file:",paste( path.to.spt, i, sep="/"))
source(paste(path.to.spt, i, sep="/"))


for (i in rev(spt.dir.files)){
  if ( length(grep( "-methods.q$", i))>0 ) {
    pp("sourcing file:",paste( path.to.spt, i, sep="/"))
    source(paste(path.to.spt, i, sep="/"))
  }
}
