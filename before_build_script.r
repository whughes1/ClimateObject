oldwd=getwd()
rdir=paste(oldwd,"/R",sep="")
rsavdir=paste(oldwd,"/Rsav",sep="")
setwd(rsavdir)
unlink(list.files(),recursive=TRUE)
setwd(rdir)
file.copy(list.files(),to=rsavdir,overwrite=TRUE,recursive=TRUE,copy.date=TRUE)
unlink(list.files(),recursive=TRUE)
setwd(rsavdir)
file.copy(list.files(recursive=TRUE),to=rdir,overwrite=TRUE,copy.date=TRUE)
setwd(oldwd)
