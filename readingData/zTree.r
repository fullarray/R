#make sure the current working directory is set. E.g. setwd(yourProjectDataset)
#make sure the current working directory is set. E.g. setwd(yourProjectDataset)

#store files read in a variable
files <- lis.files(pattern = "[0-9]{6}_[0-9]{4}.xls$]", recursive=TRUE)
trustGS <- zTreeTables(files)

#save as Rdata file
save(trustGS, zTreeTables, file="yourFileName.Rdata")

#save in Stata-13 format
saveGeneric<-with(trustGS, merge(globals, subjects))
write.dta(saveGeneric, file="yourFileName.dta")

#save in CSV
write.csv(saveGeneric, file="yourFileName.csv")
fn<-list.files(pattern="yourFileName\\.[^.]*")
xtable(cbind(name=fn, size=file.size(fn)))

#if two tables need to be merged
with(trustGS, merge(globals, subjects))
