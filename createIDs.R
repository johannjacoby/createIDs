# uniqueID generator, johann.jacoby@gmail.com v20181109
# modify settings in setting section below if needed
# otherwise source this script by pressing CTRL+SHIFT+S.
# a file with IDs will be created whose location and name will be shown in the console when it's done.


####################################### SETTING SECTION ####################################
numberOfCodes <- 2000
sizeOfCodes <- 8
useNumbers <- 1 # should numbers be used in addition to letters? 1 = yes, 0 = no (i.e. only letters)
useVowels <- 0 # should vowels be used in addition to consonants? 1 = yes, 0 = no (i.e. only consonants)
reduceConfusion <- 1 # should certain numbers/letters not be used because they may be too easily confused with each other?
maxMult <- 2 # maximal number of times in sequence of a particular character
prefix <- "FASEL_" #a character string that should be added in frot of every id (in addition to the sizeOfCodes number) - leave blank if not needed
suffix <- "...blubber" #a character string that should be added in the end of every id (in addition to the sizeOfCodes number) - leave blank if not needed


reduceConfusionSet <- c("O","0","Q","1","I","J","N","M","6","G","M","N") # these characters will be excluded because they can be confused
# to not exclude any character, change to an empty vector:
#    c()

####################################### SETTING SECTION ####################################




###################################################################################################################
####################### do not edit below unless you know exactly what you are doing ##############################
version <- "20181109"

dir.create("uniqueID_resultfiles", showWarnings=FALSE)

customSeed <- NULL #### if you change this to a specific seed, make sure to change it back to NULL afterwards!!!!
thisSeed <- ifelse(is.null(customSeed) || is.na(as.numeric(customSeed)), as.numeric(format(Sys.time(),"%s")), customSeed)
set.seed(thisSeed)

excludeAccidentalWords <- c(paste0(rep("x",sizeOfCodes+10), collapse=""))
excludeAccidentalWordsFile <- NA
excludeFiles <- list.files(path=".", pattern = "makeUniqueIDs\\.excludes(_[0-9]{10,12})?\\.txt", full.names = TRUE, include.dirs = FALSE, ignore.case = TRUE, recursive = FALSE)

if (length(excludeFiles)>0){
	excludeFilesDates <- unlist(lapply(regmatches(excludeFiles, regexec("makeUniqueIDs\\.excludes(_([0-9]{10,12}))?\\.txt",excludeFiles)), function(x) x[3]))
	if (""%in%excludeFilesDates){excludeAccidentalWordsFile <- "makeUniqueIDs.excludes.txt"}
	if (any(!is.na(excludeFilesDates)) && any(excludeFilesDates!="")) {excludeAccidentalWordsFile <- paste0("makeUniqueIDs.excludes_",max(excludeFilesDates),".txt")}
	if (!is.na(excludeAccidentalWordsFile)){excludeAccidentalWords <- read.table(file=excludeAccidentalWordsFile, stringsAsFactors = FALSE)[,1]}
}

universe <- LETTERS
if(useVowels==0) {universe <- LETTERS[!LETTERS%in%toupper(c("a","e","i","o","u"))]}
if(useNumbers==1) {universe <- c(universe, as.character(0:9))}
if (reduceConfusion == 1) { universe <- universe[!universe%in%reduceConfusionSet]}

if (length(universe)^sizeOfCodes <= numberOfCodes*10 ){
	stop("The settings (number of requested unique codes, their size, etc.) and the settings do not allow for the generation of reasonably unique random codes. Please adjust the settings (e.g. increase the size of codes).")
}


collectIDs <- c()

pb <- txtProgressBar(min = 0, max = numberOfCodes, style = 3, char="=")

for (i in 1:numberOfCodes) {
	candidate <- paste0(sample(universe,sizeOfCodes,replace=TRUE), collapse="")
	while(candidate%in%collectIDs || length(grep(paste0(toupper(excludeAccidentalWords),collapse="|"),candidate))>0){candidate <- paste0(sample(universe,sizeOfCodes,replace=TRUE), collapse="")}
	collectIDs <- c(collectIDs, candidate)
	setTxtProgressBar(pb, i)
}
close(pb)

if(!is.na(prefix) || prefix != "") { collectIDs <- paste0(prefix, collectIDs) }
if(!is.na(suffix) || suffix != "") { collectIDs <- paste0(collectIDs, suffix) }

fileName <- paste0("uniqueID_resultfiles/uniqueIDsv",version,"_created",strftime(Sys.time(), "%Y%m%d-%H%M%S"),".txt")
pathPlusFileName <- paste0(getwd(),"/",fileName)
cat(paste0(
	paste0(
		"uniqueIDs script v",version," (questions, comments etc. to johann.jacoby@gmail.com)\n"
		,numberOfCodes, " unique random codes of ",sizeOfCodes," characters\n"
		,"sampled from these letters ",ifelse(useNumbers==1, paste0("and digits:\n",paste0(universe, collapse = " "),"\n"),"")
		, "Seed: ",thisSeed,"\n"
		, ifelse(exists("prefix") && prefix != "", paste("Prefix to each code: ", prefix,"\n"),"")
		, ifelse(exists("suffix") && suffix != "", paste("Suffix to each code: ", suffix,"\n"),"")
		, ifelse(length(excludeAccidentalWords)>0 && excludeAccidentalWords[1]!=paste0(rep("x",sizeOfCodes+10), collapse=""), paste0("The accidental strings listed in the following file have been avoided:\n[",getwd(),"/",excludeAccidentalWordsFile,"]\n"),"")
	)
	, "\n++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n"
)
, file=pathPlusFileName)
write.table(collectIDs, file=fileName, append=TRUE, quote=FALSE, col.names = FALSE, sep="\t")
cat(paste0("The codes have been created and written to the file\n[",pathPlusFileName,"]"))
set.seed(Sys.time())
