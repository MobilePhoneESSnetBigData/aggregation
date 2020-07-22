
rNnetEventOD<-function(n, dupFileName, regsFileName, postLocJointPath, times = NULL ) {
    if (!file.exists(dupFileName))
        stop(paste0(dupFileName, " does not exists!"))
    
    dupProbs <- fread(
        dupFileName,
        sep = ',',
        header = TRUE,
        stringsAsFactors = FALSE
    )
    devices<-as.numeric(dupProbs[,1][[1]])
    
    # 2. read regions
    if (!file.exists(regsFileName))
        stop(paste0(regsFileName, " does not exists!"))
    
    regions <- fread(
        regsFileName,
        sep = ',',
        header = TRUE,
        stringsAsFactors = FALSE
    )
    
    # 3. read posterior location probabilities
    ndevices <- nrow(dupProbs)
    postLocJoint<-list(length = ndevices)
    for( i in 1:ndevices) {
        postLocJoint[[i]] <- readPostLocJointProb(postLocPath, dupProbs[i,1])
    }
    
}

