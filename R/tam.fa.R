## File Name: tam.fa.R
## File Version: 9.19
#################################################################
# Exploratory Factor Analysis and Bifactor Models
tam.fa <- function( resp , irtmodel , dims=NULL , nfactors=NULL ,
            pid = NULL ,pweights = NULL , verbose = TRUE , control = list() )
{
    require_namespace_msg("GPArotation")
    require_namespace_msg("psych")
    disp <- "....................................................\n"
    increment.factor <- progress <- nodes <- snodes <- ridge <- xsi.start0 <- QMC <- NULL
    maxiter <- conv <- convD <- min.variance <- max.increment <- Msteps <- convM <- NULL

    #**** handle verbose argument
    args_CALL <- as.list( sys.call() )
    if ( ! tam_in_names_list( list=control, variable="progress" )     ){
        control$progress <- tam_args_CALL_search( args_CALL=args_CALL , variable="verbose" ,
                                default_value = TRUE )
    }
    #*******

    # attach control elements
    e1 <- environment()
    con <- list( nodes = seq(-6,6,len=21) , snodes = 1500 ,QMC=TRUE,
                convD = .001 ,conv = .0001 , convM = .0001 , Msteps = 4 ,
                maxiter = 1000 , max.increment = 1 ,
                min.variance = .001 , progress = TRUE , ridge=0,seed=NULL,
                xsi.start0=FALSE , increment.factor=1)
    con[ names(control) ] <- control
    Lcon <- length(con)
    con1a <- con1 <- con ;
    names(con1) <- NULL
    for (cc in 1:Lcon ){
        assign( names(con)[cc] , con1[[cc]] , envir = e1 )
    }
    if ( !is.null(con$seed)){ set.seed( con$seed )     }
    maxK <- max(resp ,na.rm=TRUE)

    #************************************************************
    # irtmodel = bifactor 1 or bifactor2
    if (irtmodel %in% c("bifactor1","bifactor2")){
        dim.names <- sort( unique(paste(dims) ))
        dim.names <- dim.names[ dim.names!="NA" ]
        dims.num <- match( dims , dim.names )
        D <- length( unique(dim.names))
        # define Q matrix for bifactor model
        I <- ncol(resp)
        Q <- matrix( 0 , I , D+1 )
        Q[,1] <- 1
        for (dd in 1:D){
            Q[dims.num==dd,dd+1] <- 1
        }
        rownames(Q) <- colnames(resp)
        colnames(Q) <- c("g" , dim.names )
        # variance constraints
        variance.fixed <- NULL
        if (irtmodel == "bifactor2"){
            variance.fixed <- cbind( 1:(D+1) , 1:(D+1) , 1 )
        }
        for (dd in 1:D){
            v1 <- cbind( dd , seq(dd+1, D+1) , 0 )
            variance.fixed <- rbind( variance.fixed , v1 )
        }
    }
    #************************************

    #************************************************************
    # irtmodel = efa
    # exploratory factor analysis
    if (irtmodel %in% c("efa")){
        D <- nfactors
        # define Q matrix for bifactor model
        I <- ncol(resp)
        Q <- matrix( 1 , I , D )
        Q[,1] <- 1
        for (dd in 2:D){
            Q[ seq(1,dd-1) , dd ] <- 0
                    }
        rownames(Q) <- colnames(resp)
        colnames(Q) <- paste0("Dim",1:D)
        # variance constraints
        variance.fixed <- cbind( 1:D , 1:D , 1 )
        for (dd in 1:(D-1) ){
            v1 <- cbind( dd , seq(dd+1, D) , 0 )
            variance.fixed <- rbind( variance.fixed , v1 )
        }
    }
    #************************************

    #*****************************
    # define item response model
    irtmodel2 <- if (maxK==1){"2PL" } else {"GPCM" }


    #*****************************************************
    # estimate model
    if ( irtmodel %in% c("bifactor2","efa") ){
        res <- tam.mml.2pl( resp=resp , Q=Q , irtmodel= irtmodel2 ,
                    variance.fixed=variance.fixed , pid=pid ,
                    pweights=pweights , control=con )
    }
    if ( irtmodel=="bifactor1"){
        res <- tam.mml( resp=resp , Q=Q , variance.fixed=variance.fixed , pid=pid ,
                    pweights=pweights , control=con )
    }
    #****
    # calculate standardized loadings
    B <- res$B
    B <- B[,2,]
    # B for Rasch testlet model
    if (irtmodel=="bifactor1"){
        Bsd <- sqrt( diag( res$variance ) )
        B <- B * matrix( Bsd , nrow=nrow(B) , ncol= ncol(B) , byrow=TRUE)
    }
    itemvariance <- rowSums( B^2 ) + 1.7^2    # add logistic variance
    itemvariance <- matrix( itemvariance, nrow=nrow(B), ncol= ncol(B), byrow=FALSE)
    if (irtmodel %in% c("bifactor1","bifactor2","efa") ){
        B.stand <- B / sqrt( itemvariance )
    }
    res$B.stand <- B.stand
    res$itemvariance <- itemvariance

    # oblimin rotation in exploratory factor analysis
    if (irtmodel=="efa"){
        res$efa.oblimin <- GPArotation::oblimin(L=B.stand)
        # Schmid Leiman transformation
        corrmatr <- tcrossprod( B.stand )
        diag(corrmatr) <- 1
        sl.sol <- psych::schmid(model=corrmatr, nfactors = nfactors )
        res$B.SL <- B.stand <- sl.sol$sl[ , seq(1,nfactors+1) ]
    }

    res$irtmodel <- irtmodel
    #****
    # calculate dimensionality/reliability measures
    res$meas <- tam_fa_reliability_measures( B.stand=B.stand,
                    itemvariance=itemvariance, xsi=res$xsi, maxK=maxK )
    #--- output
    return(res)
}
#####################################################################
