preprocess <-
function(exprsObj, log2=TRUE, norm="ALL", destname=NULL) {


if(norm == "ALL") {

    mc.normdata <- scale(exprs(exprsObj), center=TRUE, scale=FALSE)
    z.normdata <- scale(exprs(exprsObj), center=TRUE, scale=TRUE)
    q.normdata <- normalizeBetweenArrays(exprs(exprsObj), method="quantile")
    cl.normdata <- normalizeBetweenArrays(exprs(exprsObj), method="cyclicloess")

    if(log2) {

        minval <- min(exprs(exprsObj))

        if(minval > 0) {

            intensitiesFileL2 <- log2(exprs(exprsObj))

            mcL2.normdata <- scale(intensitiesFileL2, center=TRUE, scale=FALSE)
            zL2.normdata <- scale(intensitiesFileL2, center=TRUE, scale=TRUE)
            qL2.normdata <- normalizeBetweenArrays(intensitiesFileL2, 
            method="quantile")
            clL2.normdata <- normalizeBetweenArrays(intensitiesFileL2,
            method="cyclicloess")

        }

        else {

            log2 <- FALSE
            warning("Log2 transformation is infeasible due to intensity values")
        }

    }

    if(log2) {

        ## Produce multiple density plots
        if(is.null(destname)) {

            arg <- match.call()
            chArgs <- as.character(arg)
            expfile <- paste(getwd(), sprintf("/%s%s", chArgs[2],
            "_density_graphs.jpeg"), sep="")
            message(
            sprintf("Saving density graph for %s in %s", chArgs[2], getwd())
            )
        }

        else {

            if(is.character(destname)) expfile <- destname
            message(sprintf("Saving density graph in %s", expfile))
        }

        jpeg(expfile)
        par(mfrow=c(3, 3))
        plot(density(exprs(exprsObj)), xlab="Intensity values", ylab="Density",
        type="l", lwd=1.5, col="blue", main="Raw data")
        plot(density(mc.normdata), xlab="Intensity values", ylab="Density",
        type="l", lwd=1.5, col="blue", main="Mean-centering")
        plot(density(z.normdata), xlab="Intensity values", ylab="Density",
        type="l", lwd=1.5, col="blue", main="z-score")
        plot(density(q.normdata), xlab="Intensity values", ylab="Density",
        type="l", lwd=1.5, col="blue", main="Quantile")
        plot(density(cl.normdata), xlab="Intensity values", ylab="Density",
        type="l", lwd=1.5, col="blue", main="Cyclic loess")
        plot(density(mcL2.normdata), xlab="Intensity values", ylab="Density",
        type="l", lwd=1.5, col="blue", main="Mean-centering and log2")
        plot(density(zL2.normdata), xlab="Intensity values", ylab="Density",
        type="l", lwd=1.5, col="blue", main="z-score and log2")
        plot(density(qL2.normdata), xlab="Intensity values", ylab="Density",
        type="l", lwd=1.5, col="blue", main="Quantile and log2")
        plot(density(clL2.normdata), xlab="Intensity values", ylab="Density",
        type="l", lwd=1.5, col="blue", main="Cyclic loess and log2")
        dev.off()


        list(
            rawdata = exprs(exprsObj),
            mc.normdata = mc.normdata,
            z.normdata = z.normdata,
            q.normdata = q.normdata,
            cl.normdata = cl.normdata,
            mcL2.normdata = mcL2.normdata,
            zL2.normdata = zL2.normdata,
            qL2.normdata = qL2.normdata,
            clL2.normdata = clL2.normdata
        )


    }

    else {

## Produce multiple density plots
    if(is.null(destname)) {

        arg <- match.call()
        chArgs <- as.character(arg)
        expfile <- paste(getwd(), sprintf("/%s%s", chArgs[2],
        "_density_graphs.jpeg"), sep="")
        message(
        sprintf("Saving density graph for %s in %s", chArgs[2], getwd())
        )
    }

    else {


        if(is.character(destname)) expfile <- destname
        message(sprintf("Saving density graph in %s", expfile))
    }

        jpeg(expfile)
        par(mfrow=c(3, 2))
        plot(density(exprs(exprsObj)), xlab="Intensity values", ylab="Density",
        type="l", lwd=1.5,col="blue", main="Raw data")
        plot(density(mc.normdata), xlab="Intensity values", ylab="Density",
        type="l", lwd=1.5,col="blue", main="Mean-centering")
        plot(density(z.normdata), xlab="Intensity values", ylab="Density",
        type="l", lwd=1.5,col="blue", main="z-score")
        plot(density(q.normdata), xlab="Intensity values", ylab="Density",
        type="l", lwd=1.5,col="blue", main="Quantile")
        plot(density(cl.normdata), xlab="Intensity values", ylab="Density",
        type="l", lwd=1.5,col="blue", main="Cyclic loess")
        dev.off()


        list(
            rawdata = exprs(exprsObj),
            mc.normdata = mc.normdata,
            z.normdata = z.normdata,
            q.normdata = q.normdata,
            cl.normdata = cl.normdata
        )
    }
}

else {

    minval <- min(exprs(exprsObj))
    if(minval > 0) {

        intensitiesFileL2 <- log2(exprs(exprsObj))
    }

    else {

        log2 <- FALSE
        warning("Log2 transformation was infeasible due to intensity values")
    }

    if(is.null(destname)) {

        arg <- match.call()
        chArgs <- as.character(arg)
        expfile <- paste(getwd(), sprintf("/%s%s", chArgs[2],
        "_density_graphs.jpeg"), sep="")
        message(
        sprintf("Saving density graph for %s in %s", chArgs[2], getwd())
        )
    }

    else {


        if(is.character(destname)) expfile <- destname
        message(sprintf("Saving density graph for %s in %s", norm, expfile))
    }

    switch(norm,
            mc = {
            mc.normdata <- scale(exprs(exprsObj), center=TRUE, scale=FALSE)

            jpeg(expfile)
            par(mfrow=c(1, 2))
            plot(density(exprs(exprsObj)), xlab="Intensity values", 
            ylab="Density", type="l", lwd=1.5,col="blue", main="Raw data")
            plot(density(mc.normdata), xlab="Intensity values", 
            ylab="Density", type="l", lwd=1.5,col="blue", main="Mean-centering")
            dev.off()

            list(
            rawdata = exprs(exprsObj),
            mc.normdata = mc.normdata)
            },
            z = {
            z.normdata <- scale(exprs(exprsObj), center=TRUE, scale=TRUE)

            jpeg(expfile)
            par(mfrow=c(1, 2))
            plot(density(exprs(exprsObj)), xlab="Intensity values", 
            ylab="Density", type="l", lwd=1.5,col="blue", main="Raw data")
            plot(density(z.normdata), xlab="Intensity values", 
            ylab="Density", type="l", lwd=1.5,col="blue", main="z-score")
            dev.off()

            list(
            rawdata = exprs(exprsObj),
            z.normdata = z.normdata)
            },
            q = {

            q.normdata <- normalizeBetweenArrays(exprs(exprsObj), 
            method="quantile")

            jpeg(expfile)
            par(mfrow=c(1, 2))
            plot(density(exprs(exprsObj)), xlab="Intensity values", 
            ylab="Density", type="l", lwd=1.5,col="blue", main="Raw data")
            plot(density(q.normdata), xlab="Intensity values", 
            ylab="Density", type="l", lwd=1.5,col="blue", main="Quantile")
            dev.off()

            list(
            rawdata = exprs(exprsObj),
            q.normdata = q.normdata)
            },
            cl = {

            cl.normdata <- normalizeBetweenArrays(exprs(exprsObj), 
            method="cyclicloess")

            jpeg(expfile)
            par(mfrow=c(1, 2))
            plot(density(exprs(exprsObj)), xlab="Intensity values", 
            ylab="Density", type="l", lwd=1.5,col="blue", main="Raw data")
            plot(density(cl.normdata), xlab="Intensity values", 
            ylab="Density", type="l", lwd=1.5,col="blue", main="Cyclic loess")
            dev.off()

            list(
            rawdata = exprs(exprsObj),
            cl.normdata = cl.normdata)
            },
            mcL2 = {

            if(!log2) stop("no log2 data available...")

            else {
                mcL2.normdata <- scale(intensitiesFileL2, center=TRUE, 
                scale=FALSE)

                jpeg(expfile)
                par(mfrow=c(1, 2))
                plot(density(exprs(exprsObj)), xlab="Intensity values", 
                ylab="Density", type="l", lwd=1.5,col="blue", main="Raw data")
                plot(density(mcL2.normdata), xlab="Intensity values", 
                ylab="Density", type="l", lwd=1.5, col="blue", 
                main="Mean-centering and log2")
                dev.off()

                list(
                rawdata = exprs(exprsObj),
                mcL2.normdata = mcL2.normdata)
            }
            },
            zL2 = {

            if(!log2) stop("no log2 data available...")

            else {
                zL2.normdata <- scale(intensitiesFileL2, center=TRUE, 
                scale=TRUE)

                jpeg(expfile)
                par(mfrow=c(1, 2))
                plot(density(exprs(exprsObj)), xlab="Intensity values", 
                ylab="Density", type="l", lwd=1.5,col="blue", main="Raw data")
                plot(density(zL2.normdata), xlab="Intensity values", 
                ylab="Density", type="l", lwd=1.5, col="blue", 
                main="z-score and log2")
                dev.off()

                list(
                rawdata = exprs(exprsObj),
                zL2.normdata = zL2.normdata)
            }
            },
            qL2 = {

            if(!log2) stop("no log2 data available...")

            else {
                qL2.normdata <- normalizeBetweenArrays(intensitiesFileL2, 
                method="quantile")

                jpeg(expfile)
                par(mfrow=c(1, 2))
                plot(density(exprs(exprsObj)), xlab="Intensity values", 
                ylab="Density", type="l", lwd=1.5,col="blue", main="Raw data")
                plot(density(qL2.normdata), xlab="Intensity values", 
                ylab="Density", type="l", lwd=1.5, col="blue", 
                main="Quantile and log2")
                dev.off()

                list(
                rawdata = exprs(exprsObj),
                qL2.normdata = qL2.normdata)
            }
            },
            clL2 = {

            if(!log2) stop("no log2 data available...")

            else {
                clL2.normdata <- normalizeBetweenArrays(intensitiesFileL2,
                method="cyclicloess")

                jpeg(expfile)
                par(mfrow=c(1, 2))
                plot(density(exprs(exprsObj)), xlab="Intensity values", 
                ylab="Density", type="l", lwd=1.5,col="blue", main="Raw data")
                plot(density(clL2.normdata), xlab="Intensity values", 
                ylab="Density", type="l", lwd=1.5, col="blue", 
                main="Cyclic loess and log2")
                dev.off()

                list(
                rawdata = exprs(exprsObj),
                clL2.normdata = clL2.normdata)
            }
            }
        )

    }
}