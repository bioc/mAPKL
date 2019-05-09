\name{annotate}
\alias{annotate}

\title{
    Genome annotation of the "exemplars".
}
\description{
The user may extract several genome specific information for the "exemplars"
as described in the microarray annotation file.
}
\usage{
    annotate(exemplars,chip)
}

\arguments{
    \item{exemplars}{
    The "exemplars" of the mAPKL class. %%\code{exemplars}
    }
    \item{chip}{
    The platforms's name of the microarray chip used (e.g. "hgu133plus2.db")
    %%\code{chip}
    }
}
\details{
This function uses as key the probe id and returns the mathing information
as described in the gene chip annotation file. The returned information are
ussually multiple to the number of probe ids (one to many relationship) and are 
stored in the {results}. In the rest items only the first of the returned 
values are stored.

}
\value{
    \item{results}{
    The accumulated annotation results.  %%\code{results}
    }
    \item{probe}{
    The probe id.  %%\code{probe}
    }
    \item{symbol}{
    The official gene symbol.  %%\code{symbol}
    }
    \item{entrezId}{
    The Entrez gene Identifier. %\code{entrezId}
    }
    \item{ensemblId}{
    The ensembl ID as indicated by ensembl. %%\code{ensemblId}
    }
    \item{map}{
    The cytoband locations of the gene. %%\code{map}
    }
}

\author{
    Argiris Sakellariou
}

\examples{
## We use the "exemplars" from the mAPKL.Rd example

exemplrs <- c(24, 26, 42, 45, 63, 81, 95, 99, 102, 113, 134, 135, 145, 152, 168)
names(exemplrs)<- c("215717_s_at", "1561358_at", "222752_s_at", "233922_at",
"218871_x_at", "33323_r_at", "244311_at", "220932_at", "205508_at", "209596_at",
"215180_at", "1560638_a_at", "201852_x_at", "229947_at", "221731_x_at")

gene.info <- annotate(exemplrs,"hgu133plus2.db")
}

\keyword{ methods }
