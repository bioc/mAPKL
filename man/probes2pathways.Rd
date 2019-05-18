\name{probes2pathways}
\alias{probes2pathways}

\title{
    Extract pathways from "exemplars"
}
\description{
The user may extract the pathways where the "exemplars" are involved with the 
aid of the reactome database.
}
\usage{
    probes2pathways(annotObj)
}

\arguments{
    \item{annotObj}{
    The "Annot" class object. %%\code{annotObj}
    }
}
\details{
This function utilizes the "Annot" class object as returned by the "annotate" 
function to extract the pathways where the "exemplars" are involved with the 
aid of the "reactome" database. We employ the probe Ids for the matching.
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

## We now use the "gene.info" to identify the relevant pathways

probes2pathways(gene.info)
}

\keyword{ methods }

