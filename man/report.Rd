\name{report}
\alias{report}

\title{
    Produce an HTML report of the mAP-KL analysis
}
\description{
This function gathers the results of several analysis sessions (
feature selection, classification, annotation and network) and produces a
report in HTML format.
}
\usage{
    report(mAPKLObj, ClassifyObj, AnnotObj=NULL, netObj=NULL, file)
}

\arguments{
    \item{mAPKLObj}{
    An object of mAPKL class. %%\code{mAPKLObj}
    }
    \item{ClassifyObj}{
    An object of mAPKL class. %%\code{ClassifyObj}
    }
    \item{AnnotObj}{
    An object of Annot class. %%\code{AnnotObj}
    }
    \item{netObj}{
    An object of NetAttr class. %%\code{netObj}
    }
    \item{file}{
    The full path and the name of the produced report %%\code{file}
    }
}
\details{
It presents the data samples with their phenotype labels, the exemplars with
their statistical scores( adj.p-value, p-value and fc), and network
characteristics (like weighted local degree,closeness,betweenness,
transitivity) if such a network analysis has been performed. In addition,
the report presents the classification performance achieved by those
exemplars (either cross-validation or hold-out), and finally several
annoation information (symbol, entrez id, ensemble id, and their chromosomal
location) if an annotation analysis has been carried out.
}

\value{
Upon successful completion an HTML report is saved in the working directory.
}
\author{
    Argiris Sakellariou
}

\examples{
## When a network attributes object is present
\dontrun{report(out,class.pred,class.metrics,netObj=net.attr,
"C:/.../.../mAPKLreport.html")#Define a local path to store your report}

## When an annotation object is present
\dontrun{report(out,class.pred,class.metrics,gene.info,
"C:/.../.../mAPKLreport.html")#Define a local path to store your report}

## Without annotation and network attributes objects
\dontrun{report(out,class.pred,class.metrics,
file="C:/.../.../mAPKLreport.html")#Define a local path to store your report}
}

\keyword{ methods }
