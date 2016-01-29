\name{rforcecom.update}
\alias{rforcecom.update}

\title{
Update a record
}

\description{
Update a record
}

\usage{
rforcecom.update(session, objectName, id, 
                 fields, applyAssignmentRules=TRUE)
}

\arguments{
  \item{session}{ Session data. It can be retrieve from \code{\link{rforcecom.login}}. }
  \item{objectName}{ An object name. (ex: "Account", "Contact", "CustomObject__c") }
  \item{id}{ Record ID to update. (ex: "999x000000xxxxxZZZ") }
  \item{fields}{ Sets of field name and its value. (ex: Name="CompanyName", Phone="000-000-000" ) }
  \item{applyAssignmentRules}{ A logical value or a valid AssignmentRule Id applied for created or updated Cases or Leads. If TRUE, the existing active assignment rules will fire. }
}

\value{
No data.
}

\author{
Takekatsu Hiramura <thira@plavox.info>
}

\examples{
\dontrun{
 # Updating a record
 objectName <- "Account"
 id <- "999x000000xxxxxZZZ"
 fields <- c(Phone="9999-9999-9999")
 rforcecom.update(session, objectName, id, fields)
 }
}

\keyword{connection}

