# define matrices
nvars <- 100
ncons <- 50
n_nonzero <- round(0.2*nvars*ncons) # approximate, there may be actually less values
Amat <- data.frame(
	i=sample.int(ncons, n_nonzero, replace=TRUE),
	j=sample.int(nvars, n_nonzero, replace=TRUE),
	x=runif(n_nonzero)
)
Amat <- Amat[!duplicated(Amat[,c("i", "j")]),]
vars <- runif(nvars)

# Matrix solution
library(Matrix)
AmatSparse <- Matrix::sparseMatrix(i=Amat[,"i"], j=Amat[,"j"], x=Amat[,"x"])
expected <- as.vector(AmatSparse %*% vars)
str(expected)

# SQLite solution
library(RSQLite)
dbname <- "sparsemat.db"
con <- dbConnect(drv=RSQLite::SQLite(), dbname=dbname)
dbWriteTable(con, name="Amat", value=Amat, overwrite=TRUE)
dbWriteTable(con, name="vars", value=data.frame(j=seq(1:nvars), x=vars), overwrite=TRUE)	
mat_mult_sqlite <- function() {
	ans <- dbGetQuery(con, paste(
      "select sum(Amat.x*vars.x) from Amat",
      "join vars on Amat.j=vars.j",
      "group by Amat.i order by Amat.i"
    ))
	return(ans[,1, drop=TRUE])
}
x <- mat_mult_sqlite()
all.equal(expected, x)

# tiledb
library(tiledb)
uri <- file.path("c:/users/karsten.weinert/documents/tiledb/sparsemat")
if(dir.exists(uri)) unlink(uri, recursive=TRUE)
AmatSparse <- Matrix::sparseMatrix(i=Amat[,"i"], j=Amat[,"j"], x=Amat[,"x"])
fromSparseMatrix(AmatSparse, uri)
AmatTileDB <- tiledb_array(uri = uri, is.sparse = TRUE)
