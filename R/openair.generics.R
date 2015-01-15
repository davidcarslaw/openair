#####################
#openair.generics
#####################
#S3
#

#functions

#openair
#is.openair
#results, results.default, results.openair
#openairApply (hidden)
#summary.openair, head.openair, tail.openair
#plot.openair
#print.openair
#names.openair


#####################
#openair
#is.openair - two level tester
#####################
#kr 23/10/2010 v 0.0.1

#####################
#to do
#####################
#make test more robust?


#crude def
openair <- function(x){
    class(x) <- "openair"
    x
}

#is tester
is.openair <- function(x, full.test=TRUE, ...){

   #standard test
   output <- is(x)[1]=="openair"
   #full.test
   if(full.test & output==TRUE){
       output <- all(c("plot", "data", "call") %in% names(x))
   }
   #output
   output
}


#results


results <- function(object,...)
                  UseMethod("results")

####################
#results method
#results.default
####################
#to handle all other cases without error
#

##' @export

results.default <- function(object,...){

    object
}

#################
#results.openair
#################
#kr 13/12/2010 v 0.0.1

#################
#to do
#################

##' @export

results.openair <- function(object,
                       subset = "all", silent=FALSE,
                       ...){

   if(!is.openair(object)) return(invisible(NULL))

   if(!silent){
       message("\nopenair object created by: \n\t", deparse(object$call), "\n")
   }

   test <- object$data$subsets

   if(is.null(test)){
      if(!is.null(subset) && subset != "all"){
          warning("In results(...): subset option ignored,",
                  "\n\t[subset requested from openair object without data subsets]",
                  call. = FALSE)
      }
      return(object$data)
   }


   if(is.null(subset) || subset=="all") {
        if(!silent){
            message("contains", length(test), " data frame(s):")
            message("returning as list")
            message("\t$", paste(test, collapse=", $", sep=", $"), "\n")
        }
        return(object$data[names(object$data) != "subsets"])
   }

   temp <- subset[subset %in% test]
   if(length(temp) < 1)
       stop("In results(...): requested subset(s) not found",
           "\n\t[suggest NULL, all or one or more of available subsets]",
           "\n\t[available subset(s): ", paste(test, collapse = ", ", sep = ", "),"]",
           call. = FALSE)
   if(!silent & length(temp) < length(subset))
       warning("In results(...): some requested subset(s) not found, so ignored",
                    "\n\t[ignored subset(s): ", paste(subset[subset != temp], collapse = ", ", sep = ", "),"]",
                    "\n\t[available subset(s): ", paste(test, collapse = ", ", sep = ", "),"]",
                    call. = FALSE)
   if(length(temp)==1)
       return(object$data[[temp]]) else {
           if(!silent){
               message("returning as list of ", length(temp), " data frame(s):")
               message("\t$", paste(temp, collapse=", $", sep=", $"), "\n")
           }
           return(object$data[names(object$data) %in% temp])
       }

}


#################
#openairApply
#################
#kr 17/12/2010 v 0.0.1

################
#notes
################
#hidden? workhorse for simple generics
#

#################
#to do
#################

openairApply <- function(object, fun=summary, subset = "all",
                         ..., fun.name=deparse(substitute(fun))){

   if(!is.openair(object)) return(invisible(NULL))
   fun.name <- if(grepl("[(]",fun.name)==TRUE)
                   "supplied 'fun'" else
                       paste(fun.name, "(...)", sep="")

   message("\nopenair object created by:\n\t", deparse(object$call))

   test <- object$data$subsets

   if(is.null(test)){
      if(!is.null(subset) && subset != "all"){
          warning("In ", fun.name, ": subset option ignored,",
                  "\n\t[subset requested from openair object without data subsets]",
                  call. = FALSE)
      }
      ans <- fun(object$data,...)
      message("")
      print(ans)
      message("")
      return(invisible(ans))
   }

   if(is.null(subset) || subset=="all") {
        message("\ncontains ", length(test), " data frame(s):")
        message("\t$data$", paste(test, collapse=", $data$", sep=", $data$"), "\n")
        temp <- test
   } else {
        temp <- subset[subset %in% test]
        if(length(temp) < 1){
             message("")
             stop("In ", fun.name, ": requested subset(s) not found",
                  "\n\t[suggest one (or more) of: ", paste(test, collapse=", ", sep=", "), "]",
                  call. = FALSE)
        }

        message("\nrequested data subset(s):")
        message("\t$data$", paste(subset, collapse=", $data$", sep = ", $data$"), "\n")

        if(length(temp) < length(subset))
            warning("In ", fun.name, ": some requested subset(s) not found, so ignored",
                    "\n\t[ignored subset(s): ", paste(subset[subset != temp], collapse = ", ", sep = ", "),"]",
                    "\n\t[available subset(s): ", paste(test, collapse = ", ", sep = ", "),"]",
                    call. = FALSE)
   }

   ans <- sapply(temp, function(y){
       fun(object$data[[y]], ...) },
       simplify = FALSE, USE.NAMES = TRUE
   )

   #########################
   #strictly consistent structure
   #but messy
   #########################
   ##ans <- list(data = ans)

   print(ans)
   return(invisible(ans))

}


#################
#summary.openair
#################
#kr 17/12/2010 v 0.0.3


##' @method summary openair
##' @export
summary.openair <- function(object, subset = "all", ...)
    openairApply(object, fun=summary, subset=subset, ..., fun.name="summary")


#################
#head.openair
#tail.openair
#################
#kr 17/12/2010 v 0.0.1

################
#note
################
#x not object!
#see ?head, ?tail


##' @export
##' @importFrom utils head
##' @export 
##' @importFrom utils tail
head.openair <- function(x, subset = "all", ...)
    openairApply(x, fun=head, subset=subset, ..., fun.name="head")
tail.openair <- function(x, subset = "all", ...)
    openairApply(x, fun=tail, subset=subset, ..., fun.name="tail")



#################
#plot.openair
#################
#kr 03/11/2010 v 0.0.2

#################
#to do
#################

##' @method plot openair
##' @export
plot.openair <- function(x, subset = "all", silent = TRUE, ...){

   if(!is.openair(x)) return(invisible(NULL))

   if(!silent){
       message("\nopenair object created by: \n\t", deparse(x$call), "\n")
   }

   test <- x$plot$subsets

   if(is.null(test)){
      if(!is.null(subset) && subset != "all"){
          warning("In plot(...): subset option ignored,",
                  "\n\t[subset requested from openair object without data subsets]",
                  call. = FALSE)
      }
      return(plot(x$plot, ...))
   }


   if(is.null(subset) || subset == "all"){
      test <- x$main.plot
      if(is.null(test)){
          message("")
          stop("In plot(...): bad openair object structure",
             "\n\t[please contact openair admin if valid]",
             call. = FALSE)
      }
      return(x$main.plot())
   }

   #only plot 1, 1st valid
   temp <- subset[subset %in% test]
   if(length(temp) < 1){
        message("")
        stop("In plot(...): requested subset not found",
             "\n\t[suggest one of: ", paste(test, collapse=", ", sep=", "), "]",
             call. = FALSE)
   }

   if(length(temp)<length(subset)){
        warning("In plot(...): multiple subsets requested and some not found, using first valid",
            call. = FALSE)
   } else {
       if(length(temp)>1){
            warning("In plot(...): multiple subsets requested, using first valid",
                call. = FALSE)
       }
   }

   temp <- temp[1]
   #own plot style
   test <- x[[paste(temp,"plot", sep=".")]]
   if(!is.null(test))
       return(test(x$plot[[temp]], ...))
   #ind plot handling
   test <- x$ind.plot
   if(!is.null(test))
       return(x$ind.plot(x$plot[[temp]], ...))
   #staight plot
   return(x$plot[[temp]])
}

#################
#print.openair
#################
#kr 03/11/2010 v 0.0.2

#################
#to do
#################
##' @export
##' @method print openair
print.openair <- function(x, silent = FALSE, plot = TRUE, ...){

   if(!is.openair(x)) return(invisible(NULL))
      #must have call, data and plot elements

   if(!silent){

      message("\nopenair object created by:\n\t", deparse(x$call))

      message("\nthis contains:")

      test <- x$data$subsets
      if(is.null(test)){
          message("\ta single data frame:\n\t$data [with no subset structure]")
      } else {
        message("\t", length(test), " data frame(s):")
        message("\t$data$", paste(test, collapse=", $data$", sep=", $data$"))
      }

      test <- x$plot$subsets
      if(is.null(test)){
          message("\ta single plot object:\n\t$plot [with no subset structure]")
      } else {
        message("\t", length(test), " plot objects(s):")
        message("\t$plot$", paste(test, collapse=", $plot$", sep=", $plot$"), "\n")
      }

      message("") #tidy

   }

   if(plot) plot(x, silent = TRUE, ...)
}


#########################
#names.openair
#########################
#kr 16/12/2010 v 0.0.2

#########################
#to do
#########################
#review this
#currently not S3 and base names(x)
#so can't pass extra args via ...!
#
##' @export 
names.openair <- function(x, ...){

    #stuff we own up to...
    vis.elements <- c("data", "plot", "call")
    #make names non-recursive
    class(x) <- "not-openair"

    names(x)[names(x) %in% vis.elements]

}


