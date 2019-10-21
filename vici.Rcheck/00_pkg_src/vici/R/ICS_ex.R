#'Toy data to upload in the app.
#'
#'@docType data
#'
#'@keywords data
#'
#'@name ICS_ex
#'
#'@format A tab-separated .txt file
#'
#'@usage data(ICS_ex)
#'
#'@examples
#'if(interactive()){
#' set.seed(1382019)
#' nsubj <- 20
#' ntp <- 3
#' nstim <- 3
#' narm <- 3
#' subj <- rep(rep(rep(1:nsubj, each = ntp), times = nstim), times = narm)
#' stim <- rep(rep(c("NS", "S1", "S2"), each = nsubj*ntp), times = narm)
#' tp <- rep(rep(c("D0", "D1", "D3"), times=nsubj*nstim), times = narm)
#' a <- rep(c("Placebo", "A2", "A3"), each = nsubj*nstim*ntp)
#' y1 <- round(abs(rnorm(n=nsubj*nstim*ntp*narm,m = 0.03, sd=0.06)) +
#'   (stim=="S2" & a == "A2" & tp == "D1")*abs(rnorm(n=nsubj*nstim*ntp*narm, m = 0.05, sd=0.01)), 4)
#' y2 <- round(abs(rnorm(n=nsubj*nstim*ntp*narm,m = 0.03, sd=0.06)) +
#'   (stim=="S1" & a =="A3" & tp == "D3")*abs(rnorm(n=nsubj*nstim*ntp*narm, m = 0.1, sd=0.02)), 4)
#' ICS_ex <- cbind.data.frame("Subject" = subj, "StimulationPool" = stim, "TimePoint" = tp,
#'                            "Arm" = a, "Response1" = y1, "Response2" = y2)
#' #View(ICS_ex)
#' write.table(ICS_ex, file="Documents/GitHub/vici/data/ICS_ex.txt", sep="\t",
#' row.names = FALSE, quote = FALSE)
#'}
#'
#'
NULL
