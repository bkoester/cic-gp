#Notes about running this:
#I run as follows from an R session: 
#R> output <- grade.penalty(verbose=TRUE,full_analysis=TRUE)
#
#options:
#2) verbose=FALSE:  
#   if set to TRUE, this will print a bunch of stuff to the screen, 
#   including regression results and balance statistics for the matching. 
#   It is more useful if you look at one course at a time, but could be confusing for 
#   multiple courses because of the sheer volume of stuff it will print to screen.
#3) full_analysis=FALSE: 
#   if set to TRUE, this will do regression and matching and return statistics about the 
#   gender effect. The matching can be be slow for large courses, so for this option I thought it might 
#   be useful to just do the basic grade penalty (full_analysis=FALSE) calculation to get booted up
#4) handle_duplicates=FALSE:
#   Default is to not do anything about duplicates.
#5) analyze_duplicates=TRUE:
#   This will add columns to the output table that count the number of multiple attempts at course
#   and the difference in grade between the first and last attempts for males and females. 
#   This is only meaningful if you didn't clean these out of your data. 
#   WARNING: THIS WILL CRASH IF handle_duplicates=TRUE.
#6) composite=FALSE
#   By default matching and regression use component test scores. 
#   If set to FALSE they will use MATH and ENGL scores.
#7) bigplot=TRUE
#   By default, all plots are dumped into a single PDF. If FALSE, each plot goes to plotdir 
#   directory, with a name matching the course name.
#8) ENROLL=FALSE
#   Does not run by default. ENROLL=TRUE will compute by section enrollment and include it in the
#   regression model (not matching yet) as a covariate.
#9) TERM_STABILITY=TRUE
#   By default, term stability plots are created and added to the same file as the GP plots. Set to FALSE
#   to deactivate.
#   Updates:
#   11-Jun-2015 (v2)
#    -- Fixed bug that caused full_analysis to crash for first terms with < 50 students
#    -- Added duplicate handling. Defaults to keeping last course [~lines 110-112]. 
#       See new 'remove.duplicates' function for more information.
#    -- Student records with GENDER =! 'F' | GENDER != 'M' are now explicilty thrown out.'
#   16-Jun-2015 (v3)
#    -- set default to not do anything about duplicates (i.e. handle_duplicates == FALSE)
#    -- added library call to epicalc for R > 3.1 users, who must install from source.
#    -- included options (analyze_duplicates == TRUE) for basic duplicates analysis
#   24-Jun-2015 (v4)
#    -- added option to use composite scores in analysis OR component (math and english/verbal)
#   4-Jul-2015 (v5)
#    -- set default behavior to use the COMPONENT test scores in regression/matching.
#    -- fixed bug in component regression that mistakenly also included TEST_COMP.
#   6-Jul-2015 (v6) 
#    -- added single term handling to regression modeling
#   25-July-2015 (v7)
#    -- for courses with NO terms with > 50 students, matching fails gracefully:
#       prints error to screen
#    -- all matching results will be set to NA
#   3-Aug-2015 (v8)
#    -- added option to plot to multiple files or to pile all plots in a single PDF.
#   20-Sept-2015 (v9)
#    -- added routine to compute section enrollment, effect in regression model (ENROLL=TRUE)
#   7-Nov-2015 (v10)
#    -- added routine to create term stability plots by default(TERM_STABILITY=TRUE)
######
grade.penalty <- function(verbose=FALSE,full_analysis=FALSE, handle_duplicates=FALSE,
                          analyze_duplicates=TRUE,composite=FALSE,bigplot=TRUE,ENROLL=FALSE,TERM_STABILITY=TRUE)
{
  
#Define local things in these files: libraries, courses, and other stuff
source ("config.R")
source ("startup.R")

#Read in the two data tables and merge
data <- read.delim(paste(datadir,"student.course.tab",sep=""))
sr   <- read.delim(paste(datadir,"student.record.tab",sep=""))
sc <- merge(data,sr,by='ID',all.x=TRUE)

#compute by-section enrollment numbers if the keyword is set, and exit gracefully if we don't find the 
#right column.
if (ENROLL == TRUE)
{
  if (!'SECTION' %in% names(sc))
  {
   print('Did not find SECTION column, ignoring ENROLLMENT')
  }
  if ('SECTION' %in% names(sc))
  {
  sc <- add.section.enrollment(sc)
  }
}

#Now, only keep rows where the gender is F/M. Eliminate " " and "U" in case they are there.
e <- sc$GENDER == 'M' | sc$GENDER == 'F'
sc <- sc[which(e),]

#Now get rid of all grades for which we have no Gender or test score info.
e <- !is.na(sc$GENDER)
sc <- sc[which(e),]

#If the GPAO is already granular (e.g confined to 4.0, 3.7, etc):
#sc$GPAO_GRANULAR = sc$GPAO

if (bigplot == TRUE){pdf(paste(plotdir,"GP.plots.pdf", sep = ""))}

#If it isn't then make it so:
sc$GPAO_GRANULAR = round(floor(sc$GPAO*3)/3, digits=1)

#Now create a single course ID
sc$CATALOG_NBR <- paste(sc$SUBJECT,sc$CATALOGNBR,sep=" ")

#Set up the variables to store our results. After looping over all the courses
#These will be returned in a table.

#Define basic grade penalty statistics
MALES_FULL_MEAN_GP   <- mat.or.vec(length(course_list),1)
MALES_FULL_MEAN_GP[] <- NA
MALES_FULL_SD        <- MALES_FULL_MEAN_GP
MALES_FULL_SE        <- MALES_FULL_MEAN_GP
MALES_FULL_N         <- MALES_FULL_MEAN_GP
FEMALES_FULL_MEAN_GP <- MALES_FULL_MEAN_GP
FEMALES_FULL_SD      <- MALES_FULL_MEAN_GP
FEMALES_FULL_SE      <- MALES_FULL_MEAN_GP
FEMALES_FULL_N       <- MALES_FULL_MEAN_GP
COHEN_D_STAT         <- MALES_FULL_MEAN_GP

#Regreression statistics
GENDER_EFF_REG       <- MALES_FULL_MEAN_GP
GENDER_EFF_PVAL      <- MALES_FULL_MEAN_GP

#Matching statistics and diagnostics
GENDER_EFF_MATCHED_MCTRL   <- MALES_FULL_MEAN_GP
GENDER_EFF_SE_MCTRL        <- MALES_FULL_MEAN_GP
MIN_MCTRL_MATCH      <- MALES_FULL_MEAN_GP
MAX_MCTRL_MATCH      <- MALES_FULL_MEAN_GP
MCTRL_NO_MATCH       <- MALES_FULL_MEAN_GP
GENDER_EFF_MATCHED_FCTRL   <- MALES_FULL_MEAN_GP
GENDER_EFF_SE_FCTRL        <- MALES_FULL_MEAN_GP
MIN_FCTRL_MATCH      <- MALES_FULL_MEAN_GP
MAX_FCTRL_MATCH      <- MALES_FULL_MEAN_GP
FCTRL_NO_MATCH       <- MALES_FULL_MEAN_GP
NTOT     <-  MALES_FULL_MEAN_GP
NMALES   <-  MALES_FULL_MEAN_GP
NFEMALES <- MALES_FULL_MEAN_GP
NDUP     <-  MALES_FULL_MEAN_GP
GDIFF    <-  MALES_FULL_MEAN_GP
MAXDUP   <-  MALES_FULL_MEAN_GP
NDUP_M   <-  MALES_FULL_MEAN_GP
GDIFF_M  <-  MALES_FULL_MEAN_GP
NDUP_F   <-  MALES_FULL_MEAN_GP
GDIFF_F  <-  MALES_FULL_MEAN_GP


#Loop through all of the courses and print each plot:
for( i in 1:length(course_list))
{
  
  #CHOOSE A COURSE TO ANALYZE
  cn = course_list[i]
  print(cn)
  short_cn = cn #short_codes[i]
  
  #All of the will go to this file:
  if (bigplot == FALSE){pdf(paste(plotdir,cn,".plots.pdf", sep = ""))}
  
  #Pick out only what we want
  wc = subset(sc, CATALOG_NBR == cn)
  
  #How many terms are there?
  NTERMS <- length(wc$TERM[!duplicated(wc$TERM)])

  #Now handle duplicates
  if (handle_duplicates == TRUE & NTERMS > 1)
  {
    keep <- remove.duplicates(wc,keep='LAST',verbose=TRUE)
    wc   <- wc[keep,]
  }
  
  #Get duplicate statistics if desired.
  if (analyze_duplicates == TRUE & handle_duplicates == FALSE & NTERMS > 1)
  {
    if (verbose == TRUE){print('computing duplicate statistics')}
    dup_res <- duplicates.bias.or.noise(wc)
    NTOT[i]     <- dup_res$NTOT
    NMALES[i]   <- dup_res$NMALES
    NFEMALES[i] <- dup_res$NFEMALES
    NDUP[i]     <- dup_res$NDUP
    GDIFF[i]    <- dup_res$GDIFF
    MAXDUP[i]   <- dup_res$MAXDUP
    NDUP_M[i]   <- dup_res$NDUP_M
    GDIFF_M[i]  <- dup_res$GDIFF_M
    NDUP_F[i]   <- dup_res$NDUP_F
    GDIFF_F[i]  <- dup_res$GDIFF_F
  }
  
  #Could be a course with low enrollment
  if (dim(wc)[1] > 50)
    
  {
    
    males_ctrl_omatch <- NA
    
    if (full_analysis == TRUE)
    {
      #Format things for regression and matching
      reg_input <- make.regression.format(wc)
      
      #Do the regression analysis for the course. Use either composite or component scores.
      #If there is more than one term, use TERM as a regression variable. 
      #If ENROLL is false, dont' include it in the regression model:
      if (ENROLL == FALSE)
      {
        if (composite == FALSE & NTERMS > 1) {reg <- glm(GRADE ~ GPAO + GENDER + TEST_MATH + TEST_ENGL+TERM,data=reg_input)}
        if (composite == FALSE & NTERMS == 1){reg <- glm(GRADE ~ GPAO + GENDER + TEST_MATH + TEST_ENGL,data=reg_input)}
        if (composite == TRUE  & NTERMS > 1) {reg  <- glm(GRADE ~ GPAO + GENDER + TERM + TEST_COMP,data=reg_input)}
        if (composite == TRUE  & NTERMS == 1){reg  <- glm(GRADE ~ GPAO + GENDER + TEST_COMP,data=reg_input)}
      }
      if (ENROLL == TRUE)
      {
        if (composite == FALSE & NTERMS > 1) {reg <- glm(GRADE ~ GPAO + GENDER + TEST_MATH + TEST_ENGL+SENROLL,data=reg_input)}
        if (composite == FALSE & NTERMS == 1){reg <- glm(GRADE ~ GPAO + GENDER + TEST_MATH + TEST_ENGL+SENROLL,data=reg_input)}
        if (composite == TRUE  & NTERMS > 1) {reg  <- glm(GRADE ~ GPAO + GENDER + TEST_COMP+SENROLL,data=reg_input)}
        if (composite == TRUE  & NTERMS == 1){reg  <- glm(GRADE ~ GPAO + GENDER + TEST_COMP+SENROLL,data=reg_input)}
      }
      
      
      if (verbose == TRUE)
      {
        print(paste('Found',NTERMS,'terms'))
        if (NTERMS > 1){print('included  TERM as a covariate')}
        if (NTERMS ==1){print('not using TERM as a covariate')}
        
        print(signif(summary(reg)$coefficients,4))
      }
      #Do the matching analysis. The result contains the mean and standard error of the differential GP.
      #First with males as the controls group
      males_ctrl_omatch <- full.gender.matching(reg_input,verbose=verbose,composite=composite) 
      #Then with females
      #females_ctrl_omatch <- full.gender.matching(reg_input,verbose=verbose,rev=TRUE)
      
    }
    
    #Calculate Summary Statistics: 
    #note that this calls epicalc!, not supported paste R 2.15 so it 
    #may need to be compiled from source. For this reason, I'm recalling the library in case you had 
    #to install from source and not startup.R.
    library(epicalc)
    out = aggregate(wc$GRADE, by = list(wc$GENDER, wc$GPAO_GRANULAR ),FUN = c("mean", "sd", "se", "count"))
    
    names(out) = c("Gender", "Adj_Cum_GPA","Course_Grade","sd", "se","N")
    e <- out$Adj_Cum_GPA >= 0 & out$Adj_Cum_GPA < 4.3
    out <- out[which(e),]
    
    #percents
    d = aggregate(out$N, by = list(out$Gender), FUN = c("sum"))
    nfemale <- d[1,2]
    nmale   <- d[2,2]         
    
    female_pct = paste(round(d[1,2]/sum(d[1,2]+d[2,2]) * 100, digits=0), "%", sep = "")
    male_pct =   paste(round(d[2,2]/sum(d[1,2]+d[2,2]) * 100, digits = 0), "%", sep = "")
    classsize = format(dim(wc)[1], big.mark = ",")
    title = concat(c(cn,  " (", short_cn, " ) (N = ", classsize, ")\n ",  
                     female_pct, " Females ", male_pct, " Males"))
    #Gender Comparisons by Course Grade
    d = aggregate(wc$GRADE, by = list(wc$GENDER), FUN = c("mean"))
    female_mn = paste(" Mean = ", round(d[1,2], digits=2))
    male_mn =   paste(" Mean = ", round(d[2,2], digits = 2))
    c_text = concat(c("Females: ", female_mn, " \n Males: ", male_mn))
    
    #Grade difference d value
    t = t.test(wc$GRADE~wc$GENDER)
    #Cohen's d = 2t /???(df)
    dval = round (2 * t$statistic * -1 / t$parameter^.5, digits = 2)
    c_text = concat(c(c_text, " \n d = ", dval, "\n"))
    
    #Gender Comparisons by Grade Penalty
    wc$penalty = wc$GRADE-wc$GPAO
    wc <- wc[is.finite(wc$penalty),]
    
    d = aggregate(wc$penalty, by = list(wc$GENDER), FUN = c("mean","sd","se"))

    female_mn = paste(" Mean = ", round(d[1,2], digits=2))
    male_mn =   paste(" Mean = ", round(d[2,2], digits = 2))
    c_text2 = concat(c("Females: ",  female_mn, " \n Males: ", male_mn))
    
    #Grade Penalty D Value
    t = t.test(wc$penalty~wc$GENDER)
    dval = round (abs(2 * t$statistic * -1/ t$parameter^.5), digits = 2)
    c_text2 = concat(c(c_text2, " \n d = ", dval))
    
    c_text3 = concat(c("GRADES:\n", c_text, "\nGRADE PENALTY:\n", c_text2) )
    
    #Create Plot
    limits <- aes(ymax = Course_Grade + se, ymin=Course_Grade - se)
    
    print (ggplot(data=out, aes(x=Adj_Cum_GPA, y=Course_Grade, group=Gender, colour=Gender))  + 
             #geom_point() + 
             geom_bar(data = out, aes(x = Adj_Cum_GPA, y = N/sum(N)*2, 
             colour = Gender, fill = Gender), stat = "identity") +
             geom_errorbar(limits, size = 1, width = 0.2) + 
             #geom_text(aes(label = N, y = Course_Grade - se*2), size = 3, colour = "black") + 
             geom_line(aes(y=Course_Grade-sd), colour = "green", size = 1, linetype = "dotted")  + 
             geom_line(aes(y=Course_Grade+sd), colour = "green", size = 1, linetype = "dotted")  + 
             ggtitle(concat(c(title, "\n\n", c_text3))) +
             theme(plot.title = element_text(size = rel(1))) + 
             geom_abline(slope=1, intercept=0, size = 1, colour = "black", linetype = "solid") + 
             ylim(0,4)  + xlim (0,4) +
             labs(y = "Course Grade", x = "Adjusted Cumulative GPA", subtitle=""))
            #annotate("text", x = 1, y = 3.5, label = c_text3))
     
    
    MALES_FULL_MEAN_GP[i]   <- d[2,2]
    MALES_FULL_SD[i]     <- d[2,3]
    MALES_FULL_SE[i]     <- d[2,4]
    MALES_FULL_N[i]      <- nmale
    FEMALES_FULL_MEAN_GP[i] <- d[1,2]
    FEMALES_FULL_SD[i]   <- d[1,3]
    FEMALES_FULL_SE[i]   <- d[1,4]
    FEMALES_FULL_N[i]    <- nfemale
    COHEN_D_STAT[i]      <- dval
    
    if (full_analysis == TRUE)
    {
      GENDER_EFF_MATCHED_MCTRL[i] <- -1.0*males_ctrl_omatch[1] #This makes the grade penalty the same sign as the regression results
      GENDER_EFF_SE_MCTRL[i]      <- males_ctrl_omatch[2]
      MIN_MCTRL_MATCH[i]      <- males_ctrl_omatch[3]
      MAX_MCTRL_MATCH[i]      <- males_ctrl_omatch[4]
      MCTRL_NO_MATCH[i]       <- males_ctrl_omatch[5]
      #GENDER_EFF_MATCHED_FCTRL[i] <- -1.0*females_ctrl_omatch[1] #This makes the grade penalty the same sign as the regression results
      #GENDER_EFF_SE_FCTRL[i]      <- females_ctrl_omatch[2]
      #MIN_FCTRL_MATCH      <- females_ctrl_omatch[3]
      #MAX_FCTRL_MATCH      <- females_ctrl_omatch[4]
      #FCTRL_NO_MATCH       <- females_ctrl_omatch[5]
    
      GENDER_EFF_REG[i]     <- signif(summary(reg)$coefficients[3,1],4)
      GENDER_EFF_PVAL[i]    <- signif(summary(reg)$coefficients[3,4],4)
    }
    if (TERM_STABILITY == TRUE){plot.term.stability(wc)}
  } #if course size > n
  
  if (bigplot == FALSE){dev.off()} #Turn off the plotting to PDF
  
} #Main Loop

if (bigplot == TRUE){dev.off()}

#Return the table of results
output <- data.frame(course_list,
                     MALES_FULL_MEAN_GP,MALES_FULL_SD, MALES_FULL_SE,MALES_FULL_N,
                     FEMALES_FULL_MEAN_GP,FEMALES_FULL_SD,FEMALES_FULL_SE,FEMALES_FULL_N,
                     COHEN_D_STAT,
                     GENDER_EFF_REG,GENDER_EFF_PVAL,
                     GENDER_EFF_MATCHED_MCTRL,GENDER_EFF_SE_MCTRL,
                     MIN_MCTRL_MATCH, MAX_MCTRL_MATCH,MCTRL_NO_MATCH)

#If the duplicates analysis is TRUE, then add on those columns to the final table
if (analyze_duplicates == TRUE & handle_duplicates == FALSE)
{
  output <- data.frame(output,NTOT,NMALES,NFEMALES,NDUP,
                     GDIFF,MAXDUP,NDUP_M,GDIFF_M,
                     NDUP_F,GDIFF_F)
}

return(output)
}

#This sets up the regression data frame that can also be use for the matching.
make.regression.format <- function(wc)
{
  GRADE <- as.numeric(wc$GRADE)
  GPAO             <- as.numeric(wc$GPAO)
  GENDER              <- as.factor(wc$GENDER)
  TEST_MATH        <- as.numeric(wc$TEST_MATH)
  TEST_ENGL        <- as.numeric(wc$TEST_ENGL)
  TEST_COMP        <- as.numeric(wc$TEST_COMP)
  TERM             <- as.factor(wc$TERM)
  reg_input        <- data.frame(GRADE,GPAO,GENDER,TEST_MATH,TEST_ENGL,TEST_COMP,TERM)
  
  if ('SECTION' %in% names(wc))
  {
    SENROLL       <- as.numeric(wc$ENROLL)
    reg_input     <- data.frame(reg_input,SENROLL)
    reg <- glm(GRADE ~ GPAO + GENDER + TEST_MATH + TEST_ENGL+TERM+SENROLL,data=reg_input)
    
  }
  
  #Matching needs gender also to be kept as an integer...
  reg_input$squant <- as.numeric(ifelse(reg_input$GENDER=="F", 0, 1))
  return(reg_input)
  
}

########################
#MATCHING
#Based on OPTMATCH
#Documentation is here: http://cran.r-project.org/web/packages/optmatch/index.html
#References:
#1)Hansen, B.B. and Klopfer, S.O. (2006), ‘ Optimal full matching and related designs via network flows’, 
#  Journal of Computational and Graphical Statistics, 15, 609–627.
#2) Hansen, B.B. (2004), ‘Full Matching in an Observational Study of Coaching for the SAT’, 
#   Journal of the American Statistical Association, 99, 609–618
#
#WHAT THIS DOES:
#1) Expects data in the format output by MAKE.REGRESSION.FORMAT()
#2) Does 'optimal matching' by term, on the basis of GPAO, & (TEST_ENGL, TEST_MATH) or TEST_COMP (composite == TRUE)
#3) In verbose mode (verbose = TRUE), prints out post-match balance statistics
#4) Returns an effect size by computing a weighted average of the cases - controls.
full.gender.matching <- function(reg_input,verbose=FALSE,rev=FALSE,composite=TRUE)
{
  print('matching by term')
  
  if (rev == TRUE)
  {
    print('flipping genders')
    e  <- reg_input$squant == 0
    e2 <- reg_input$squant == 1
    reg_input$squant[e]  <- 1
    reg_input$squant[e2] <- 0
    #reg_input$GENDER[e] <- 'M'
    #reg_input$GENDER[e2] <- 'F'
  }
  
  if (verbose == TRUE)
  {
    print('checking input balance')
    if (composite == TRUE)
    {
      bal <- xBalance(squant ~ GPAO +
                    TEST_COMP,data=reg_input, report = c("chisquare.test"))
    }
    else
    {
      bal <- xBalance(squant ~ GPAO +
                        TEST_ENGL+TEST_MATH,data=reg_input, report = c("chisquare.test"))
    }
    print(bal$overall)
  }
  
  #These courses are large enough that we should be able to match by TERM!
  termlist <- reg_input$TERM[!duplicated(reg_input$TERM)]
  nterm <- length(termlist)
  flag <- 0
  tflag <- 0
  
  for (i in 1:nterm)
  {
    e <- reg_input$TERM == termlist[i]
    
    #Require that the course-term has enough people.
    if (sum(e) > 50)
    {
      tflag <- 1
      sub_input <- reg_input[e,]
      #Create the GLM model that is the basis for for the Mahalanobis distances
      if (composite == TRUE)
      {
        model <- glm(squant ~ GPAO + TEST_COMP, family=binomial(), data=sub_input)
      }
      else
      {
        model <- glm(squant ~ GPAO + TEST_MATH + TEST_ENGL, family=binomial(), data=sub_input)
      }
      
      #Create a matching object
      mmon    <- match_on(model)
      
      #Make the distance matrix/propensity scores
      ppmatch <- mmon+caliper(mmon,1)
      
      #Now assign the final matches
      m1 <- fullmatch(ppmatch,data=sub_input)
      
      #Print out the quality statistics of the match
      if (verbose == TRUE)
      {
        if (composite == TRUE)
        {
          post.bal <- xBalance(squant ~ GPAO + TEST_COMP,data=sub_input, 
                             report = c("all"),
                             strata = data.frame(original = factor("none"), m1))
        }
        else
        {
          post.bal <- xBalance(squant ~ GPAO + TEST_MATH + TEST_ENGL,data=sub_input, 
                               report = c("all"),
                               strata = data.frame(original = factor("none"), m1))
        }  
        print(post.bal$overall)
      }
      
    #Get the effect size from regression of the matches...seems a little silly, but people do it.
    #lmmtch <- (summary(lm(sub_input$GRADE ~ sub_input$squant + m1)))
    
    #Integrate the matching results into the structure
    out <- data.frame(sub_input,m1)
    
    #Sum up the case-control statistics within term  
   
      if (rev == FALSE){out       <- out[order(out$m1,out$GENDER), ]}
      if (rev == TRUE) {out       <- out[order(out$m1,-out$GENDER), ]}
      out$count <- sequence(rle(as.vector(out$m1))$lengths)
    
      nid    <- length(out$m1[!duplicated(out$m1)])
      nstart <- which(out$count == 1)
      ntot   <- length(out$m1)
    
      diff   <- mat.or.vec(ntot,1)
      wt     <- diff
    
      for (j in 1:nid)
      {
        start_ind <- nstart[j]
        if (j < nid){stop_ind  <- nstart[j+1]-1}
        if (j == nid){stop_ind <- ntot}
        ind <- c(start_ind:stop_ind)
        
        #The grade difference between cases and controls for matched individuals in this term
        diff[j]   <- out$GRADE[start_ind]-mean(out$GRADE[(start_ind+1):stop_ind])
        #The 'weight', or number of controls. This is used later in the weighted average
        wt[j]     <- length(ind)-1 
        #print(out[ind,])
      }
    
    if (flag == 1)
    {
      
      allwt   <- c(allwt,wt)
      alldiff <- c(alldiff,diff) 
    }
    else
    {
      allwt <- wt
      alldiff <- diff
      flag <- 1
    }
    
    }
    
  }
  
  #A table of all the results, that is, all case-control grade penalties
  #for all terms. If all terms have < 50, spit out a warning, give NA for the table.
  
  if (tflag == 0)
    {
      print('warning: course has no terms with > 50 students. Matching fails.')
      print('setting all matching outputs to NA!')
      mneff <- NA
      seeff <- NA
      minctrls <- NA
      maxctrls <- NA
      unmatch <- NA
    }
  if (tflag == 1)
  {
    struct <- data.frame(alldiff,allwt)
  
    #Now compute the actual mean and error from this table
    #Use bootstrap estimates to get the mean and CIs/SEs
  
    nmtch  <- length(struct$alldiff)
    print(nmtch)
    nboot <- 1000
    reps  <- mat.or.vec(nboot,1)
    for (k in 1:nboot)
    {
      sub <-sample(1:nmtch,nmtch,replace=TRUE)
      reps[k] <- weighted.mean(struct$alldiff[sub],struct$allwt[sub],na.rm=TRUE)
    }
    mneff <- mean(reps)
    seeff <- sqrt(var(reps))
    minctrls <- min(allwt)
    maxctrls <- max(allwt)
    unmatch <- sum(is.na(struct$alldiff))
  }
  
  return(c(mneff,seeff,minctrls,maxctrls,unmatch)) #return the relevant stats.
  #return(post.bal)
  
}


#If the section tag exists, this will add the enrollment by section.
add.section.enrollment <- function(sc)
{
  if ('SECTION' %in% names(sc))
  {
    sc$COURSE_LONG <- paste(sc$SUBJECT,sc$CATALOGNBR,sc$TERM,sc$SECTION,sep="")
    sc       <- sc[order(sc$COURSE_LONG), ]
    sc$count <- sequence(rle(as.vector(sc$COURSE_LONG))$lengths)

    nid    <- length(sc$COURSE_LONG[!duplicated(sc$COURSE_LONG)])
    nstart <- which(sc$count == 1)
    ntot   <- length(sc$COURSE_LONG)

    ENROLL <- mat.or.vec(ntot,1)

    for (j in 1:nid)
    {
      start_ind <- nstart[j]
      if (j < nid){stop_ind  <- nstart[j+1]-1}
      if (j == nid){stop_ind <- ntot}
      ind <- c(start_ind:stop_ind)
      ENROLL[ind] <- length(ind)
    }
    sc <- data.frame(sc,ENROLL)   
  }
    return(sc)
}


#####################################
#REMOVE.DUPLICATES
#One of many things to deal with: students that take classes multiple times.
#This function keeps a specific duplicate or removes the ID altogether (default).
#You may keep NONE, the FIRST grade, or the LAST grade among the dupicates
#This assumes you have already selected a subject/catnum and returns indices of the records to KEEP.
remove.duplicates <- function(data,keep='NONE',verbose=FALSE)
{
  
  #Sort grades by ID, then TERM.
  data       <- data[order(data$ID,data$TERM), ]
  data$count <- sequence(rle(as.vector(data$ID))$lengths)
  
  ntot   <- length(data$ID)
  
  #Keep the first recorded grade of the duplicates
  if (keep == "FIRST")
  { 
    good <- which(data$count == 1)
    ngood <- length(good)
    if (verbose == TRUE)
    {
      print('keeping the first grade only')
      print(paste('kept ',ngood,' records of ',ntot,sep=""))
    }
  }    
  else
  {
    nid    <- length(data$ID[!duplicated(data$ID)])
    nstart <- which(data$count == 1)
    
    kdup   <- mat.or.vec(ntot,1)
    kfirst <- mat.or.vec(ntot,1)
    klast  <- mat.or.vec(ntot,1)
    
    for (i in 1:nid)
    {
      start_ind <- nstart[i]
      if (i < nid){stop_ind  <- nstart[i+1]-1}
      if (i == nid){stop_ind <- ntot}
      ind <- c(start_ind:stop_ind)
      kdup[ind] <- length(ind)
      kfirst[ind] <- start_ind
      klast[ind]  <- stop_ind
    }
    
    #Don't keep records for ANY duplicates.
    if (keep == "NONE") 
    {
      good  <- which(klast == kfirst)
      ngood <- length(good)
      if (verbose == TRUE)
      {
        print('discarding duplicate students')
        print(paste('kept ',ngood,' students of ',nid,sep=""))
      }
    }
    
    #Keep only the last grade among duplicates
    if (keep == "LAST") 
    {
      good <- which(data$count == kdup)
      ngood <- length(good)
      if (verbose == TRUE)
      {
        print('keeping the last grade only')
        print(paste('kept ',ngood,' records of ',ntot,sep=""))
      }   
    }
  }
  
  return(good)
  
}

#Makes the grade-penalty by TERM/GENDER plot for a course.
plot.term.stability <- function(sc)
{
  sc       <- sc[order(sc$TERM), ]
  sc$count <- sequence(rle(as.vector(sc$TERM))$lengths)
  
  nid    <- length(sc$TERM[!duplicated(sc$TERM)])
  nstart <- which(sc$count == 1)
  ntot   <- length(sc$TERM)
  
  GPAOM  <- mat.or.vec(nid,1)
  GPAOF  <- GPAOM
  GRADEM <- GPAOM
  GRADEF <- GPAOM
  TERM   <- GPAOM
  FTERM  <- GPAOM
  SUBJECT    <- sc$SUBJECT[1]
  CATALOGNBR <- sc$CATALOGNBR[1]
  
  for (j in 1:nid)
  {
    start_ind <- nstart[j]
    if (j < nid){stop_ind  <- nstart[j+1]-1}
    if (j == nid){stop_ind <- ntot}
    ind <- c(start_ind:stop_ind)
  
    sub <- sc[ind,]
    f   <- which(sub$GENDER == 'F')
    m   <- which(sub$GENDER == 'M')
    
    TERM[j]   <- sub$TERM[1]
    GPAOM[j]  <- mean(sub$GPAO[m],na.rm=TRUE)
    GRADEM[j] <- mean(sub$GRADE[m],na.rm=TRUE)
    GPAOF[j]  <- mean(sub$GPAO[f],na.rm=TRUE)
    GRADEF[j] <- mean(sub$GRADE[f],na.rm=TRUE)
  }
  
  for (i in 1:(nid-1)){FTERM[i] <- (TERM[i+1]-TERM[i])*0.25}
  e     <- FTERM > 0
  delta <- min(FTERM[e])
  FTERM <- TERM+delta
  
  e <- GRADEM !=0 & GRADEF != 0
  TERM <- TERM[e]
  FTERM <- FTERM[e]
  GRADEM <- GRADEM[e]
  GRADEF <- GRADEF[e]
  GPAOM  <- GPAOM[e]
  GPAOF  <- GPAOF[e]
  
  rvec <- (c(GPAOM,GRADEM,GPAOF,GRADEF))
  yrange <- c(2,4)
  
  
  plot(TERM,GPAOM,pch='+',xlab='TERM',main=paste(SUBJECT,CATALOGNBR,sep=" "),
       ylab='GPAO and Grade',col='blue',ylim=yrange)
  arrows(TERM,GPAOM,TERM,GRADEM,code=2,col='blue',length=0.1)
  points(FTERM,GPAOF,pch='+',col='red')
  arrows(FTERM,GPAOF,FTERM,GRADEF,code=2,col='red',length=0.1)
  legend(TERM[1],4.0,c('Females','Males'),pch=c('+','+'),col=c('red','blue'))
  
  
}




#This computes some simple statistics about duplicates
duplicates.bias.or.noise <- function(data)
{
    
    #Sort grades by ID, then TERM.
    data       <- data[order(data$ID,data$TERM), ]
    data$count <- sequence(rle(as.vector(data$ID))$lengths)
  
    ntot   <- length(data$ID)
    nid    <- length(data$ID[!duplicated(data$ID)])
    nstart <- which(data$count == 1)
    
    id     <- mat.or.vec(ntot,1)
    kdup   <- mat.or.vec(ntot,1)
    kfirst <- mat.or.vec(ntot,1)
    klast  <- mat.or.vec(ntot,1)
    
    att_1  <- mat.or.vec(nid,1)
    att_n  <- mat.or.vec(nid,1)
    cts    <- mat.or.vec(nid,1)
    gender <- mat.or.vec(nid,1)
    
    
    for (i in 1:nid)
    {
      start_ind <- nstart[i]
      if (i < nid){stop_ind  <- nstart[i+1]-1}
      if (i == nid){stop_ind <- ntot}
      ind <- c(start_ind:stop_ind)
      kdup[ind] <- length(ind)
      kfirst[ind] <- start_ind
      klast[ind]  <- stop_ind
      att_1[i]    <- data$GRADE[start_ind]
      att_n[i]    <- data$GRADE[stop_ind]
      cts[i]      <- length(ind)
      gender[i]   <- as.character(data$GENDER[start_ind])
      
    }
  
    fin  <- data.frame(att_1,att_n,cts,gender)
    
    NTOT     <- length(fin$gender)
    NMALES   <- length(which(fin$gender == 'M'))
    NFEMALES <- length(which(fin$gender == 'F'))
    
    #Statistics for everyone
    e      <- fin$cts > 1
    fin    <- fin[e,]
    NDUP   <- sum(e)
    GDIFF  <- signif(mean(fin$att_1-fin$att_n),3)
    MAXDUP <- max(fin$cts)
    #Statistics by gender
    em    <- which(fin$cts > 1 & fin$gender == 'M')
    ef    <- which(fin$cts > 1 & fin$gender == 'F')
    finm  <- fin[em,]
    finf  <- fin[ef,]

    NDUP_M <- length(em)
    GDIFF_M <- signif(mean(finm$att_1-finm$att_n),3)
    NDUP_F <- length(ef)
    GDIFF_F <- signif(mean(finf$att_1-finf$att_n),3)

    out <- data.frame(NTOT,NDUP,MAXDUP,GDIFF,NMALES,NDUP_M,GDIFF_M,NFEMALES,NDUP_F,GDIFF_F)
    return(out)
    
}
