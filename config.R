#Config.R: sourcing this will create the variables at the current level.

  #Define some directories
  codedir <- "/Users/benkoester/REBUILD/matz_paper"                         #Code goes here
  plotdir <- '~/Box.Sync/Box Sync/InComingPlots/matz_paper/'                #Plots are written here
  datadir <- "/Users/benkoester/Box.Sync/Box\ Sync/REBUILD.NON/matz_paper/" #data is read from here.
  setwd(codedir)
  
  #Define your courses:
  course_list <- c('BIOLOGY 171','BIOLOGY 172','BIOLOGY 173')#
  #'CHEM 130','CHEM 210','CHEM 211','CHEM 215',
  #'MATH 115','MATH 116',
  #'PHYSICS 140','PHYSICS 141','PHYSICS 240','PHYSICS 241',
  #'ECON 101',
  #'PSYCH 111',
  #'ENGLISH 125')