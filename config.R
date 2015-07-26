#Config.R: sourcing this will create the variables at the current level.

  #Define some directories
  codedir <- "/Users/bkoester/Google\ Drive/code/REBUILD/matz_paper/cic-gp/"   #Code goes here (git directory!)
  plotdir <- '~/Box Sync/InComingPlots/matz_paper/'              #Plots are written here
  datadir <- "/Users/bkoester/Box\ Sync/REBUILD.NON/matz_paper/" #data is read from here.
  setwd(codedir)
  
  #Define your courses:
  course_list <- c('BIOLOGY 100', 'BIOLOGY 107','BIOLOGY 282','BIOLCHEM 212','BIOLOGY 164',
                   'BIOLOGY 171','BIOLOGY 172','BIOLOGY 173',
                   'ACC 271','ECON 101','ECON 404','OMS 301', #ACC 271 doesnt' seem to fit
                   'ASTRO 171','GEOSCI 341','PHYS 106',
                   'CHEM 125','CHEM 126',
                   'CHEM 130','CHEM 210','CHEM 211','CHEM 215','CHEM 216',
                   'ENGR 100',
                   'MATH 115','MATH 185','MATH 116','MATH 156',
                   'ASTRO 120','ASTRO 122','ASTRO 125','ASTRO 101','ASTRO 188','PHYSICS 112','PHYSICS 288',
                   'PHYSICS 125','PHYSICS 135','PHYSICS 140',
                   'PHYSICS 127','PHYSICS 136','PHYSICS 141',
                   'PHYSICS 126','PHYSICS 235','PHYSICS 128','PHYSICS 241',
                   'PHYSICS 160','PHYSICS 161','PHYSICS 240',
                   'POLSCI 111','POLSCI 140','POLSCI 160','POLSCI 101',
                   'PSYCH 111','PSYCH 112','PYSCH 116','PSYCH 171',
                   'SOC 101','SOC 102','SOC 103','SOC 195',
                   'STATS 402','STATS 250','STATS 350','IOE 265','MATH 425',
                   'ENGLISH 124','ENGLISH 125','ENGLISH 225')
  course_list <- 'SOC 101'
  