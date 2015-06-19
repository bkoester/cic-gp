# cic-gp
These are scripts and sample configuration files for the first analysis of grade penalties. As the goals of the paper and the analysis evolve, this project will adapt.

To on-board people, we are starting with all of the scripts/routines in a single file. This includes plotting and analysis. The longer term plan is begin breaking out scripts in separate files and to create configuration files as people get more comfortable. 

A few basic steps to be taken:
1) Debug errors as people discover them.
2) Create a configuration file that contains local paths to code, data, and output.
3) Split out various subroutines into separate files to keep things tidy.

(18-Jun-2015)
As it stands, to run this you need to download all R files and set directories and courses and in config.R. Notice that you can set 'codedir', the path to the code in the config file. The best thing to do is place all your files in that directory, and or just initiate this GitHub project there.