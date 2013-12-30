#!/bin/bash - 
#===============================================================================
#          FILE: grat_usage.sh
#         USAGE: ./grat_usage.sh 
#        AUTHOR: mrajner
#       CREATED: 12.01.2013 16:44:52 CET
#===============================================================================

set -o nounset                              # Treat unset variables as an error

# after successfully source compilation you should be able to run this command
# make sure the grat command can be found in your executables path

  grat \
    -S JOZE:52.1:21.1:110, 3:3:3 \
    -F /home/mrajner/dat/ncep_reanalysis/pres.sfc.2011.nc@SP:pres \
    , ~/data/wghm/dat/WGHM.nc @ WGHM \
    -G rajner@GN : 1 : 2 \
    -D 201101:1@D -V

    # specify the station: name,lat[decDeg],lon[decDeg],height[m]

# The spaces are not mandatory. The program searches for the next switch (starting with "-")
# or field separator "," ":" 
# thus the commands below are equal:

# grat -F ../file , file2: field1  :field2 , 
# grat -F../file,file2:field1:field2, 

# this is extreemly useful if one use <TAB> completion for path and filenames
