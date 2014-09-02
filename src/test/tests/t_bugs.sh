#!/bin/bash - 
#===============================================================================
#          FILE: bugs.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 28.08.2014 09:54
#===============================================================================

set -o nounset 
set -o pipefail

counter=0

# 2014.09.02
# second @LS (after @GP)
value_check \
  -F ../data/test_data.nc:sp, :t,@LS:ls,@GP:gp,@LS:ls\
  -S j -D201201 : m \
  -o :level -J1000,10 -H 2>/dev/null #> t_bugs.dat${counter} 

let counter++ 

# 2014.09.02
# should ignore fsdfdsfd but treat all after it as one parameter
value_check fsdfdsfsd -Sj -F10

value_check -F 10@SP -Sj -D 2010@~

touch t_bugs.dat


