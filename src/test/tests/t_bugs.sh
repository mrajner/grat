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
  # should ignore not_starting_with_dash 
  # but treat all after it as one parameter
  # i.e. not -S given error
value_check not_starting_with_dash -Sj -F10
let counter++ 


# 2014.09.02
  # FIXED 329259ae88ccc8c5b9cb241bf5d43c9a14920308
value_check -F 10@SP -Sj -D 2010@~ > t_bugs.dat${counter}

touch t_bugs.dat


