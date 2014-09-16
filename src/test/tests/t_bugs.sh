#!/bin/bash - 
#===============================================================================
#          FILE: bugs.sh
#        AUTHOR: mrajner (mrajner@gik.pw.edu.pl)
#       CREATED: 28.08.2014 09:54
#===============================================================================

set -o nounset 

counter=0


exit

# TODO co to
  grat -F 10 @SP -Sj -M1 -wn

  exit
# set NaN if value cannot be found (previousle 0)
# FIXED 14eea59d5338d987901cc44bdbd10fc8af6c792d
{
value_check -v -H
value_check -FNCEP@SP -D1777 -Sj -H
value_check -FNCEP@SP,NCEP -D2100,2014 -Sj -H -wn
} &>t_bugs.dat${counter}
let counter++ 

# 2014.09.16
# segmentation fault
# FIXED 5a582b0e0148df1b22f8f74d0ddd7d581bf53374
{
grat -D2000
value_check -D1950
} &>t_bugs.dat${counter}
let counter++ 

# 2014.09.12
# unnecessary header output when -BI,N but no @GE
# FIXED 59a6cb55654b458fcfc048253723ede06f0970a1
grat -F10@SP -M2 -G@GN -BI,N -Sj -H>t_bugs.dat${counter}
let counter++
# 2014.09.04
# problem  when -D before -S
{
value_check -Sj -D2009 -F10
} &>t_bugs.dat${counter}
let counter++ 

# 2014.09.02
# second @LS (after @GP)
value_check \
  -F ../data/test_data.nc:sp, :t,@LS:ls,@GP:gp,@LS:ls\
  -S j -D201201 : m \
  -o :level -J1000,10 -H 2>/dev/null > t_bugs.dat${counter} 
let counter++ 

# 2014.09.02
# should ignore not_starting_with_dash 
# but treat all after it as one parameter
# i.e. not -S given error
# FIXED 40927a5342cb05872bd9e063ddd9ed3edb235499
{
  value_check -starting_with_dash -Sj -F10
  value_check not_starting_with_dash -Sj -F10
	grat -starting_with_dash -Sj -F10@SP
	grat not_starting_with_dash -Sj -F10@SP
} &>t_bugs.dat${counter}
let counter++ 

# 2014.09.02
# FIXED 329259ae88ccc8c5b9cb241bf5d43c9a14920308
value_check -F 10@SP -Sj -D 2010@~ > t_bugs.dat${counter}
let counter++ 

touch t_bugs.dat


