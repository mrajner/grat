#!/bin/bash - 
## \file
## \brief Make map of polygon(s)
##
## This scrips 
## \author Marcin Rajner
## \date 03.11.2012
## This scripts need GMT to be installed \cite Wessel98
## The \c .pdf suffix will be given for output file
#===============================================================================


## If there are no command line argument then stop with error
: ${1?"Try: $0 -h"}

## This function read in polygon file and return informations for plot
get_information(){

  ## Get the number of polygons
  number_of_polygons=$(cat $FILE | grep -v "#" | awk 'NR==1{ print $_ + 0 }'  )
  last=2

  ## initialize counter
  count=0
  ## loop over all polygons
  while [ $count -lt $number_of_polygons ]
  do

    ## Get the number of polygon points and the polygon action (incl/excl)
    ## and save in the array
    number_of_points=(${number_of_points[*]} $(cat $FILE | grep -v "#" | awk "NR==${last} { print \$_ + 0 }"))
    if_include=(${if_include[*]} $(cat $FILE | grep -v "#" | awk "NR==$(($last+1)){ print \$_}"))
    let last=$last+${number_of_points[$count]}+2
    let count++
  done
}

usage()
{
DESCRIPTION="This program generate the map of polygon. It requires Generic Mapping Tools command available"
echo "
usage: $0 options
 
$DESCRIPTION
 
OPTIONS:
   -h      Show this message
   -v      Verbose (default no verbose)
   -d      Debug (set debugging mode)
   -f      file [required]
   -R      GMT specific range (e.g. -R10/30/30/50)
   -o      output file
"
}
 
VERBOSE=
DEBUG=
FILE=
OTHERARGS=

while getopts "vhdR:f:o:p:" flag
do
  case "$flag" in
    f) FILE="$OPTARG";echo $FILE ;;
    d) set -x ; DEBUG=true;;
    v) VERBOSE=true ;;
    h) usage ; exit ;;
    R) R="-R$OPTARG" ;;
    o) output="$OPTARG" ;;
    p) POINTSFILE="$OPTARG" ;;
  esac
#  echo "$flag" $OPTIND $OPTARG
done

if [ -z $FILE ]; then
  shift $((OPTIND-1))
  OTHERARGS="$@"
fi

echo $FILE

# todo ! from command line
if [ -z $FILE ] || [ -z $output ] ; then
  echo "Not enough cmd line parameters... , try $0 -h"
  exit
fi



if [ -n "$VERBOSE" ] ; then
  echo "you set the verbose: $VERBOSE"
  echo "not recognized parameters args $OTHERARGS"
fi
echo "creating map for: $FILE ..."

get_information $FILE
echo "Number of polygons:    " $number_of_polygons 
echo "Number of points:      " ${number_of_points[*]}
echo "Include[+]/exclude[-]: " ${if_include[*]}

#cat $FILE |grep -v "#" |nl

function get_R(){
  last=3
  R=$(for i in ${number_of_points[*]}
  do
    cat $FILE | grep -v "#" | sed -n -e $(($last+1)),$(($last+$i))p 
    last=$(($last+$i+2))
  done | minmax -C | awk '{print $1-1, $2+1, $3-1,$4+1}' | sed 's/\s/\//g')
  R=-R$R
}


if [ -z $R ]; then
  get_R
fi

A="-A999"

  gmtset FRAME_WIDTH=0.01c
#  psbasemap $R -K -JM20+ -X0 -Y0 -B100 > $output.ps
pscoast $R -Slightblue -Glightgray  -K -Di $A -J  > $output.ps
  last=3
  for i in $(seq 0 $((${#number_of_points[*]}-1)))
  do
    if [ ${if_include[$(($i))]} = "+" ]; then
      color=lightgreen
    else
      color=lightred
    fi
    cat $FILE | grep -v "#" | sed -n -e $(($last+1)),$(($last+${number_of_points[$i]}))p \
      | psxy  -R -J -K -O -A -W2p -L -G$color >> $output.ps
    last=$(($last+${number_of_points[$i]}+2))
  done 

  if [ -z $POINTSFILE ] ; then
    echo "no points file given"
  else
    makecpt -Cjet -T0.1/0.9/0.2 |sed 's/^B.*/B 200 0 0/' |sed 's/^F.*/F 0 180 0/' > points.cpt
    cat $POINTSFILE | awk "{print \$1 , \$2 ,\$(3)}" | psxy $R -J -Sc5p -Cpoints.cpt -Gred -W0.41p/gray -O -K -V  >> $output.ps
  fi


  pscoast $R -O -Di $A -J -W -N1thin >> $output.ps

ps2raster $output.ps -Tf -P -A
#evince $output.ps






exit 0
