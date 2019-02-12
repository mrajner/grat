# time grat                                    \
#   -! \
command='
  -S  wettzell  : 49 : 12
  -F  ~/dat/erainterim/sp.2014.nc @ EWT : sp @ pascal2mmwater , ~/dat/landsea/landmaskRg0.25.nc @LS
  -D m
  -M 2                                       
  -G ~/src/gotic2/data/grn1.data@GR : 1 : 2  '

 echo grat $command
 eval grat $command
 echo
 eval grat "-!"  $command

  -S  wettzell  : 49 : 12
  -F  ~/dat/erainterim/sp.2014.nc @ EWT : sp @ pascal2mmwater , ~/dat/landsea/landmaskRg0.25.nc @LS
  -D m
  -M 2                                       
  -G ~/src/gotic2/data/grn1.data@GR : 1 : 2  '
