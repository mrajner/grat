
mask=~/pub/onsala/baltic/atml/CMEMSmask.nc
mask2=mask2.nc
grdmath $mask 0 DENAN = $mask2

echo  exclamation
$G/bin/value_check -! -F $mask  : sla  -Sj , os, r, o, o:57:11 -wn 
echo
echo  normal
$G/bin/value_check   -F $mask  : sla  -Sj , os , r, o, o:57:11 -wn

echo
echo mask2
echo  exclamation
$G/bin/value_check -! -F $mask2 -Sj , os, r, o, o:57:11 -wn 
echo
echo  normal
$G/bin/value_check   -F $mask2 -Sj , os , r, o, o:57:11 -wn
