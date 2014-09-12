make FC=ifort 1>/dev/null 2>&1 
make FC=ifort 1>if 2>&1 

make FC=gfortran 1>/dev/null 2>&1
make FC=gfortran 1>gf 2>&1


      do_not_compare_list='com\|FF\|^\(real\|user\|sys\)\|\(Unknown[[:space:]]*\)\{3\}\|Program started\|eta *[[:digit:]]\|[Ee]xecution time\|^#[[:space:]]\+v[[:digit:]]\|^#[[:space:]]\+compiled on\|^#[[:space:]]\+compiler:\|FFLAGS\|| %: *[[:digit:]]'

colordiff -I "$do_not_compare_list" if gf 
    :
    echo
    echo
colordiff -y if gf 
