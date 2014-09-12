time {
make -j 1 FC=ifort 1>/dev/null 2>&1 
make FC=ifort  2>&1  | tee if >/dev/null

make -j 1 FC=gfortran 1>/dev/null 2>&1
make FC=gfortran  2>&1  | tee gf >/dev/null
}


      do_not_compare_list='com\|FF\|^\(real\|user\|sys\)\|\(Unknown[[:space:]]*\)\{3\}\|Program started\|eta *[[:digit:]]\|[Ee]xecution time\|^#[[:space:]]\+v[[:digit:]]\|^#[[:space:]]\+compiled on\|^#[[:space:]]\+compiler:\|FFLAGS\|| %: *[[:digit:]]'

colordiff -I "$do_not_compare_list" if gf 
    :
    echo
    echo
colordiff -y if gf 

