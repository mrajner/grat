make FC=ifort >/dev/null
make FC=ifort &> if

make FC=gfortran >/dev/null
make FC=gfortran &> gf

      do_not_compare_list='com\|FF\|^\(real\|user\|sys\)\|\(Unknown[[:space:]]*\)\{3\}\|Program started\|eta *[[:digit:]]\|[Ee]xecution time\|^#[[:space:]]\+v[[:digit:]]\|^#[[:space:]]\+compiled on\|^#[[:space:]]\+compiler:\|FFLAGS\|| %: *[[:digit:]]'

colordiff -I "$do_not_compare_list" if gf 
    :
