make FC=ifort >/dev/null
make FC=ifort &> if

make FC=gfortran >/dev/null
make FC=gfortran &> gf

diff  if gf |colordiff
