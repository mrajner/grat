!./mpc.sh
plot 'tmp' us ($17-$9-4) w l,\
 'tmp' us ($25-$9-4) w l, \
 'tmp' us ($25-$24-4) w l, \
 './point_pot_cyl.dat' us ($17-$9) w l, \
 './point_pot_cyl.dat' us ($25-$9) w l, \
 './point_pot_cyl.dat' us ($25-$24) w l


pause -1
