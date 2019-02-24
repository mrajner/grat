grat.o : grat.f90 mod_admit.o mod_cmdline.o mod_site.o mod_green.o mod_date.o mod_data.o mod_parser.o 
mod_3d.o : mod_3d.f90 
mod_admit.o : mod_admit.f90 mod_printing.o mod_site.o mod_data.o mod_cmdline.o 
mod_aggf.o : mod_aggf.f90 mod_printing.o mod_normalization.o 
mod_cmdline.o : mod_cmdline.f90 
mod_data.o : mod_data.f90 mod_polygon.o mod_printing.o mod_cmdline.o 
mod_date.o : mod_date.f90 mod_data.o mod_cmdline.o mod_printing.o 
mod_green.o : mod_green.f90 mod_3d.o mod_polygon.o mod_data.o mod_site.o mod_aggf.o mod_normalization.o mod_printing.o mod_cmdline.o 
mod_normalization.o : mod_normalization.f90 
mod_parser.o : mod_parser.f90 mod_admit.o mod_green.o mod_data.o mod_polygon.o mod_date.o mod_site.o mod_cmdline.o mod_printing.o 
mod_polygon.o : mod_polygon.f90 mod_cmdline.o mod_printing.o 
mod_printing.o : mod_printing.f90 mod_cmdline.o 
mod_site.o : mod_site.f90 mod_date.o mod_data.o mod_cmdline.o mod_printing.o 
polygon_check.o : polygon_check.f90 mod_site.o mod_parser.o mod_polygon.o 
value_check.o : value_check.f90 mod_polygon.o mod_site.o mod_date.o mod_data.o mod_parser.o mod_cmdline.o 
