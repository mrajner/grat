mod_utilities.o mod_utilities.mod: mod_constants.o mod_constants.mod
mod_atmosphere.o mod_atmosphere.mod: mod_constants.o mod_constants.mod mod_printing.o mod_printing.mod
mod_cmdline.o mod_cmdline.mod: mod_constants.o mod_constants.mod mod_utilities.o mod_utilities.mod
mod_constants.o mod_constants.mod:
mod_data.o mod_data.mod: mod_mjd.o mod_mjd.mod mod_atmosphere.o mod_atmosphere.mod mod_constants.o mod_constants.mod mod_polygon.o mod_polygon.mod mod_cmdline.o mod_cmdline.mod mod_printing.o mod_printing.mod mod_utilities.o mod_utilities.mod
mod_date.o mod_date.mod: mod_mjd.o mod_mjd.mod mod_constants.o mod_constants.mod mod_cmdline.o mod_cmdline.mod mod_data.o mod_data.mod mod_printing.o mod_printing.mod mod_utilities.o mod_utilities.mod
mod_green.o mod_green.mod: mod_atmosphere.o mod_atmosphere.mod mod_normalization.o mod_normalization.mod mod_date.o mod_date.mod mod_site.o mod_site.mod mod_constants.o mod_constants.mod mod_3d.o mod_3d.mod mod_polygon.o mod_polygon.mod mod_cmdline.o mod_cmdline.mod mod_aggf.o mod_aggf.mod mod_data.o mod_data.mod mod_printing.o mod_printing.mod mod_spherical.o mod_spherical.mod mod_utilities.o mod_utilities.mod
mod_mjd.o mod_mjd.mod: mod_constants.o mod_constants.mod
mod_normalization.o mod_normalization.mod: mod_constants.o mod_constants.mod mod_utilities.o mod_utilities.mod
mod_parser.o mod_parser.mod: mod_admit.o mod_admit.mod mod_date.o mod_date.mod mod_green.o mod_green.mod mod_site.o mod_site.mod mod_constants.o mod_constants.mod mod_polygon.o mod_polygon.mod mod_cmdline.o mod_cmdline.mod mod_data.o mod_data.mod mod_printing.o mod_printing.mod mod_utilities.o mod_utilities.mod
mod_polygon.o mod_polygon.mod: mod_constants.o mod_constants.mod mod_cmdline.o mod_cmdline.mod mod_printing.o mod_printing.mod mod_utilities.o mod_utilities.mod
mod_printing.o mod_printing.mod: mod_constants.o mod_constants.mod mod_cmdline.o mod_cmdline.mod
mod_3d.o mod_3d.mod: mod_constants.o mod_constants.mod
mod_site.o mod_site.mod: mod_date.o mod_date.mod mod_constants.o mod_constants.mod mod_cmdline.o mod_cmdline.mod mod_data.o mod_data.mod mod_printing.o mod_printing.mod mod_utilities.o mod_utilities.mod
mod_admit.o mod_admit.mod: mod_atmosphere.o mod_atmosphere.mod mod_site.o mod_site.mod mod_constants.o mod_constants.mod mod_cmdline.o mod_cmdline.mod mod_printing.o mod_printing.mod mod_data.o mod_data.mod mod_utilities.o mod_utilities.mod
mod_spherical.o mod_spherical.mod: mod_constants.o mod_constants.mod
mod_aggf.o mod_aggf.mod: mod_normalization.o mod_normalization.mod mod_atmosphere.o mod_atmosphere.mod mod_constants.o mod_constants.mod mod_utilities.o mod_utilities.mod
