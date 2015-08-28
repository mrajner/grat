mod_utilities$(SUFFIX).o: mod_constants$(SUFFIX).o
mod_atmosphere$(SUFFIX).o: mod_constants$(SUFFIX).o mod_printing$(SUFFIX).o
mod_cmdline$(SUFFIX).o: mod_constants$(SUFFIX).o mod_utilities$(SUFFIX).o
mod_constants$(SUFFIX).o:
mod_data$(SUFFIX).o: mod_mjd$(SUFFIX).o mod_atmosphere$(SUFFIX).o mod_constants$(SUFFIX).o mod_polygon$(SUFFIX).o mod_cmdline$(SUFFIX).o mod_printing$(SUFFIX).o mod_utilities$(SUFFIX).o
mod_date$(SUFFIX).o: mod_mjd$(SUFFIX).o mod_constants$(SUFFIX).o mod_cmdline$(SUFFIX).o mod_data$(SUFFIX).o mod_printing$(SUFFIX).o mod_utilities$(SUFFIX).o
mod_green$(SUFFIX).o: mod_atmosphere$(SUFFIX).o mod_normalization$(SUFFIX).o mod_site$(SUFFIX).o mod_constants$(SUFFIX).o mod_3d$(SUFFIX).o mod_polygon$(SUFFIX).o mod_cmdline$(SUFFIX).o mod_aggf$(SUFFIX).o mod_data$(SUFFIX).o mod_printing$(SUFFIX).o mod_spherical$(SUFFIX).o mod_utilities$(SUFFIX).o
mod_mjd$(SUFFIX).o: mod_constants$(SUFFIX).o
mod_normalization$(SUFFIX).o: mod_constants$(SUFFIX).o mod_utilities$(SUFFIX).o
mod_parser$(SUFFIX).o: mod_admit$(SUFFIX).o mod_date$(SUFFIX).o mod_green$(SUFFIX).o mod_site$(SUFFIX).o mod_constants$(SUFFIX).o mod_polygon$(SUFFIX).o mod_cmdline$(SUFFIX).o mod_data$(SUFFIX).o mod_printing$(SUFFIX).o mod_utilities$(SUFFIX).o
mod_polygon$(SUFFIX).o: mod_constants$(SUFFIX).o mod_cmdline$(SUFFIX).o mod_printing$(SUFFIX).o mod_utilities$(SUFFIX).o
mod_printing$(SUFFIX).o: mod_constants$(SUFFIX).o mod_cmdline$(SUFFIX).o
mod_3d$(SUFFIX).o: mod_constants$(SUFFIX).o
mod_site$(SUFFIX).o: mod_date$(SUFFIX).o mod_constants$(SUFFIX).o mod_cmdline$(SUFFIX).o mod_data$(SUFFIX).o mod_printing$(SUFFIX).o mod_utilities$(SUFFIX).o
mod_admit$(SUFFIX).o: mod_atmosphere$(SUFFIX).o mod_site$(SUFFIX).o mod_constants$(SUFFIX).o mod_cmdline$(SUFFIX).o mod_printing$(SUFFIX).o mod_data$(SUFFIX).o mod_utilities$(SUFFIX).o
mod_spherical$(SUFFIX).o: mod_constants$(SUFFIX).o
mod_aggf$(SUFFIX).o: mod_normalization$(SUFFIX).o mod_atmosphere$(SUFFIX).o mod_constants$(SUFFIX).o mod_printing$(SUFFIX).o mod_utilities$(SUFFIX).o
