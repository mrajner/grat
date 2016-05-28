$(FC)/mod_utilities$(SUFFIX).o: $(FC)/mod_constants$(SUFFIX).o
$(FC)/mod_atmosphere$(SUFFIX).o: $(FC)/mod_printing$(SUFFIX).o $(FC)/mod_constants$(SUFFIX).o
$(FC)/mod_cmdline$(SUFFIX).o: $(FC)/mod_constants$(SUFFIX).o $(FC)/mod_utilities$(SUFFIX).o
$(FC)/mod_constants$(SUFFIX).o:
$(FC)/mod_data$(SUFFIX).o: $(FC)/mod_printing$(SUFFIX).o $(FC)/mod_cmdline$(SUFFIX).o $(FC)/mod_mjd$(SUFFIX).o $(FC)/mod_constants$(SUFFIX).o $(FC)/mod_polygon$(SUFFIX).o $(FC)/mod_atmosphere$(SUFFIX).o $(FC)/mod_utilities$(SUFFIX).o
$(FC)/mod_date$(SUFFIX).o: $(FC)/mod_cmdline$(SUFFIX).o $(FC)/mod_printing$(SUFFIX).o $(FC)/mod_mjd$(SUFFIX).o $(FC)/mod_constants$(SUFFIX).o $(FC)/mod_data$(SUFFIX).o $(FC)/mod_utilities$(SUFFIX).o
$(FC)/mod_green$(SUFFIX).o: $(FC)/mod_normalization$(SUFFIX).o $(FC)/mod_printing$(SUFFIX).o $(FC)/mod_cmdline$(SUFFIX).o $(FC)/mod_constants$(SUFFIX).o $(FC)/mod_3d$(SUFFIX).o $(FC)/mod_polygon$(SUFFIX).o $(FC)/mod_aggf$(SUFFIX).o $(FC)/mod_data$(SUFFIX).o $(FC)/mod_atmosphere$(SUFFIX).o $(FC)/mod_spherical$(SUFFIX).o $(FC)/mod_utilities$(SUFFIX).o $(FC)/mod_site$(SUFFIX).o
$(FC)/mod_mjd$(SUFFIX).o: $(FC)/mod_constants$(SUFFIX).o
$(FC)/mod_normalization$(SUFFIX).o: $(FC)/mod_constants$(SUFFIX).o $(FC)/mod_utilities$(SUFFIX).o
$(FC)/mod_parser$(SUFFIX).o: $(FC)/mod_cmdline$(SUFFIX).o $(FC)/mod_printing$(SUFFIX).o $(FC)/mod_constants$(SUFFIX).o $(FC)/mod_admit$(SUFFIX).o $(FC)/mod_polygon$(SUFFIX).o $(FC)/mod_data$(SUFFIX).o $(FC)/mod_utilities$(SUFFIX).o $(FC)/mod_green$(SUFFIX).o $(FC)/mod_date$(SUFFIX).o $(FC)/mod_site$(SUFFIX).o
$(FC)/mod_polygon$(SUFFIX).o: $(FC)/mod_cmdline$(SUFFIX).o $(FC)/mod_printing$(SUFFIX).o $(FC)/mod_constants$(SUFFIX).o $(FC)/mod_utilities$(SUFFIX).o
$(FC)/mod_printing$(SUFFIX).o: $(FC)/mod_cmdline$(SUFFIX).o $(FC)/mod_constants$(SUFFIX).o
$(FC)/mod_3d$(SUFFIX).o: $(FC)/mod_constants$(SUFFIX).o
$(FC)/mod_site$(SUFFIX).o: $(FC)/mod_cmdline$(SUFFIX).o $(FC)/mod_printing$(SUFFIX).o $(FC)/mod_constants$(SUFFIX).o $(FC)/mod_data$(SUFFIX).o $(FC)/mod_utilities$(SUFFIX).o $(FC)/mod_date$(SUFFIX).o
$(FC)/mod_admit$(SUFFIX).o: $(FC)/mod_printing$(SUFFIX).o $(FC)/mod_cmdline$(SUFFIX).o $(FC)/mod_constants$(SUFFIX).o $(FC)/mod_data$(SUFFIX).o $(FC)/mod_atmosphere$(SUFFIX).o $(FC)/mod_utilities$(SUFFIX).o $(FC)/mod_site$(SUFFIX).o
$(FC)/mod_spherical$(SUFFIX).o: $(FC)/mod_constants$(SUFFIX).o
$(FC)/mod_aggf$(SUFFIX).o: $(FC)/mod_printing$(SUFFIX).o $(FC)/mod_normalization$(SUFFIX).o $(FC)/mod_constants$(SUFFIX).o $(FC)/mod_atmosphere$(SUFFIX).o $(FC)/mod_utilities$(SUFFIX).o
