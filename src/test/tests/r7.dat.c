########################################################################
#                                                          value_check #
#                                                     v1.0-28-gbf592b5 #
########################################################################
#                                                           compiler:  #
#                                                      compiled on pw  #
#                                                            FFLAGS =  #
########################################################################
#                                Copyright 2013, 2014 by Marcin Rajner #
#                                      Warsaw University of Technology #
#                                             License: GPL v3 or later #
########################################################################

#-----------------------------------------------------------------------
Command invoked:
value_check -S j -o /dev/null -L/dev/null@p, /tmp/f1@s , /tmp/f2@u , /tm
              p/f3@c, /dev/null@p, /tmp/f1@s , /tmp/f2@u , /tmp/fff@c -V
#-----------------------------------------------------------------------
Command parsing:
  -S { -Sj }
    j
  -o { null }
    output file was set: null
  -L { fff@c }
    /dev/null <- points                                  
    /tmp/f1 <- summary                                 
    /tmp/f2 <- unknown                                 
    /tmp/f3 <- unknown                                 
    /dev/null <- points                                  
    /tmp/f1 <- summary                                 
    /tmp/f2 <- unknown                                 
    /tmp/fff <- unknown                                 
  -V { -V }
    verbose mode
#-----------------------------------------------------------------------

      Name lat [deg] lon [deg]     H [m]    Hp [m]    H* [m]   Hrsp[m]
    joze_a   52.0000   21.0000  110.0000        --        --        -- 
#-----------------------------------------------------------------------
 /dev/null /dev/null /tmp/f1 /tmp/f2 /tmp/f3 /dev/null /tmp/f1 /tmp/f2 /tmp/fff 

#-----------------------------------------------------------------------
  joze_a   52.000E+00   21.000E+00    2.500E-06    0.000E+00   52.000E+00   21.000E+00  212.357E-03  212.357E-03    0.000E+00  101.32E+03 
  joze_a   52.000E+00   21.000E+00    2.500E-06   50.000E+00   52.000E+00   21.000E+00  212.357E-03  424.714E-03    0.000E+00  101.32E+03 
  joze_a   52.000E+00   21.000E+00    2.500E-06  100.000E+00   52.000E+00   21.000E+00  212.357E-03  637.071E-03    0.000E+00  101.32E+03 
  joze_a   52.000E+00   21.000E+00    2.500E-06  150.000E+00   52.000E+00   21.000E+00  212.357E-03  849.428E-03    0.000E+00  101.32E+03 
  joze_a   52.000E+00   21.000E+00    2.500E-06  200.000E+00   52.000E+00   21.000E+00  212.357E-03    1.062E+00    0.000E+00  101.32E+03 
  joze_a   52.000E+00   21.000E+00    2.500E-06  250.000E+00   52.000E+00   21.000E+00  212.357E-03    1.274E+00    0.000E+00  101.32E+03 
  joze_a   52.000E+00   21.000E+00    2.500E-06  300.000E+00   52.000E+00   21.000E+00  212.357E-03    1.486E+00    0.000E+00  101.32E+03 
  joze_a   52.000E+00   21.000E+00   10.000E-06    0.000E+00   52.000E+00   21.000E+00  304.321E+06  304.321E+06   -2.246E+06  101.32E+03 
  joze_a   52.000E+00   21.000E+00   10.000E-06   50.000E+00   52.000E+00   21.000E+00  304.321E+06  608.642E+06   -4.492E+06  101.32E+03 
  joze_a   52.000E+00   21.000E+00   10.000E-06  100.000E+00   52.000E+00   21.000E+00  304.321E+06  912.964E+06   -6.738E+06  101.32E+03 
  joze_a   52.000E+00   21.000E+00   10.000E-06  150.000E+00   52.000E+00   21.000E+00  304.321E+06    1.217E+09   -8.984E+06  101.32E+03 
  joze_a   52.000E+00   21.000E+00   10.000E-06  200.000E+00   52.000E+00   21.000E+00  304.321E+06    1.522E+09  -11.230E+06  101.32E+03 
  joze_a   52.000E+00   21.000E+00   10.000E-06  250.000E+00   52.000E+00   21.000E+00  304.321E+06    1.826E+09  -13.476E+06  101.32E+03 
  joze_a   52.000E+00   21.000E+00   10.000E-06  300.000E+00   52.000E+00   21.000E+00  304.321E+06    2.130E+09  -15.722E+06  101.32E+03 
  joze_a   52.000E+00   21.000E+00  475.000E-03    0.000E+00   52.475E+00   21.000E+00  143.903E+09  146.034E+09  -15.723E+06  101.32E+03 
  joze_a   52.000E+00   21.000E+00  475.000E-03   50.000E+00   52.304E+00   21.595E+00  143.903E+09  289.937E+09  -15.723E+06  101.32E+03 
  joze_a   52.000E+00   21.000E+00  475.000E-03  100.000E+00   51.915E+00   21.758E+00  143.903E+09  433.840E+09  -15.723E+06  101.32E+03 
  joze_a   52.000E+00   21.000E+00  475.000E-03  150.000E+00   51.588E+00   21.382E+00  143.903E+09  577.744E+09  -15.724E+06  101.32E+03 
  joze_a   52.000E+00   21.000E+00  475.000E-03  200.000E+00   51.553E+00   20.739E+00  143.903E+09  721.647E+09  -15.724E+06  101.32E+03 
  joze_a   52.000E+00   21.000E+00  475.000E-03  250.000E+00   51.835E+00   20.278E+00  143.903E+09  865.551E+09  -15.725E+06  101.32E+03 
  joze_a   52.000E+00   21.000E+00  475.000E-03  300.000E+00   52.236E+00   20.328E+00  143.903E+09    1.009E+12  -15.725E+06  101.32E+03 
  joze_a   52.000E+00   21.000E+00    9.869E+00    0.000E+00   61.869E+00   21.000E+00  763.634E+09    1.773E+12  -15.725E+06  101.32E+03 
  joze_a   52.000E+00   21.000E+00    9.869E+00   50.000E+00   57.584E+00   35.177E+00  763.634E+09    2.537E+12  -15.725E+06  101.32E+03 
  joze_a   52.000E+00   21.000E+00    9.869E+00  100.000E+00   49.291E+00   35.998E+00  763.634E+09    3.300E+12  -15.725E+06  101.32E+03 
  joze_a   52.000E+00   21.000E+00    9.869E+00  150.000E+00   43.233E+00   27.755E+00  763.634E+09    4.064E+12  -15.725E+06  101.32E+03 
  joze_a   52.000E+00   21.000E+00    9.869E+00  200.000E+00   42.625E+00   16.431E+00  763.634E+09    4.828E+12  -15.725E+06  101.32E+03 
  joze_a   52.000E+00   21.000E+00    9.869E+00  250.000E+00   47.754E+00    7.140E+00  763.634E+09    5.591E+12  -15.725E+06  101.32E+03 
  joze_a   52.000E+00   21.000E+00    9.869E+00  300.000E+00   56.007E+00    5.604E+00  763.634E+09    6.355E+12  -15.725E+06  101.32E+03 
100% |********************|  0.0s [eta  0.0] (proc:  0.0 | %: 93.0) /dev/null  
