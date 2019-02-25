correct handling of FillValue

automatically select variables if unambiogus: give information in log

value_check -F file.nc -L@d - should give dates even if no variable found:
e.g. value_check -F swvl_201602_1_era5.nc -L@d gives error

2016.02.17 unnecesary computation, e.g. grat -Sj,l,m -FERA@H liczy wysokość 
oddzielnie dla każdej stacji z modelu H i przelicza wielokrotnie. 

BUG? era sp2014 size? and datalisting trough grat

dodać odczytywanie lsdm days since....

* -C option to center data files
* variable modifer call only if necessary, do not correct all fields
* move RSP to HRSP in 2D? (twice, on terrain GE, and site GN)
* latitude gravity dependence Merriam s. 491

    te dwa niżej tylko do value_check? czy może do całego grata zastosować
    20181009 do not read whole fields when -! is in use
    20181009 use -! by default when -In@I - if interpolation is nearest than why bother to get other data points

    połączyć grat i value_check i polygon_check w jedno (uuu, duża sprawa -- przy zachowaniu kompabitylności wstecznej)

    rozdzielić testy i grat do różnych repozytoriów (zrób testy jako submodule)
