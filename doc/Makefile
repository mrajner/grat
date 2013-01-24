all: mapa1.pdf\
	   mapa2.pdf\
		 mapa3.pdf\
     mapa1.png\
	   mapa2.png\
		 mapa3.png\
		 interpolation_ilustration.pdf \
		 interpolation_ilustration.png \
		 helpfiles

mapa1.pdf mapa2.pdf mapa3.pdf: ./polygon_ilustration.sh
	bash $<

%.png:%.pdf
	convert  $< -resize 30% $@

interpolation_ilustration.png:interpolation_ilustration.pdf
	convert $< $@

interpolation_ilustration.pdf: interpolation_ilustration.tex interpb1.dat interpl1.dat interpn1.dat 
	pdflatex -shell-escape $<
	latexmk -c 

interpb1.dat interpl1.dat interpn1.dat:  interpolation_ilustration.sh
	bash $<

helpfiles: grat.hlp polygon_check.hlp value_check.hlp

%.hlp: ~/src/grat/bin/$(@:.hlp=)
	$(@:.hlp=) -h > $@

clean:
	rm -f ps2raster*
	rm -f mapa?.points
	rm -f interp{b,l,n}1.dat *.ps
