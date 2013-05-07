all: \
	spher_area.pdf spher_area.svg \
	podklad.pdf podklad2.pdf \
	polygon_ilustration.pdf polygon_ilustration.png\
	interpolation_ilustration.pdf\
	interpolation_ilustration.svg

spher_area.pdf: spher_area.tex
	pdflatex $<

%.svg:%.pdf
	inkscape $< --export-plain-svg=$@ --export-text-to-path -w 50
%.png:%.pdf
	convert -resize 900 $< $@

podklad.pdf podklad2.pdf: podklad_do_convolution.sh
	bash $^


other: 
		interpolation_ilustration.pdf \
		interpolation_ilustration.png 

mapa1.pdf mapa2.pdf mapa3.pdf: ./polygon_ilustration.sh
	bash $<

interpolation_ilustration.pdf: interpolation_ilustration.tex
	bash interpolation_ilustration.sh
	pdflatex -shell-escape $<


clean:
	rm -f ps2raster*
	rm -f mapa?.points *.cpt
	rm -f mapa*{pdf,svg,png}
	rm -f interp{b,l,n}1.dat *.ps
	rm -f podklad*.{grd,dat}
	latexmk -c -quiet
