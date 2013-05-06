all: \
	spher_area.pdf spher_area.svg

spher_area.pdf: spher_area.tex
	pdflatex $<

%.svg:%.pdf
	inkscape $< --export-plain-svg=$@
