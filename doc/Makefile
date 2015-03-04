
#  man:

doxygen:
	{ cat ./Doxyfile ; echo "PROJECT_NUMBER = $$(git describe)" ; } \
	| doxygen -

html: doxygen

pdf: doxygen
	cd ./latex/ && pdflatex -shell-escape refman

