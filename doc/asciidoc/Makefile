FILES     := $(shell ls **/*.[0-9].txt)
MANPAGES  := $(FILES:.txt=)
HTMLPAGES := $(FILES:.txt=.html)

A2X := a2x

all: info.dat $(MANPAGES) $(HTMLPAGES)

info.dat:
	echo ":Revision:" $$(git describe ) > $@
	echo ":My date out:" $$(grat --date) >> $@
	echo ":Date:" $$(grat --date) >> $@
	echo ":grat:" $$(grat --date) >> $@

%: %.txt
	$(A2X) --format=manpage $<

%.html: %.txt
	$(A2X) --format=xhtml $<
