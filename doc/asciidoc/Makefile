# FILES     := $(shell ls **/*.[0-9].txt)
# MANPAGES  := $(FILES:.txt=)
# HTMLPAGES := $(FILES:.txt=.html)

ASCIIDOC := asciidoctor

all:
	$(ASCIIDOC) -b manpage ./grat.adoc
	man ./grat.3
# all: info.dat $(MANPAGES) $(HTMLPAGES)

# info.dat:
# 	echo ":Revision:" $$(git describe ) > $@
# 	echo ":My date out:" $$(grat --date) >> $@
# 	echo ":Date:" $$(grat --date) >> $@
# 	echo ":grat:" $$(grat --date) >> $@

# %: %.txt
# 	$(A2X) --format=manpage $<

# %.html: %.txt
# 	$(A2X) --format=xhtml $<
