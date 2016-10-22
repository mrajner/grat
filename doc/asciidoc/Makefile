FILES    := $(shell ls *.[0-9].txt)
MANPAGES := $(FILES:.txt=)
HTMLPAGES := $(FILES:.txt=.html)

A2X := a2x 

all: info.dat $(MANPAGES) $(HTMLPAGES)
	#  install

info.dat:
	echo ":Revision:" $$(git describe --tags ) > $@
	echo ":My date out:" $$(grat --date) >> $@
	echo ":Date:" $$(grat --date) >> $@
	echo ":grat:" $$(grat --date) >> $@

%: %.txt
	$(A2X) --format=manpage $<

%.html: %.txt
	$(A2X) --format=xhtml $<

MANDIR := $${HOME}/.local/share/man/

install: $(MANPAGES)
	mkdir -p $(MANDIR)
	echo $<
	cp -uv $(MANPAGES) -t $(MANDIR)

#  $(MANDIR):
	#  mkdir -p $@
