FILES    := $(shell ls *.[0-9].txt)
MANPAGES := $(FILES:.txt=)

A2X := a2x --format=manpage

all: $(MANPAGES) install

%: %.txt
	$(A2X) $<

MANDIR := $$HOME/.local/share/man/

install: $(MANPAGES)
	echo $(MANPAGES)
	echo $<

ki:
	#  $(MANDIR)
	#  cp -uv $(MANPAGES) -t $(MANDIR)

#  $(MANDIR):
	#  mkdir -p $@
