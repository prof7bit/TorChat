PREFIX=/usr
PLUGINDIR=$(PREFIX)/lib/purple-2
COMPILEDIR=../../bin

FPCOPT = \
	-Fu../client \
	-FU./lib \
	-FE../../bin \
	-g- \
	-CX \
	-XX \
	-Xs \
	-O3 \
	$(OPT) \

all:
	mkdir -p lib
	fpc $(FPCOPT) purpletorchat.lpr
	
foo:
	mkdir -p lib
	fpc $(FPCOPT) -dNoOutputRedirect -dUseHeapTrc purpletorchat.lpr

install:
	mkdir -p $(PLUGINDIR)
	install --mode=0644 $(COMPILEDIR)/libpurpletorchat.so $(PLUGINDIR)

clean:
	$(MAKE) -C ../client clean
	$(RM) -r lib
			