PREFIX=/usr
PLUGINDIR=$(PREFIX)/lib/purple-2
COMPILEDIR=../../bin

FPCOPT = \
	-Fu../client \
	-Fu../client/lnet-0.6.5 \
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
	
install:
	mkdir -p $(PLUGINDIR)
	install --mode=0644 $(COMPILEDIR)/libpurpletorchat.so $(PLUGINDIR)

clean:
	$(MAKE) -C ../client clean
	$(RM) -r lib


## this is only for making the preview
## binary that I upload to github
demo:
	mkdir -p lib
	fpc $(FPCOPT) -dUseHeapTrc -dDebugToConsole -dDebugToFile purpletorchat.lpr

