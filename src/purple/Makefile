PREFIX=/usr
PLUGINDIR=$(PREFIX)/lib/purple-2
COMPILEDIR=../../bin

FPCOPT = \
	-Fu../core \
	-Fu../core/lnet-0.6.5 \
	-FU./lib \
	-FE../../bin \
	-g- \
	-CX \
	-XX \
	-Xs \
	-O3 \
	-k--reduce-memory-overheads \
	-k--no-keep-memory \
	$(OPT) \

all:
	mkdir -p lib
	fpc $(FPCOPT) -dDebugToConsole -dDebugToFile purpletorchat.lpr
	
install:
	mkdir -p $(PLUGINDIR)
	install --mode=0644 $(COMPILEDIR)/libpurpletorchat.so $(PLUGINDIR)

clean:
	$(MAKE) -C ../core clean
	$(RM) -r lib


## this is only for making the preview
## binaries that I upload to github
demo:
	mkdir -p lib
	fpc $(FPCOPT) -dDebugToConsole -dDebugToFile purpletorchat.lpr
	fpc -Twin32 $(FPCOPT) -dDebugToConsole -dDebugToFile purpletorchat.lpr
