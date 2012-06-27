MPURPLE = $(MAKE) -C src/purple
MGUI = $(MAKE) -C src/gui
MCORE = $(MAKE) -C src/core
MTOOLS = $(MAKE) -C src/tools
MBIN = $(MAKE) -C bin

all: purple

install: installpurple

purple:
	$(MPURPLE) all OPT="-O- -gl"
	
gui:
	$(MGUI) all

installpurple:
	$(MPURPLE) install
	
installgui:
	$(MGUI) install
		
clean:
	$(MCORE) clean
	$(MPURPLE) clean
	$(MGUI) clean
	$(MTOOLS) clean
	$(MBIN) clean


## this is only for myself for making the
## archives that I upload to github
demozip: clean
	$(MPURPLE) demo
	$(MTOOLS) all
	$(MTOOLS) zip

## this one is for my friendly plugin tester
update: clean pull purple demoinstall
	
pull:
	git pull
	
demoinstall:
	install bin/libpurpletorchat.so ~/.purple/plugins

demosymlink:
	ln -fs `readlink -f bin/libpurpletorchat.so` ~/.purple/plugins/libpurpletorchat.so
