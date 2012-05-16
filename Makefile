MPURPLE = $(MAKE) -C src/purple
MGUI = $(MAKE) -C src/gui

all: purple

install: installpurple installgui

purple:
	$(MPURPLE) all
	
gui:
	$(MGUI) all

installpurple:
	$(MPURPLE) install
	
installgui:
	$(MGUI) install
		
clean:
	$(MPURPLE) clean
	$(MGUI) clean
	$(RM) bin/*.dll 
	$(RM) bin/*.so
	$(RM) bin/torchat
	$(RM) bin/torchat.exe 
	$(RM) bin/*.map 
	
