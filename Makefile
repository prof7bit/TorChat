SUBDIRS = src/gui
SUBMAKE = $(foreach DIR, $(SUBDIRS), $(MAKE) $(MAKEFLAGS) -C $(DIR) $(1) &&) exit 0;

all:
	$(call SUBMAKE, all)
	@echo "\n\
	Everything successfully built :-)\n\
	You can now run the executable\n\
	or install with\n\
	\n\
	    $$ sudo make install\n\
	\n\
	which will install to /usr/local\n\
	or alternatively you might specify\n\
	a different prefix:\n\
	\n\
	    $$ sudo make install PREFIX=/usr\n\
	"
	
install:
	$(call SUBMAKE, install)
	
uninstall:
	$(call SUBMAKE, uninstall)
	
clean:
	$(call SUBMAKE, clean)

