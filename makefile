#######################################################################
# Generic make script for compiling erlang code                       #
# The environment variable $ERLHOME has to be set to where erlang/OTP #
# is installed                                                        #
# Compiles the code into a ebin dir. relative to the source dir.      #
# (../ebin)                                                           #
#######################################################################
SOURCE = src/main.erl \
	 src/bencode.erl \
	 src/storage.erl \
	 src/tests/bencode_tests.erl \
	 src/tests/all_tests.erl 

#Where include files are stored ".hrl"
EFLAGS = -I include \


#Compiles the code into a ebin dir. relative to the root directory
EBIN = ebin
ERL = erl
GEN = beam
ERLC_EMULATOR = erl -boot start_clean
ERLHOME=/usr
PATH= .:$(ERLHOME)/bin:/bin:/usr/bin:/usr/local/bin
TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)

CODE = $(SOURCE:%.erl=$(EBIN)/%.beam) 

$(EBIN)/%.beam: %.erl
	$(ERLHOME)/bin/erlc  -W -b beam -o $(EBIN) $(EFLAGS) $(WAIT) $<



all:  $(TARGETS)


clean:
	rm -f $(CODE)
cleanall:
	rm -f `find . -type f -name "*.beam" -o -name "*.dump"`

