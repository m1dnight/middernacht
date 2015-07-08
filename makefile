#######################################################################
# Generic make script for compiling erlang code                       #
# The environment variable $ERLHOME has to be set to where erlang/OTP #
# is installed                                                        #
# Compiles the code into a ebin dir. relative to the source dir.      #
# (../ebin)                                                           #
#######################################################################
SOURCE = tracker/main.erl \
	 bencoding/bencode.erl \
	 tests/bencode_tests.erl \
	 tests/all_tests.erl \
	 tests/tracker_test.erl


#Compiles the code into a ebin dir. relative to the source dir. 
EBIN = ebin
ERL = erl
GEN = beam
ERLC_EMULATOR = erl -boot start_clean
ERLHOME=/usr
PATH= .:$(ERLHOME)/bin:/bin:/usr/bin:/usr/local/bin
TARGETS = $(SOURCE:%.erl=$(EBIN)/%.beam)

CODE = $(SOURCE:%.erl=$(EBIN)/%.beam) 

$(EBIN)/%.beam: %.erl
	$(ERLHOME)/bin/erlc  -W -b beam -o $(EBIN) $(WAIT) $<



all: $(TARGETS)



clean:
	rm -f `find . -type f -name "*.beam" -o -name "*.dump"`

