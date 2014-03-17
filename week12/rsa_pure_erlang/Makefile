.SUFFIXES: .erl .beam .yrl

MODS := $(wildcard *.erl)

%.beam: %.erl
	erlc -W $<

test: beams
	erl -s rsa_test test	

beams: ${MODS:%.erl=%.beam}


clean:
	rm -rf *.pri *.pub *.beam *.aux *.log *.pdf *.out *.toc erl_crash.dump *~





