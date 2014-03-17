.SUFFIXES: .erl .beam .yrl

MODS := $(wildcard *.erl)

%.beam: %.erl
	erlc -W $<

server1: beams
	@erl -noshell -s server1 start	

server2: beams
	@erl -noshell -s server2 start	

server3: beams
	@erl -noshell -s server3 start	


client1: beams
	@erl -noshell -s client1 test -s init stop

client2: beams
	@erl -noshell -s client2 test -s init stop

client3: beams
	@erl -noshell -s client3 test -s init stop



beams: ${MODS:%.erl=%.beam}

clean:
	rm -rf *.beam *.aux *.log *.pdf *.out *.toc erl_crash.dump *~





