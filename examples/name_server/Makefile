.SUFFIXES: .erl .beam .yrl

MODS := $(wildcard *.erl)

%.beam: %.erl
	erlc -W $<

all: sockets.pdf beams

sockets.pdf: sockets.tex
	pdflatex sockets.tex
	pdflatex sockets.tex

beams: ${MODS:%.erl=%.beam}

clean:
	rm -rf lecture_*.tex *.aux *.log *.toc *.out
	rm -rf erl_crash.dump *.beam 

