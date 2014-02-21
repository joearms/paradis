.SUFFIXES: .erl .beam .yrl

MODS := $(wildcard *.erl)
ERLC = "/Users/joe/nobackup/otp_src_R16B/bin/erlc"
%.beam: %.erl
	${ERLC} -W $<

all: week4_problems.pdf beams
	./make_slides f2-f3.org



week4_problems.pdf: week4_problems.tex
	pdflatex week4_problems.tex
	pdflatex week4_problems.tex

beams: ${MODS:%.erl=%.beam}

clean:
	rm -rf *.beam *.aux *.log *.pdf *.out *.toc




