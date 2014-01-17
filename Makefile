.SUFFIXES: .erl .beam .yrl

MODS := $(wildcard *.erl)

%.beam: %.erl
	erlc -W $<

all: week4_problems.pdf beams
	./make_slides f2-f3.org



week4_problems.pdf: week4_problems.tex
	pdflatex week4_problems.tex
	pdflatex week4_problems.tex

beams: ${MODS:%.erl=%.beam}

clean:
	rm -rf *.beam *.aux *.log *.pdf *.out *.toc




