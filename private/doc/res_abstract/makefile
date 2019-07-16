# Makefile to build PDF from tex

DOC_NAME := ResearchAbstract-OronPort

all: $(DOC_NAME).pdf

$(DOC_NAME).pdf: abstract.tex
	pdflatex -jobname $(DOC_NAME) $^

.PHONY: clean
clean:
	rm -f $(wildcard $(DOC_NAME).*) $(filter-out abstract.tex abstract.bib,$(wildcard abstract.*))

