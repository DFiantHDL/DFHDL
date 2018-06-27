BIBTEX=bibtex
PDFLATEX=pdflatex

PDFARGS=-synctex=1 -interaction=nonstopmode

BIBFILES=

TARGET=dfiant_samos2018

SRCS=$(wildcard *.tex)

pdf: $(SRCS)
	$(PDFLATEX) $(PDFARGS) $(TARGET).tex

$(TARGET).bbl: $(TARGET).tex $(TARGET).aux $(BIBFILES)
	$(BIBTEX) $(TARGET)

bib:
	bibtex $(TARGET)

tar:
	$(RM) /tmp/$(TARGET).tgz
	tar -cvz --exclude=.svn '--exclude=*~' -f /tmp/$(TARGET).tgz .
	mv /tmp/$(TARGET).tgz .

clean:
	$(RM) *~ $(TARGET).aux $(TARGET).log $(TARGET).ptc $(TARGET).run.xml $(TARGET).idx $(TARGET).bcf $(TARGET).synctex.gz $(TARGET).dvi $(TARGET).blg $(TARGET).bbl *.aux $(TARGET).pdf \
	$(TARGET).toc $(TARGET).lof $(TARGET).lot



