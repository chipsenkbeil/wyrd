# Wyrd documentation makefile
all:	manual.pdf manual.html post-build-cleanup wyrd.1 wyrdrc.5

src/manual.ps: dvi
	dvips -tletterSize -Ppdf -G0 -o $@ src/manual.dvi

dvi: src/manual.tex
	cd src && latex manual.tex && latex manual.tex

src/manual.tex: src/manual.tex.in
	latex2man -CLATEX -L $< $@

manual.pdf: src/manual.ps
	ps2pdf13 $< $@

manual.html: src/manual.tex
	cd src && hevea -fix manual.tex && hevea -fix manual.tex
	cp src/manual.html .

src/manual.tex.stripped: src/manual.tex.in
	python src/remove-tt.py $< $@

wyrd.1: src/manual.tex.stripped
	latex2man -M $< $@

wyrdrc.5: src/manual.tex.stripped
	latex2man -CWYRDRC -M $< $@

post-build-cleanup: manual.pdf manual.html wyrd.1 wyrdrc.5
	cd src && rm -f *.aux *.log *.toc *.haux *.htoc *.dvi *.ps *.stripped *.html manual.tex

clean:
	rm -f manual.tex *.aux *.log *.toc *.haux *.htoc *.dvi *.ps *.pdf *.html *.stripped wyrd.1 wyrdrc.5
	cd src && rm -f manual.tex *.aux *.log *.toc *.haux *.htoc *.dvi *.ps *.pdf *.html *.stripped wyrd.1 wyrdrc.5


# arch-tag: DO_NOT_CHANGE_a5d62ea3-3a73-4de2-a2b3-a70bb310823f 
