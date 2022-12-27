.PHONY: clean

pages_before := 00-header.part \
								01-titlepage.part \
								02-intro.part \
								03-format.part \
								04-koine.part
pages_after :=  99-footer.part

%-book.pdf: %.html
	@echo "FIXME: css injection"
	sed -i '/^@page {$$/a marks: crop cross;' styles/main.css
	sed -i '/^@page {$$/a bleed: 5mm;' styles/main.css
	sed -i '/^@page {$$/a -prince-trim: 5mm;' styles/main.css
	docker run --rm -v "`pwd`":/data michaelperrin/prince:latest -j -o /data/tmp.pdf /data/$<
	gs -dPDFX -dBATCH -dNOPAUSE -dNOOUTERSAVE -dNoOutputFonts -sDEVICE=pdfwrite -sColorConversionStrategy=CMYK -dProcessColorModel=/DeviceCMYK -dCompatibilityLevel=1.4 -dPDFSETTINGS=/prepress -dHaveTransparency=false -sOutputFile="$@" tmp.pdf
	sed -i '/^marks: crop cross;/d' styles/main.css
	sed -i '/^bleed: 5mm;/d' styles/main.css
	sed -i '/^-prince-trim: 5mm;/d' styles/main.css
	rm -rf tmp.pdf

%.pdf: %.html
	docker run --rm -v "`pwd`":/data michaelperrin/prince:latest -j -o /data/$@ /data/$<

%.html: $(pages_before) 50-%.part $(pages_after)
	cat $? > $@

%.part: static/%.html.mustache
	haskell-mustache $< meta.json > $@

50-%.part: build
	cabal run typeset -- $* > $@
	@echo "FIXME: removing html entities with actual chars"
	sed -i '1,5d'         $@
	sed -i 's/&amp;/\&/g' $@
	sed -i 's/&lt;/</g'   $@
	sed -i 's/&gt;/>/g'   $@
	sed -i 's/&quot;/"/g' $@

build:
	cabal build

clean:
	rm -rf *.html *.part *.pdf
