.PHONY: clean

pages := 00-header.html \
				 01-titlepage.html \
				 02-intro.html \
				 03-format.html \
				 04-koine.html \
				 05-j.html \
				 volume.html \
				 99-footer.html

ebook.pdf:

%.pdf: %.html
	docker run --rm -v "`pwd`":/data michaelperrin/prince:latest -j -o /data/$@ /data/$<

result.pdf: ebook.html
	@echo "FIXME: css injection"
	sed -i '/^@page {$$/a marks: crop cross;' styles/main.css
	sed -i '/^@page {$$/a bleed: 5mm;' styles/main.css
	sed -i '/^@page {$$/a -prince-trim: 5mm;' styles/main.css
	docker run --rm -v "`pwd`":/data michaelperrin/prince:latest -j -o /data/$@ /data/$<
	sed -i '/^marks: crop cross;/d' styles/main.css
	sed -i '/^bleed: 5mm;/d' styles/main.css
	sed -i '/^-prince-trim: 5mm;/d' styles/main.css

book.pdf: result.pdf
	gs -dPDFX -dBATCH -dNOPAUSE -dNOOUTERSAVE -dNoOutputFonts -sDEVICE=pdfwrite -sColorConversionStrategy=CMYK -dProcessColorModel=/DeviceCMYK -dCompatibilityLevel=1.4 -dPDFSETTINGS=/prepress -dHaveTransparency=false -sOutputFile="$@" $<

ebook.html: $(pages)
	cat $? > $@

%.html: static/%.html.mustache
	haskell-mustache $< meta.json > $@

volume.html: build
	cabal run > $@
	@echo "FIXME: removing html entities with actual chars"
	sed -i '1,5d'         $@
	sed -i 's/&amp;/\&/g' $@
	sed -i 's/&lt;/</g'   $@
	sed -i 's/&gt;/>/g'   $@
	sed -i 's/&quot;/"/g' $@

build:
	cabal build

clean:
	rm -rf *.html *.pdf
