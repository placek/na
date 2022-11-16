.PHONY: clean

pages := static/00-header.html \
				 static/01-titlepage.html \
				 static/02-intro.html \
				 static/03-format.html \
				 static/04-koine.html \
				 static/05-j.html \
				 volume.html \
				 static/99-footer.html

ebook.pdf:

%.pdf: %.html
	docker run --rm -v "`pwd`":/data michaelperrin/prince:latest -j -o /data/$@ /data/$<

result.pdf: ebook.html
	sed -i '/^@page {$$/a marks: crop cross;' styles/main.css
	sed -i '/^@page {$$/a bleed: 5mm;' styles/main.css
	sed -i '/^@page {$$/a -prince-trim: 5mm;' styles/main.css
	docker run --rm -v "`pwd`":/data michaelperrin/prince:latest -j -o /data/$@ /data/$<

book.pdf: result.pdf
	gs -dPDFX -dBATCH -dNOPAUSE -dNOOUTERSAVE -dNoOutputFonts -sDEVICE=pdfwrite -sColorConversionStrategy=CMYK -dProcessColorModel=/DeviceCMYK -dCompatibilityLevel=1.4 -dPDFSETTINGS=/prepress -dHaveTransparency=false -sOutputFile="$@" $<

ebook.html: $(pages)
	cat $? > $@

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
	rm -rf volume.html ebook.html ebook.pdf book.pdf result.pdf
	sed -i '/^marks: crop cross;/d' styles/main.css
	sed -i '/^bleed: 5mm;/d' styles/main.css
	sed -i '/^-prince-trim: 5mm;/d' styles/main.css
