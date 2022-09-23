.PHONY: clean edit

target := result.pdf
intro  := static/00-header.html static/01-titlepage.html static/02-format.html static/03-koine.html static/04-j.html static/99-footer.html
pages  := 01-intro.pdf 99-volume.pdf

$(target): $(pages)
	pdfunite $? $@

%.pdf: %.html
	docker run --rm -v "`pwd`":/data michaelperrin/prince:latest -j -o /data/$@ /data/$<

01-intro.html: $(intro)
	cat $? > $@

99-volume.html: build
	cabal run > $@
	@echo "FIXME: removing html entities with actual chars"
	sed -i '1,4d'         $@
	sed -i 's/&amp;/\&/g' $@
	sed -i 's/&lt;/</g'   $@
	sed -i 's/&gt;/>/g'   $@
	sed -i 's/&quot;/"/g' $@

build:
	cabal build

edit: $(target)
	okular $?

clean:
	rm -rf 99-volume.html 99-volume.pdf 01-intro.html 01-intro.pdf $(target)
