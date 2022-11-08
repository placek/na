.PHONY: clean edit

pages := static/00-header.html \
				 static/01-titlepage.html \
				 static/02-intro.html \
				 static/03-format.html \
				 static/04-koine.html \
				 static/05-j.html \
				 volume.html \
				 static/99-footer.html

result.pdf:

%.pdf: %.html
	docker run --rm -v "`pwd`":/data michaelperrin/prince:latest -j -o /data/$@ /data/$<

result.html: $(pages)
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
	rm -rf volume.html result.html result.pdf
