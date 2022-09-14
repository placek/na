all: book.pdf

app:
	cabal build

in.html: app
	cabal run > in.html
	sed -i '1,3d' in.html
	sed -i 's/&amp;/\&/g' in.html
	sed -i 's/&lt;/</g' in.html
	sed -i 's/&gt;/>/g' in.html
	sed -i 's/&quot;/"/g' in.html

out.pdf: in.html
	docker run --rm -v "`pwd`":/data michaelperrin/prince:latest -o /data/out.pdf /data/in.html

book.pdf: out.pdf title1.pdf title2.pdf
	pdfunite title1.pdf title2.pdf out.pdf book.pdf

clean:
	rm -rf in.html out.pdf book.pdf
