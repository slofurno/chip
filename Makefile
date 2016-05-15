
public/app.html: src/*.elm
	elm-make src/*.elm --output="./public/app.html"
