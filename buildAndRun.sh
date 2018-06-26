rm -rf elm-stuff
elm make src/Main.elm --output=elm.js --debug --yes
elm reactor