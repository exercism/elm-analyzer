# Build the summary builder
cd review 
elm make src/Main.elm --output=src/main.js
cp src/cli.js src/main.js ../bin
