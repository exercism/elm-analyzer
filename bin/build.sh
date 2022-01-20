# Build the summary builder
elm make src/Main.elm --output=src/main.js
cp src/cli.js src/main.js ./bin
