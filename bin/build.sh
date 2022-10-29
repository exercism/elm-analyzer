# Build the summary builder
npx elm make src/Main.elm --output=src/main.js --optimize
cp src/cli.js src/main.js ./bin
