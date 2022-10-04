FROM node:lts-alpine AS builder

# Working directory as specified by exercism
WORKDIR /opt/analyzer

# Add binaries to path
ENV PATH="/opt/analyzer/bin:${PATH}"

# Install curl to download executables
RUN apk add --update --no-cache curl \
  && mkdir -p bin

# Install jq
RUN curl -L -o bin/jq https://github.com/stedolan/jq/releases/download/jq-1.6/jq-linux64 \
  && chmod +x bin/jq

# Install elm
RUN curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz \
  && gunzip -c elm.gz > bin/elm \
  && chmod +x bin/elm

# Install elm-review
ENV ELM_HOME="/opt/analyzer/.elm"
RUN npm install --global elm-review@2.7.4 --prefix /opt/analyzer

# Copy source code
COPY . .

# Build cache in .elm, elm-stuff, solution/elm.json and solution/elm-stuff
# For the solution
WORKDIR /opt/analyzer/test_data/two-fer/perfect_solution
RUN mkdir -p elm-stuff && rm -rf elm-stuff \
  && elm make src/TwoFer.elm --output=/dev/null
# For the analyzer
WORKDIR /opt/analyzer/
RUN bin/build.sh \
  && bin/run.sh two-fer test_data/two-fer/perfect_solution test_data/two-fer/perfect_solution \
  && tar cf /opt/analyzer/solution_cache.tar -C test_data/two-fer/perfect_solution elm-stuff elm.json

# Lightweight runner container
FROM node:lts-alpine
WORKDIR /opt/analyzer
ENV PATH="/opt/analyzer/bin:${PATH}"
ENV ELM_HOME="/opt/analyzer/.elm"
COPY elm.json elm.json
COPY src src
COPY --from=builder /opt/analyzer/.elm .elm
COPY --from=builder /opt/analyzer/elm-stuff elm-stuff
COPY --from=builder /opt/analyzer/bin bin
COPY --from=builder /opt/analyzer/lib lib
COPY --from=builder /opt/analyzer/solution_cache.tar solution_cache.tar
ENTRYPOINT [ "bin/run.sh" ]
