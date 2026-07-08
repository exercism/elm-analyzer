# sha256:a0b9bf06e4e6193cf7a0f58816cc935ff8c2a908f81e6f1a95432d679c54fbfd => node:24.18.0-alpine3.24
FROM node:lts-alpine@sha256:a0b9bf06e4e6193cf7a0f58816cc935ff8c2a908f81e6f1a95432d679c54fbfd AS builder

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

# Copy source code
COPY . .

# Install elm tools
ENV ELM_HOME="/opt/analyzer/.elm"
RUN npm ci

# Build cache in .elm, elm-stuff, solution/elm.json and solution/elm-stuff
# For the solution
WORKDIR /opt/analyzer/test_data/two-fer/perfect_solution
RUN mkdir -p elm-stuff && rm -rf elm-stuff \
  && npx elm make src/TwoFer.elm --output=/dev/null
# For the analyzer
WORKDIR /opt/analyzer/
RUN bin/build.sh \
  && bin/run.sh two-fer test_data/two-fer/perfect_solution test_data/two-fer/perfect_solution \
  && tar cf /opt/analyzer/solution_cache.tar -C test_data/two-fer/perfect_solution elm-stuff elm.json

# Lightweight runner container
FROM node:lts-alpine3.23@sha256:d1b3b4da11eefd5941e7f0b9cf17783fc99d9c6fc34884a665f40a06dbdfc94f
WORKDIR /opt/analyzer
ENV PATH="/opt/analyzer/bin:${PATH}"
ENV ELM_HOME="/opt/analyzer/.elm"
COPY elm.json elm.json
COPY src src
COPY --from=builder /opt/analyzer/.elm .elm
COPY --from=builder /opt/analyzer/elm-stuff elm-stuff
COPY --from=builder /opt/analyzer/node_modules node_modules
COPY --from=builder /opt/analyzer/bin bin
COPY --from=builder /opt/analyzer/solution_cache.tar solution_cache.tar
ENTRYPOINT [ "bin/run.sh" ]
