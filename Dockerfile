FROM debian:buster AS builder

# ensure locale is set during build
ENV LANG C.UTF-8

# stack installed binaries are placed here
ENV PATH /root/.local/bin:$PATH

# update and install important packages
RUN apt-get update
RUN apt-get install -y --no-install-recommends gnupg ca-certificates dirmngr curl git

# install haskell stack
RUN curl -sSL https://get.haskellstack.org/ | sh

# rl-srl-web
RUN mkdir -p /opt/rl-srl-web
COPY package.yaml /opt/rl-srl-web/
COPY stack.yaml /opt/rl-srl-web/
COPY LICENSE /opt/rl-srl-web/
COPY README.md /opt/rl-srl-web/
COPY src /opt/rl-srl-web/src
COPY app /opt/rl-srl-web/app
COPY test /opt/rl-srl-web/test

WORKDIR /opt/rl-srl-web
RUN stack setup
RUN stack install --no-interleaved-output
RUN /bin/bash -c "cp $(stack path --local-install-root)/bin/rl-srl-web ."

FROM debian:buster

HEALTHCHECK --interval=30s --start-period=30s --timeout=10s CMD curl -f https://rev.vadg.io/ || exit 1

# set the port
ENV PORT 3000
RUN echo PORT SET TO ${PORT}

# update
RUN apt-get update
# run the thing
RUN mkdir -p /opt/rl-srl-web
COPY --from=builder /opt/rl-srl-web/rl-srl-web /opt/rl-srl-web/rl-srl-web
# COPY frontend /opt/rl-srl-web/frontend
WORKDIR /opt/rl-srl-web
ENTRYPOINT ["./rl-srl-web"]
