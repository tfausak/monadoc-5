FROM ubuntu:18.04
RUN apt-get update && apt-get install --assume-yes ca-certificates
COPY data /opt/monadoc/data
COPY ghc-8.10.1 /opt/ghc/8.10/lib/ghc-8.10.1
ENV monadoc_datadir /opt/monadoc
COPY monadoc /usr/local/bin/
