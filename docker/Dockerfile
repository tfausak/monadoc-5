FROM ubuntu:18.04
RUN apt-get update && apt-get install --assume-yes ca-certificates
COPY data /opt/monadoc
ENV monadoc_datadir /opt/monadoc
COPY monadoc /usr/local/bin/
