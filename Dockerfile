FROM ubuntu:xenial
MAINTAINER Pedro Tacla Yamada <tacla.yamada@gmail.com>

RUN apt-get update && apt-get install -y \
    texlive-full \
    latex-beamer \
    context \
    make \
    && apt-get clean \
    && rm -r /var/lib/apt/lists/*

WORKDIR /data
VOLUME ["/data"]

ADD ./simple-tex-service_0.1.0.0_amd64.deb /app/
RUN apt-get update && apt-get install -y /app/simple-tex-service_0.1.0.0_amd64.deb

CMD ["simple-tex-service"]
