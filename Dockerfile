FROM ubuntu:xenial
MAINTAINER Pedro Tacla Yamada <tacla.yamada@gmail.com>

RUN apt-get update && apt-get install -y -q \
    texlive-full \
    latex-beamer \
    context \
    make \
    && apt-get clean \
    && rm -r /var/lib/apt/lists/*

WORKDIR /data
VOLUME ["/data"]

ADD ./dist/simple-tex-service_0.1.0.0_amd64.deb /app/
RUN dpkg -i /app/simple-tex-service_0.1.0.0_amd64.deb

CMD ["/bin/bash"]
