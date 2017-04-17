FROM ubuntu:16.10
MAINTAINER Pedro Tacla Yamada <tacla.yamada@gmail.com>

RUN apt-get -qq update && apt-get install -y -q \
    texlive-full \
    latex-beamer \
    context \
    make \
    && apt-get clean \
    && rm -r /var/lib/apt/lists/*

WORKDIR /data
VOLUME ["/data"]

CMD ["/bin/bash"]
