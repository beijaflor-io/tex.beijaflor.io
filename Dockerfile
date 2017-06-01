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

ADD ./.stack-fpm/linux/usr/local/bin/simple-tex-service /usr/local/bin/simple-tex-service
RUN apt-get update && apt-get install -y libgmp-dev texlive-full latex-beamer context make

CMD ["simple-tex-service"]
