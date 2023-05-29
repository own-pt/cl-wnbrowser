FROM ubuntu:latest

WORKDIR /root/

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y unzip curl openssh-client git sbcl supervisor 

RUN curl -O http://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(quit)'
COPY .sbclrc .
COPY setup.sh .
RUN chmod +x setup.sh && ./setup.sh

ENV LC_ALL=C.UTF-8
ENV LANG=C.UTF-8

CMD /bin/bash
