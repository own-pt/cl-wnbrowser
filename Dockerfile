FROM --platform=linux/amd64 clfoundation/sbcl:2.1.5-slim

RUN apt update
RUN apt-get install -y build-essential
RUN apt-get install -y git

ADD https://beta.quicklisp.org/quicklisp.lisp /root/quicklisp.lisp
RUN set -x; \
  sbcl --load /root/quicklisp.lisp \
    --eval '(quicklisp-quickstart:install)' \
    --eval '(ql:add-to-init-file)' \
    --quit

WORKDIR /root/quicklisp/local-projects

RUN git clone https://github.com/own-pt/graph-algorithms.git
RUN git clone https://github.com/own-pt/clesc

COPY . ./cl-wnbrowser

RUN sbcl --eval '(ql:quickload :graph-algorithms)' --quit


WORKDIR /root
COPY .sbclrc .
COPY run.sh .

EXPOSE 8080

CMD ["bash", "run.sh"]