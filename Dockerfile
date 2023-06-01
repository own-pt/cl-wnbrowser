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

ENV PASSWORD cff6d76ac0361bcc518cab2de7ba9b88f667458de86fbf705516
ENV ES_URL https://5325493c-3489-4c1d-a81e-5db7cbaef410.8117147f814b4b2ea643610826cd2046.databases.appdomain.cloud:31366
ENV GITHUB_CLIENT_ID 19ac9046042b97f88f88
ENV GITHUB_CLIENT_SECRET ea064ffdd1c28aebddc208b40ec04cc5ee440ca7

EXPOSE 8080

CMD ["bash", "run.sh"]
# 1. (quicklisp-quickstart:install)
# 2. (ql:add-to-init-file)
# 3. baixar as depencias em ~/quicklisp/local-projects
# 4. (ql:quickload :graph-algorithms)
# 5. (ql:quickload :cl-wnbrowser)
# 6. (in-package :cl-wnbrowser)
# 7. (START-SERVER 8000)