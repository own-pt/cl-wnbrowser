sbcl --eval '(ql:quickload :cl-wnbrowser)' --eval '(cl-wnbrowser::start-server 8080)' --eval '(loop do (sleep most-positive-fixnum))'
