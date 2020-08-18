(ql:quickload :siki)

(setf siki::*config* :development)

(sb-ext:save-lisp-and-die 
  "siki-console" 
  :toplevel #'siki:start-siki-server 
  :executable t)

