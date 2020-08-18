(ql:quickload :siki)

(setf siki::*config* :development)
(setf siki::*os-type* :linux)

(sb-ext:save-lisp-and-die 
  "siki-console" 
  :toplevel #'siki:start-siki-server 
  :executable t)

