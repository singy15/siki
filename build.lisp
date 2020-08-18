(ql:quickload :siki)

(setf siki::*config* :production)
(setf siki::*os-type* :linux)

(sb-ext:save-lisp-and-die 
  "siki" 
  :toplevel #'siki:start-siki-server 
  :executable t )

