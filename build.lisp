(ql:quickload :siki)

(setf siki::*configuration* :production)

(sb-ext:save-lisp-and-die 
  "siki" 
  :toplevel #'siki:start-siki-server 
  :executable t 
  :application-type :gui)

