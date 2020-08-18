(ql:quickload :siki)

(setf siki::*config* :development)

(sb-ext:save-lisp-and-die 
  "siki.exe" 
  :toplevel #'siki:start-siki-server 
  :executable t
  :application-type :gui)

