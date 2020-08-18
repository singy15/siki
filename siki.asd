(defsystem "siki"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (
    "datafly" 
    "hunchentoot" 
    "easy-routes" 
    "swank" 
    "djula" 
    "cl-json" 
    "local-time" 
    "alexandria" 
    "mito"
    "cl-ppcre"
    "cl-fad"
    "dbd-sqlite3")
  :components ((:module "src"
                :components
                ((:file "package-siki")
                 (:file "siki"))))
  :description "")

