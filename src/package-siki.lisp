(defpackage siki
  (:use 
    cl
    easy-routes
    djula
    sxql)
  (:export
    start-siki-server
    slurp
    spit
    p-table-exists
    keep-server-alive
    startup
    shutdown))

