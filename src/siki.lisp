(in-package :siki)

;; Variables
(defparameter *siki-server* nil)
(defparameter *preload-templates* (make-hash-table :test #'equal))
(defparameter *configuration* :development)
(defparameter *time-to-shutdown* nil)
(defparameter *max-time-to-shutdown* (* 5 60))
(defparameter *siki-port* nil)
(defparameter *swank-port* nil)

;; Configuration
(setf djula:*catch-template-errors-p* nil)
(setf djula:*fancy-error-template-p* nil)
(setf djula:*auto-escape* nil)
(djula:add-template-directory "templates/")
(djula:add-template-directory "siki-templates/")

;;; Read file
(defun slurp (path)
  (alexandria:read-file-into-string path :external-format :utf-8))

;;; Write file
(defun spit (path content)
  (alexandria:write-string-into-file content path 
    :external-format :utf-8 :if-exists :supersede))

;;; Add preload template
(defun add-preload-template (name)
  (setf (gethash name *preload-templates*) 
        (djula:render-template* 
          (djula:compile-template* name) nil nil)))

;;; Get preload template
(defun preload-template (name) 
  (if (equal *configuration* :development)
      (djula:render-template* 
        (djula:compile-template* name) nil nil)
      (gethash name *preload-templates*)))

;;; Check table exists
(defun p-table-exists (table-name)
  (> (getf (datafly:retrieve-one
             (select
               ((:as (:count :*) :cnt))
               (from :sqlite_master)
               (where (:and 
                        (:= :type "table") 
                        (:= :name table-name))))) 
           :cnt) 0))

;;; On server startup
(defun startup ()
  nil)

;;; On server shutdown
(defun shutdown ()
  nil)

;;; Start siki server
(defun start-siki-server ()
  ;; Set port
  (setf *siki-port* (+ 40000 (random 1000 (make-random-state t))))
  (setf *swank-port* (+ 50000 (random 1000 (make-random-state t))))
  
  ;; Notice
  (format t "SIKI USING PORT: ~a~%" *siki-port*)
  (format t "SWANK USING PORT: ~a~%" *swank-port*)
  
  ;; Create server instance
  (setf *siki-server* 
        (make-instance 
          'easy-routes:easy-routes-acceptor 
          :port *siki-port*
          :document-root "document-root/"))
  
  ;; Start listen
  (hunchentoot:start *siki-server*)
  
  ;; Time to shutdown
  (keep-server-alive)
  
  ;; Start swank server for development
  (swank:create-server :port *swank-port* :dont-close t)
  
  ;; Load source
  (load "./app.lisp")

  ;; Connect
  (datafly:connect-toplevel :sqlite3 :database-name "./master.db") 
  
  ;; Startup
  (startup)
  
  ;; Auto open browser
  (sb-ext:run-program 
    "/usr/bin/xdg-open" 
    (list (format nil "http://localhost:~a/" *siki-port*)) :wait nil)

  ;; Wait for shutdown
  (loop
    (decf *time-to-shutdown*)
    (when (< *time-to-shutdown* 0)
      ;; Shutdown
      (shutdown)

      ;; Disconnect
      (datafly:disconnect-toplevel) 

      ;; Exit
      (cl-user::exit))
    (sleep 1)))

;;; Reset kill timer
(defun keep-server-alive ()
  (setf *time-to-shutdown* *max-time-to-shutdown*))

;;; Add preload template
(add-preload-template "control.html")

;;; GET /siki/keep-alive
(defroute get-keep-alive ("/siki/keep-alive" :method :get) ()
  (keep-server-alive)
  (json:encode-json-to-string 
    `(:success t :time ,*time-to-shutdown*)))

;;; GET /siki/server-state
(defroute get-server-state ("/siki/server-state" :method :get) ()
  (json:encode-json-to-string 
    `((:success . t) 
      (:siki-port . ,*siki-port*)
      (:swank-port . ,*swank-port*))))

;;; GET /siki/control
(defroute get-siki-control ("/siki/control" :method :get) ()
  (preload-template "control.html"))

;;; GET /siki/reload
(defroute get-siki-reload ("/siki/reload" :method :get) ()
  ;; Load source
  (load "./app.lisp")
  
  ;; Return result
  (json:encode-json-to-string
    `((:success . t)
      (:msg . "Application reloaded"))))

(in-package :cl-user)

