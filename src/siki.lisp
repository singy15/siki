(in-package :siki)

;; Variables
(defparameter *siki-server* nil)
(defparameter *config* :development) ; :development / :production
(defparameter *time-to-shutdown* nil)
(defparameter *max-time-to-shutdown* (* 5 60))
(defparameter *siki-port* nil)
(defparameter *swank-port* nil)
; (defparameter *app-file-modified* nil)
; (defparameter *app-src* #p"./app.lisp")
(defparameter *lisp-modified* (make-hash-table))
(defparameter *db-path* "./master.db")

;; Configuration
(setf djula:*catch-template-errors-p* nil)
(setf djula:*fancy-error-template-p* nil)
(setf djula:*auto-escape* nil)
(djula:add-template-directory "html/")

;;; Read file
(defun slurp (path)
  (alexandria:read-file-into-string path :external-format :utf-8))

;;; Write file
(defun spit (path content)
  (alexandria:write-string-into-file content path 
    :external-format :utf-8 :if-exists :supersede))

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

;; Timestamp
(defun get-timestamp ()  
  (multiple-value-bind 
    (ss mm hh d m y) 
    (decode-universal-time (get-universal-time))
    (format nil "~4,'0d~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d" y m d hh mm ss)))

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
          :document-root "static/"))
  
  ;; Start listen
  (hunchentoot:start *siki-server*)
  
  ;; Time to shutdown
  (keep-server-alive)
  
  ;; Start swank server for development
  (swank:create-server :port *swank-port* :dont-close t)
  
  ;; Load source
  (mapcar 
    (lambda (path)
      (load path)
      (setf (gethash path *lisp-modified*) (file-write-date path))
      (format t "~a loaded ~a~%" path (file-write-date path))) 
    (directory "./lisp/**/*.lisp"))

  ;; Connect
  (datafly:connect-toplevel :sqlite3 :database-name *db-path*) 
  
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
    
    ;; Reload
    (maphash 
      (lambda (key val)
        (when (and (equal *config* :development)
                   (not (equal (gethash key *lisp-modified*) (file-write-date key))))
          (load key)
          (setf (gethash key *lisp-modified*) (file-write-date key))
          (format t "~a reloaded ~a~%" key (file-write-date key))))
      *lisp-modified*)
    
    (sleep 1)))

;;; Reset kill timer
(defun keep-server-alive ()
  (setf *time-to-shutdown* *max-time-to-shutdown*))

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

;;; GET /siki/reload
(defroute get-siki-reload ("/siki/reload" :method :get) ()
  ;; Load source
  ; (load *app-src*)
  
  ;; Return result
  (json:encode-json-to-string
    `((:success . t)
      (:msg . ,(format nil "Application reloaded: ~a" (get-timestamp))))))

;;; GET /siki/shutdown
(defroute get-siki-shutdown ("/siki/shutdown" :method :get) ()
  (cl-user::exit))

(in-package :cl-user)

