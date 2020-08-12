#|
  laven
  Version 0.1.0
  Copyright (c) 2018 kedama
  License : MIT
|#

; Muffle style-warning.
(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

; Require ASDF.
(require 'asdf)

#| 
  Define package quicklisp-quickstart to prevent compile error.
|#
(defpackage quicklisp-quickstart
  (:use :cl)
  (:export
    install))

#| 
  Define package quicklisp-client to prevent compile error.
|#
(defpackage quicklisp-client
  (:use :cl)
  (:export
    quickload))

#|
  Define laven-util package.
|#
(defpackage laven-util
  (:nicknames :lvn-util)
  (:use :cl :sb-ext)
  (:export
    output-multiple-stream
    gen-help-str
    byte-copy
    logging
    get-home-dir-path
    get-cur-dir-name))

;; Change package to laven-util.
(in-package :laven-util)

#|
  Output string to multiple streams.
|#
(defun output-multiple-stream (strms str)
  (mapc 
    (lambda (s) 
      (format s str) )
    strms))

#|
  Generate help string for function.
|#
(defun gen-help-str (sym)
  (format nil "* ~(~A~):~(~A~)~%  ~A~%~%" (nth 0 (package-nicknames :laven)) sym (documentation sym 'function)))

#|
  Byte copy file.
|#
(defun byte-copy (input outfile)
  (with-open-file 
    (in-stream input :direction :input :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (when in-stream
      (with-open-file 
        (out-stream outfile :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (loop for byte = (read-byte in-stream nil) while byte do 
          (write-byte byte out-stream))))))

#|
  Logging.
|#
(defun logging (msg &optional (fmt nil))
  (format t 
          "~A~A~%" 
          (cond ((equal fmt nil) "")
                ((equal fmt :begin) ">>>> ")
                ((equal fmt :end) "<<<< ")
                ((equal fmt :warn) "WARNING: ")
                ((equal fmt :err) "ERROR: ")) 
          msg))

#|
  Get home directory path.
|#
(defun get-home-dir-path ()
  (cl-user::user-homedir-pathname))

#|
  Get current directory name.
|#
(defun get-cur-dir-name ()
  (car (reverse (pathname-directory (truename "")))))

#|
  Define laven package.
|#
(defpackage laven
  (:nicknames :lvn)
  (:use :cl :sb-ext)
  (:shadow :load)
  (:export 
    laven-install
    add-to-init-file
    init
    load
    create
    test
    ; build
    help))

; Change to package laven.
(in-package :laven)

; laven directory.
(defvar *laven-dir* #p".laven/")

; QuickLisp script location.
(defvar *quicklisp-script-location* (merge-pathnames "quicklisp.lisp" (lvn-util:get-home-dir-path)))

; QuickLisp home directory.
(defvar *laven-quicklisp-dir* (merge-pathnames "quicklisp/" *laven-dir*))

; QuickLisp setup.lisp path.
(defvar *laven-quicklisp-setup* (merge-pathnames "quicklisp/setup.lisp" *laven-dir*))

; Lisp config path.
(defvar *lisp-config* (merge-pathnames ".sbclrc" (lvn-util:get-home-dir-path)))

; laven directory for .gitignore
(defvar *laven-dir-gitignore* "/.laven")

#|
  Load quicklisp if exists.
|#
(defun load-quicklisp ()
  (when (probe-file *laven-quicklisp-setup*)
    (cl-user::load *laven-quicklisp-setup*)))

(defun laven-install ()
  "Install laven to home directory."
  
  (lvn-util:logging "LAVEN-INSTALL" :begin)
  
  (ensure-directories-exist (merge-pathnames *laven-dir* (lvn-util:get-home-dir-path)))
  (lvn-util:byte-copy "./laven.lisp" #p"~/.laven/laven.lisp")

  (lvn-util:logging "(lvn:add-to-init-file) will add your .sbclrc to load laven on boot")
  (lvn-util:logging "DONE" :end))

(defun add-to-init-file ()
  "Add setting to .sbclrc"
  
  (lvn-util:logging "ADD-TO-INIT-FILE" :begin)
  (lvn-util:logging "Add settings to your .sbclrc to load laven on startup.")

  (let* ((strm (open *lisp-config*
                    :direction :output
                    :if-exists :append
                    :if-does-not-exist :create))
         (strms (list strm t)))
    (lvn-util:output-multiple-stream strms "~%")
    (lvn-util:output-multiple-stream strms ";;; The following lines added by laven:add-to-init-file:~%")
    (lvn-util:output-multiple-stream strms "#-laven~%")
    (lvn-util:output-multiple-stream strms "(let ((laven-init (merge-pathnames~%")
    (lvn-util:output-multiple-stream strms "                    #p\".laven/laven.lisp\"~%")
    (lvn-util:output-multiple-stream strms "                    (user-homedir-pathname))))~%")
    (lvn-util:output-multiple-stream strms "  (when (probe-file laven-init)~%")
    (lvn-util:output-multiple-stream strms "    (load laven-init)))~%")
    (lvn-util:output-multiple-stream strms "~%")
    (close strm))

  (lvn-util:logging "DONE" :end))

(defun init ()
  "Initialize laven in current directory."

  (lvn-util:logging "INIT" :begin)
  
  ; Check laven initialized.
  (when (probe-file *laven-dir*)
    (lvn-util:logging "LAVEN ALREADY INITIALIZED" :err)
    (return-from init))

  ; Check quicklisp.lisp exist.
  (when (not (probe-file *quicklisp-script-location*))
    (lvn-util:logging (format nil "quicklisp.lisp NOT FOUND AT ~A" *quicklisp-script-location*) :err)
    (return-from init))
  
  ; Load quicklisp.lisp.
  (cl-user::load *quicklisp-script-location*)

  ; Create laven directory.
  (ensure-directories-exist *laven-dir*)

  ; Install QuickLisp.
  (quicklisp-quickstart:install :path *laven-quicklisp-dir*)

  ; Load QuickLisp.
  (load-quicklisp)

  (lvn-util:logging "DONE" :end))

(defun load (&optional system-name)
  "Load system."

  ; Load system.
  (quicklisp-client:quickload (if system-name system-name (lvn-util:get-cur-dir-name))))

#|
  Create .asd file.
|#
(defun create-asd (name)
  (let* ((strm (open (format nil "~A.asd" name)
                    :direction :output
                    :if-exists :supersede)))
    (format strm "(defsystem \"~A\"~%" name)
    (format strm "  :version \"0.1.0\"~%")
    (format strm "  :author \"\"~%")
    (format strm "  :license \"\"~%")
    (format strm "  :depends-on ()~%")
    (format strm "  :components ((:module \"src\"~%")
    (format strm "                :components~%")
    (format strm "                ((:file \"~A\"))))~%" name)
    (format strm "  :description \"\")~%")
    (format strm "~%")
    (close strm)))

#|
  Create source file.
|#
(defun create-src (name)
  (ensure-directories-exist (merge-pathnames #p"src/"))
  (let* ((strm (open (format nil "src/~A.lisp" name)
                    :direction :output
                    :if-exists :supersede)))
    (format strm "(defpackage ~A~%" name)
    (format strm "  (:use :cl))~%")
    (format strm "(in-package :~A)~%" name)
    (format strm "~%")
    (format strm ";; blah blah blah.~%")
    (close strm)))

#|
  Create .gitignore file.
|#
(defun create-gitignore (name)
  (let* ((strm (open (format nil ".gitignore" name)
                    :direction :output
                    :if-exists :supersede)))
    (format strm "~A~%" *laven-dir-gitignore*)
    (close strm)))

(defun create ()
  "Create system skeleton."
  
  (lvn-util:logging "CREATE" :begin)

  (let ((name (lvn-util:get-cur-dir-name)))
    (create-asd name)
    (create-src name)
    (create-gitignore name))

  (lvn-util:logging "DONE" :end)  )

(defun build (filename toplevel)
  "Create executable and exit (SBCL only)."

  (lvn-util:logging "BUILD" :begin)

  (sb-ext:save-lisp-and-die filename :toplevel toplevel :executable t)

  (lvn-util:logging "DONE" :end))

(defun test (system-name)
  "Execute asdf:test-system."

  (asdf:test-system system-name))

(defun help ()
  "Show commands available."

  (format t (lvn-util:gen-help-str 'laven-install))
  (format t (lvn-util:gen-help-str 'add-to-init-file))
  (format t (lvn-util:gen-help-str 'load))
  (format t (lvn-util:gen-help-str 'create))
  ; (format t (lvn-util:gen-help-str 'build))
  ; (format t (lvn-util:gen-help-str 'test))
  (format t (lvn-util:gen-help-str 'help  )))

; Load quicklisp if installed.
(load-quicklisp)

; Change package to cl-user.
(in-package :cl-user)

#|
  Add current directory to central-registry.
|#
(setf asdf:*central-registry* '(*default-pathname-defaults*))

; Unmuffle style-warning.
(declaim #+sbcl(sb-ext:unmuffle-conditions style-warning))

