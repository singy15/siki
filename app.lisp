;; Package app
(defpackage app
  (:use
    cl
    easy-routes
    djula
    sxql)
  (:export))

(in-package :app)

;;; On server startup
(defun siki:startup ()
  ;;; Create sample table
  (unless (siki:p-table-exists :sample)
    ;; Create table 
    (datafly:execute
      (create-table :sample
        ((id 
           :type 'integer :not-null t 
           :primary-key t :autoincrement t)
         (name
           :type 'text :not-null t)
         (desc
           :type 'text :not-null t))))

      ;; Insert record
      (datafly:execute 
        (insert-into :sample
          (set= 
            :name "Sample 1"
            :desc "Description 1")))

      (datafly:execute 
        (insert-into :sample
          (set= 
            :name "Sample 2"
            :desc "Description 2")))

      (datafly:execute 
        (insert-into :sample
          (set= 
            :name "Sample 3"
            :desc "Description 3")))

      (datafly:execute 
        (insert-into :sample
          (set= 
            :name "Sample 4"
            :desc "Description 4")))
      
      ;; Update
      (datafly:execute 
        (update :sample
          (set= 
            :desc "description3")
          (where (:= :name "Sample 3"))))
      
      ;; Delete
      (datafly:execute 
        (delete-from :sample (where (:= :name "Sample 4"))))))

;;; On server shutdown
(defun siki:shutdown ()
  nil)

;;; GET /
(defroute get-index ("/" :method :get) (param1)
  (hunchentoot:log-message* :INFO "index param1: ~a" param1)
          
  (djula:render-template* 
    (djula:compile-template* "index.html") nil nil))

;;; GET /samples 
(defroute get-samples ("/samples" :method :get) ()
  (json:encode-json-to-string 
    (datafly:retrieve-all (select :* (from :sample)) 
                          :as 'trivial-types:association-list)))

;;; POST /samples 
(defroute post-samples ("/samples" :method :post) ()
  (let ((body (json:decode-json-from-string
                (hunchentoot:raw-post-data :force-text t))))
    (hunchentoot:log-message* :INFO "post-samples ~a" body)))

(in-package :cl-user)

