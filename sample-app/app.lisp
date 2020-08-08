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
  (unless (siki:p-table-exists :issue)
    ;; Create table 
    (datafly:execute
      (create-table :issue
        ((id 
           :type 'integer :not-null t 
           :primary-key t :autoincrement t)
         (category
           :type 'text :not-null t)
         (subject
           :type 'text :not-null t)
         (desc
           :type 'text :not-null t)
         (status
           :type 'text :not-null t)
         (assigned_to
           :type 'text :not-null t))))))

;;; On server shutdown
(defun siki:shutdown ()
  nil)

;;; GET /
(defroute get-index ("/" :method :get) (param1)
  (hunchentoot:log-message* :INFO "index param1: ~a" param1)
          
  (djula:render-template* 
    (djula:compile-template* "index.html") nil nil))

;;; GET /issues 
(defroute get-issues ("/issues" :method :get) ()
  (json:encode-json-to-string 
    (datafly:retrieve-all 
      (select :* (from :issue) (order-by (:desc :id))) 
        :as 'trivial-types:association-list)))

;;; POST /issues
(defroute post-issues ("/issues" :method :post) ()
  (let ((body (json:decode-json-from-string 
                (hunchentoot:raw-post-data :force-text t))))
    (hunchentoot:log-message* :INFO "post-issue ~a" body)
    
    (if (equal (cdr (assoc :id body)) "")
      (datafly:execute 
        (insert-into :issue
          (set= 
            :category (cdr (assoc :category body))
            :subject (cdr (assoc :subject body))
            :desc (cdr (assoc :desc body))
            :assigned_to (cdr (assoc :assigned-to body))
            :status (if (equal "" (cdr (assoc :status body)))
                        "NW"
                        (cdr (assoc :status body))))))
      (datafly:execute 
        (update :issue
          (set= 
            :category (cdr (assoc :category body))
            :subject (cdr (assoc :subject body))
            :desc (cdr (assoc :desc body))
            :assigned_to (cdr (assoc :assigned-to body))
            :status (cdr (assoc :status body)))
          (where (:= :id (cdr (assoc :id body)))))))
    
    (json:encode-json-to-string
      `(:success t :msg ""))))

;;; DELETE /issues
(defroute delete-issues ("/issues" :method :delete) ()
  (let ((body (json:decode-json-from-string 
                (hunchentoot:raw-post-data :force-text t))))
    (hunchentoot:log-message* :INFO "delete-issue ~a" body)
    
    (datafly:execute (delete-from :issue (where (:in :id body))))

    (json:encode-json-to-string
      `(:success t :msg ""))))

(in-package :cl-user)

