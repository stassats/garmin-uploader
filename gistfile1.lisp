(eval-when (:compile-toplevel :load-toplevel :execute)
  (:asd :drakma)
  (:asd :cl-json)
  (:asd :closure-html)
  (:asd :cxml-stp))

(defpackage garmin
  (:use :cl)
  (:export
   :upload-everything))

(in-package :garmin)

(defvar *config-file* (merge-pathnames ".config/garmin-uploader"
                                       (user-homedir-pathname)))

(defvar *username* nil)
(defvar *password* nil)
(defvar *uploaded-files* nil)

(defvar *cookie-jar* (make-instance 'drakma:cookie-jar))
(defvar *activities-directory* #p"/mnt/garmin/Garmin/Activities/")

(defvar *signin* "https://connect.garmin.com/signin")

(defun request (url parameters &key form-data (method :post))
  (drakma:http-request url
                       :method method
                       :parameters parameters
                       :cookie-jar *cookie-jar*
                       :content-length t
                       :form-data form-data))

(defun elementp (x)
  (typep x 'stp:element))

(defun find-element-if (function document &key (type 'stp:element))
  (stp:do-recursively (node document)
    (when (and (typep node type)
               (funcall function node))
      (return node))))

(defun find-attribute (name value document)
  (find-element-if
   (lambda (node)
     (equal (stp:attribute-value node name) value))
   document))

(defun parse-html (page) 
  (chtml:parse page (stp:make-builder)))

(defun get-j-id ()
  (stp:attribute-value
   (find-attribute
    "name" "javax.faces.ViewState"
    (parse-html (request *signin* nil)))
   "value"))

(defun login ()
  (request *signin*
	   `(("login" . "login")
             ("login:loginUsernameField" . ,*username*)
	     ("login:password" . ,*password*)
             ("login:signInButton" . "Sign In")
             ("javax.faces.ViewState" . ,(get-j-id)))))

(defun parse-response (response)
  (let* ((json (cdar (json:decode-json-from-string response)))
         (id (cdr (assoc :internal-id (cadr (assoc :successes json))))))
    (when id
      (view-activity id))))

(defun launch-browser (url)
  #+sbcl(sb-ext:run-program "opera" (list url)
                            :search t
                            :wait nil)
  #+ccl(ccl:run-program "opera" (list url)
                        :wait nil))

(defun view-activity (id)
  (launch-browser (format nil "http://connect.garmin.com/activity/~a" id)))

(defun upload (file)
  (multiple-value-bind (response status)
      (request "http://connect.garmin.com/proxy/upload-service-1.1/json/upload/.fit"
               `(("responseContentType" . "text/html")
                 ("data" ,(pathname file) :filename ,(file-namestring file)))
               :form-data t)
    (case status
      (200
       (parse-response response)
       (pushnew (file-namestring file) *uploaded-files*))
      (t
       (error "Bad status ~a ~a" status
              (babel:octets-to-string  response))))))

(defun read-config ()
  (with-open-file (stream *config-file*)
    (destructuring-bind (&key username password
                              uploaded)
        (read stream)
      (setf *uploaded-files* uploaded
            *username* username
            *password* password)
      (values))))

(defun write-config ()
  (with-open-file (stream *config-file* :direction :output
                                        :if-exists :supersede)
    (prin1 (list :username *username*
                 :password *password*
                 :uploaded *uploaded-files*)
           stream)))

(defun files-to-upload ()
  (loop for file in (directory (merge-pathnames "*.fit" *activities-directory*))
        unless (member (file-namestring file) *uploaded-files* :test #'equal)
        collect file))

(defun upload-everything ()
  (read-config)
  (login)
  (mapc #'upload (files-to-upload))
  (write-config))

#+(or)
(ccl:save-application "do-garmin-upload"
                      :toplevel-function
                      (lambda ()
                        (unwind-protect (garmin:upload-everything)
                          (ccl:quit)))
                      :prepend-kernel t)
