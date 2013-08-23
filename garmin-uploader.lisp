(eval-when (:compile-toplevel :load-toplevel :execute)
  (:asd :drakma)
  (:asd :cl-json)
  (:asd :closure-html)
  (:asd :cxml-stp))

(defpackage garmin
  (:use :cl)
  (:export
   :upload
   :upload-edge
   :upload-fr))

(in-package :garmin)

(defvar *config-file* (merge-pathnames ".config/garmin-uploader"
                                       (user-homedir-pathname)))

(defvar *username* nil)
(defvar *password* nil)
(defvar *uploaded* nil)
(defvar *activities-directories* nil)

(defvar *cookie-jar* (make-instance 'drakma:cookie-jar))

(defvar *device-type* :edge
  ":edge or :fr")

(defvar *signin* "https://connect.garmin.com/signin")
(defvar *upload-url*
  "http://connect.garmin.com/proxy/upload-service-1.1/json/upload/.fit")

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

(defun print-errors (json)
  (format t "An error has occured:~% ")
  (let* ((entries (caar (cdr (assoc :entries
                                   (cdr (assoc :report json))))))
         (report (cdr (assoc :trace (cdr (assoc :trace entries))))))
    (write-line (subseq report 0 (position #\Newline report)))))

(defun parse-response (response)
  (let* ((json (cdar (json:decode-json-from-string response)))
         (id (cdr (assoc :internal-id (cadr (assoc :successes json))))))
    (cond (id
           (view-activity id))
          (t
           (print-errors json)))))

(defun launch-browser (url)
  #+sbcl(sb-ext:run-program "opera" (list url)
                            :search t
                            :wait nil)
  #+ccl(ccl:run-program "opera" (list url)
                        :wait nil))

(defun view-activity (id)
  (launch-browser (format nil "http://connect.garmin.com/activity/~a" id)))

(defun upload-file (file)
  (format t "Uploading file ~a~%" (file-namestring file))
  (multiple-value-bind (response status)
      (request *upload-url*
               `(("responseContentType" . "text/html")
                 ("data" ,(pathname file) :filename ,(file-namestring file)))
               :form-data t)
    (case status
      (200
       (parse-response response)
       (pushnew (file-namestring file) (getf *uploaded* *device-type*)))
      (t
       (error "Bad status ~a ~a" status
              (babel:octets-to-string  response :encoding :latin1))))))

(defun read-config ()
  (with-open-file (stream *config-file*)
    (destructuring-bind (&key username password
                              uploaded
                              path)
        (read stream)
      (setf *uploaded* uploaded
            *username* username
            *password* password
            *activities-directories* path)
      (values))))

(defun write-config ()
  (with-open-file (stream *config-file* :direction :output
                                        :if-exists :supersede)
    (prin1 (list :username *username*
                 :password *password*
                 :path *activities-directories*
                 :uploaded *uploaded*)
           stream)
    (values)))

(defun files-to-upload ()
  (set-difference (directory (merge-pathnames "*.fit"
                                              (getf *activities-directories*
                                                    *device-type*)))
                  (getf *uploaded* *device-type*)
                  :test #'equal :key #'file-namestring))

(defun upload ()
  (read-config)
  (login)
  (mapc #'upload-file (files-to-upload))
  (write-config))

(defun upload-fr ()
  (let ((*device-type* :fr))
    (upload)))

(defun upload-edge ()
  (let ((*device-type* :edge))
    (upload)))

#+(or)
(ccl:save-application "do-garmin-upload"
                      :toplevel-function
                      (lambda ()
                        (unwind-protect
                             (if (equal (file-namestring
                                         (car ccl:*command-line-argument-list*))
                                        "ugfr")
                                 (garmin:upload-fr)
                                 (garmin:upload-edge))
                          (ccl:quit)))
                      :prepend-kernel t)
