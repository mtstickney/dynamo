;;;; dynamo.lisp

(in-package #:dynamo)

;;; TODO: Figure out what return values of methods should be
;; TODO: get rpc-version in here somewhere
(defclass rpc-server (weft:server)
  ((services :initform '() :initarg :services :reader services))
  (:documentation "Class representing a server that hosts RPC services"))

(defmethod weft:run :around ((server rpc-server) &key (backlog 5) (element-type '(unsigned-byte 8)))
  (unless (eq element-type '(unsigned-byte 8))
    (error "~A is not a supported element type for an RPC Server." element-type))
  (call-next-method server :backlog backlog :element-type element-type))

(defun make-rpc-server (address port)
  (let ((server (make-instance 'rpc-server :address address :port port)))
    (setf (weft:server-connection-handler server)
          #'(lambda (sock)
              (process-request sock server)))
    server))

(defclass rpc-service ()
  ()
  (:documentation "Class representing a service hosted on an RPC server."))

;; TODO: add a report and documentation here
(define-condition no-such-service-error ()
  ((service :initarg :service)))

(define-condition service-exists-error ()
  ((service :initarg :service)))

(defmacro asetf (place form)
  (let ((place-var (gensym)))
    `(let ((,place-var ,place))
       (setf ,place (let ((it ,place-var)) ,form)))))

(defgeneric find-service (server service)
  (:documentation "Return the service named SERVICE registered on SERVER, or nil.")
  (:method ((server rpc-server) (service string))
    (let ((cell (assoc service (services server) :test #'string-equal)))
      (values (cdr cell) (if cell t nil)))))

(defgeneric (setf find-service) (new server service)
  (:method ((new rpc-service) (server rpc-server) (service string))
    (let ((cell (assoc service (services server) :test #'string-equal)))
      (if cell
          (setf (cdr cell) new)
          (asetf (slot-value server 'services)
                 (acons service new it))))))

(defgeneric service-name (server service)
  (:documentation "Return the name SERVICE is registered as on SERVER, or NIL.")
  (:method ((server rpc-server) (service rpc-service))
    (car (find service (services server) :key #'cdr))))

(defgeneric register-service (server service service-name &key replace)
  (:documentation "Register the SERVICE with SERVER as SERVICE-NAME")
  (:method ((server rpc-server) (service rpc-service) (service-name string) &key (replace t))
    (cond
      ((and (find-service server service-name)
            (not replace))
       (error 'service-exists-error :service service-name))
      (t (setf (find-service server service-name) service)))))

(defgeneric unregister-service (server service)
  (:documentation "Unregister the SERVICE from SERVER")
  (:method ((server rpc-server) (service string))
    (asetf (slot-value server 'services)
           (remove service it :key #'car :test #'string-equal))
    nil)
  (:method ((server rpc-server) (service rpc-service))
    (asetf (slot-value server 'services)
           (remove service it :key #'cdr :test #'eq))))

(defmacro anafirst (form &rest args)
  `(let ((it ,form))
     ,@args
     it))

(defun process-call (server call)
  "Process a single RPC call and return a result object."
  (format *debug-io* "Processing call ~S~%" call)
  (let* ((service (prog1 (find-service server (cl-mtgnet::rpc-call-service call))
                    (format *debug-io* "Looked up service~%")))
         (dispatch-data (prog1 (multiple-value-list
                                (dispatch service
                                          (cl-mtgnet::rpc-call-method call)
                                          (cl-mtgnet::rpc-call-args call)))
                          (format *debug-io* "Method has been dispatched~%")))
         (result (prog1 (cl-mtgnet::make-rpc-result :data (first dispatch-data)
                                                 :id (cl-mtgnet::rpc-call-id call))
                   (format *debug-io* "Result created~%"))))

    (if (cl-mtgnet::rpc-call-id call)
        (apply #'values (cons result (rest dispatch-data)))
        nil)))

(defun read-request (socket)
  "Read a request object from SOCKET."
  (let ((str (anafirst (cl-rpc::recv-string (usocket:socket-stream socket))
                       (format *debug-io* "Received string ~S~%" it))))
    (anafirst (cl-mtgnet::unmarshall-rpc-request str)
              (format *debug-io* "Unmarshalled request ~S~%" it))))

(defmacro with-response ((&optional (var 'json:*json-output*)) &body body)
  `(with-output-to-string (,var)
                          (json:with-array (,var)
                                           ,@body)))

(defun process-request (socket server)
  "Process an RPC request made to SERVER over SOCKET."
  (format *debug-io* "Processing requests~%")
  (let* ((req (read-request socket))
         (results (mapcar #'(lambda (call)
                              (multiple-value-list
                               (process-call server call)))
                          req)))
    (cl-rpc::send-string
     (usocket:socket-stream socket)
     (with-response ()
       (loop for res in results
             when res
             do
             (destructuring-bind (result &optional (encoder #'json:encode-json))
                 res
               (cl-rpc::marshall-result result encoder)))))))

(defgeneric methods (service)
  (:documentation "Returns a list of methods that can be invoked on this service."))

(defgeneric describe-method (service method)
  (:documentation "Return a description of the RPC method METHOD."))

(defgeneric service-version (service)
  (:documentation "Return the version of SERVICE"))

(defgeneric dispatch (service method args)
  (:documentation "Invoke METHOD on SERVICE with ARGS, and return the result if any."))

(defgeneric result-encoder (service method)
  (:documentation "Return a function used to encode the result of the method to JSON."))

(defstruct method-entry
  (name nil :type string)
  (func nil :type function)
  (result-encoder #'json:encode-json :type function))

(define-condition missing-version-code () ())
(define-condition missing-version-name () ())

(defclass default-rpc-service (rpc-service)
  ((dispatch-table :initform (list (make-method-entry :name "version" :func #'service-version)
                                   (make-method-entry :name "describeMethod" :func #'describe-method)
                                   (make-method-entry :name "methods" :func #'methods))
                   :reader dispatch-table)
   (version-code :initarg :version-code
                 :initform (error 'missing-version-code)
                 :reader version-code)
   (version-name :initarg :version-name
                 :initform (error 'missing-version-name)
                 :reader version-name))
  (:documentation "Default RPC-service class which implements some convenience methods."))

(define-condition no-such-method-error ()
    ((service :initarg :service)
     (method :initarg :method)))

(defgeneric find-method-entry (service method)
  (:documentation "Return the dispatch table entry of SERVICE where METHOD is registered, or NIL.")
  (:method :around ((service default-rpc-service) method)
           (let ((entry (call-next-method service method)))
             (if (null entry)
                 (error 'no-such-method-error :method method :service service)
                 entry)))
  (:method ((service default-rpc-service) (method string))
    (find method (slot-value service 'dispatch-table) :test #'string-equal :key #'method-entry-name))
  (:method ((service default-rpc-service) (method function))
    (find method (slot-value service 'dispatch-table) :key #'method-entry-func)))

;;; TODO: Shouldn't be allowed to change the method list of an active service
(defgeneric register-method (service method-name method-func &key encoder replace)
  (:documentation "Register METHOD-FUNC as a method of SERVICE, callable as METHOD-NAME.")
  (:method ((service default-rpc-service) (method-name string) method-func &key (encoder #'json:encode-json) (replace t))
    (let ((entry (handler-case (find-method-entry service method-name)
                   (no-such-method-error (condition)
                     (declare (ignore condition))
                     nil))))
      (when (and entry (not replace))
        (error 'method-exists-error :method method-name))
      (if entry
          (setf (method-entry-func entry) method-func
                (method-entry-result-encoder entry) encoder)
          (push (make-method-entry :name method-name :func method-func :result-encoder encoder)
                (slot-value service 'dispatch-table))))))

(defgeneric unregister-method (service method)
  (:documentation "Unregister METHOD from SERVICE.")
  (:method ((service default-rpc-service) method)
    (with-slots (dispatch-table) service
      (delete (find-method-entry service method) dispatch-table))))

(defmethod dispatch ((service default-rpc-service) (method-name string) args)
  (let ((entry (find-method-entry service method-name)))
    (apply (method-entry-func entry) (cons service args))))

(defmethod methods ((service default-rpc-service))
  (mapcar #'method-entry-name (dispatch-table service)))

;; TODO: Um? What is this doing, and do we still need it?
(defmethod result-encoder ((service default-rpc-service) method)
  (let ((method-entry (find-method-entry service method)))
    (method-entry-result-encoder method-entry)))

;;; "dynamo" goes here. Hacks and glory await!
