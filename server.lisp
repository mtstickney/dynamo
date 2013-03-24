(in-package #:dynamo)

;; TODO: find a way to supply version-code and version-info without
;; doing it manually (i.e. in make-instance)
(defclass default-server-service (default-rpc-service)
  ((server :initarg :server :reader server))
    (:documentation "Default implementation of the SERVER service."))

(defgeneric rpc-version (service)
  (:documentation "Return the RPC protocol version of this server.")
  (:method ((service default-server-service))
    (rpc-version (server service))))

(defgeneric services (service)
  (:documentation "Return the list of services hosted on this server.")
  (:method ((service default-server-service))
    (mapcar #'car (services (server service)))))

(defgeneric describe-service (service service-name)
  (:documentation "Return a description of the the service named SERVICE-NAME")
  (:method ((service default-server-service) (service-name string))
    (let ((svc (find-service (server service)  service-name)))
      (when (null svc)
        (error 'invalid-service :service service-name))
      (with-output-to-string (str)
        (describe (class-of svc) str)))))

(defmethod initialize-instance :after ((service default-server-service) &key)
  (register-method service "rpcVersion" #'rpc-version)
  (register-method service "services" #'services)
  (register-method service "describeService" #'describe-service)
  (register-method service "describeMethod" #'describe-method)
  (register-method service "serviceVersion" #'service-version))

(defmethod service-version ((service default-server-service))
  (list (cons :code (version-code service))
        (cons :name (version-name service))))

(defmethod describe-method ((service default-server-service) (method string))
  (let ((entry (find-method-entry service method)))
    (with-output-to-string (str)
      (describe (method-entry-func entry) str))))
