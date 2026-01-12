(in-package :shacl-generator)

(declaim (optimize (debug 3) (speed 0)))

(defun all-resources ()
  (format t "Getting all resources")
  (let (values)
    (org.shirakumo.luckless.hashtable:maphash
     (lambda (k v)
       (declare (ignore k))
       (push v values))
     mu-cl-resources::*resources*)
    (reverse values)))

(defparameter *service-base-uri*
  "http://services.redpencil.io/mu-cl-resources/my-resources"
  "URI for this service and base for sub-resources.")

(defun generate (&optional (*service-base-uri* "http://services.redpencil.io/mu-cl-resources/my-resources"))
  "Constructs the ttl file."
  (let ((shacl-specification
          (format nil "~A~%~%# SERVICE DESCRIPTION~%~%~A~%# API ENDPOINTS~%~%~{~A~%~}~%~%# SHACL SHAPES~%~%~{~A~%~}~%~A~%"
                  (make-ttl-prefixes)
                  (make-ttl-service-description)
                  (mapcar #'make-resource-processes (all-resources))
                  (mapcar #'make-resource-shape (all-resources))
                  (make-full-shape))))
    (when (find :docker *features*)
      (with-open-file (output "/config/output/shacl-description.ttl" :direction :output :if-exists :supersede)
        (format output "~A" shacl-specification)))
    (format t "~&~%SHACL SERVICE DESCRIPTION~%~%~A" shacl-specification)))

(defun microservice-uri ()
  "URI of this microservice."
  (format nil "<~A>" *service-base-uri*))

(defun resource-uri-postfix (resource)
  (string-downcase (symbol-name (mu-cl-resources::resource-name resource))))

(defun make-process-uri (kind resource)
  (format nil "<~A/processes/~A/~A>"
          *service-base-uri*
          (string-downcase (symbol-name kind))
          (resource-uri-postfix resource)))

(defun full-shape-uri ()
  (format nil "<~A/full-shape>"
          *service-base-uri*))

(defun resource-shape-uri (resource)
  (format nil "<~A/shapes/~A>"
          *service-base-uri*
          (resource-uri-postfix resource)))

(defun resource-property-uri (resource slot)
  (format nil "<~A/shapes/~A/properties/~A>"
          *service-base-uri*
          (resource-uri-postfix resource)
          (mu-cl-resources::json-key slot)))

(defun resource-property-inverse-path-uri (resource slot)
  (format nil "<~A/shapes/~A/properties/~A/paths>"
          *service-base-uri*
          (resource-uri-postfix resource)
          (mu-cl-resources::json-key slot)))

(defun make-ttl-prefixes ()
  "Constructs the prefixes which may have been used in the model."
  (format nil "~&~{@prefix ~A: <~A>.~%~}@prefix dc: <http://purl.org/dc/elements/1.1/>.~%@prefix dct: <http://purl.org/dc/terms/>.~%@prefix sh: <http://www.w3.org/ns/shacl#>.~%@prefix ext: <http://mu.semte.ch/vocabluries/ext/>.~%@prefix mu: <http://mu.semte.ch/vocabularies/core/>.~%"
          (loop for (prefix . iri)
                  in (cl-fuseki:get-prefix-alist)
                append (list prefix iri))))

(defun make-ttl-service-description ()
  "Constructs the description of the service as per diSHCALed."
  (format nil "~{~A~&~}"
          (list (microservice-uri)
                "  a mu:Microservice;"
                "  dct:title \"Auto-converted microservice.\";"
                (format nil "  ext:input ~A;" (full-shape-uri))
                (format nil "  ext:output ~A." (full-shape-uri)))))

(defun make-resource-processes (resource)
  "Construct the processes for this resource."
  (concatenate 'string
               (format nil "~&## Processes for ~A~%~%" (resource-uri-postfix resource))
               (format nil "~A~%  ext:hasProcess~{~%    ~A~^,~}.~%~%"
                       (microservice-uri)
                       (mapcar (lambda (x) (make-process-uri x resource))
                               '(:show :list :new :update)))
               (format nil "~{~A~%~}~%"
                       (list (make-process-uri :show resource)
                             "  a ext:Process;"
                             (format nil "  dct:title \"Show ~A resource.\";"
                                     (resource-uri-postfix resource))
                             (format nil "  ext:input ~A;" (resource-shape-uri resource))
                             (format nil "  ext:output ext:emptyShape.")))
               (format nil "~{~A~%~}~%"
                       (list (make-process-uri :list resource)
                             "  a ext:Process;"
                             (format nil "  dct:title \"List ~A resource.\";"
                                     (resource-uri-postfix resource))
                             (format nil "  ext:input ~A;" (resource-shape-uri resource))
                             (format nil "  ext:output ext:emptyShape.")))
               (format nil "~{~A~%~}~%"
                       (list (make-process-uri :new resource)
                             " a ext:Process;"
                             (format nil "  dct:title \"Create new ~A resource.\";"
                                     (resource-uri-postfix resource))
                             (format nil "  ext:input ext:emptyShape;")
                             (format nil "  ext:output ~A." (resource-shape-uri resource))))
               (format nil "~{~A~%~}~%"
                       (list (make-process-uri :update resource)
                             " a ext:Process;"
                             (format nil "  dct:title \"Updates a ~A resource.\";"
                                     (resource-uri-postfix resource))
                             (format nil "  ext:input  ~A;" (resource-shape-uri resource))
                             (format nil "  ext:output ~A." (resource-shape-uri resource))))
               (format nil "~{~A~%~}~%"
                       (list (make-process-uri :delete resource)
                             " a ext:Process;"
                             (format nil "  dct:title \"Deletes a ~A resource.\";"
                                     (resource-uri-postfix resource))
                             (format nil "  ext:input  ~A;" (resource-shape-uri resource))
                             (format nil "  ext:output ~A." (resource-shape-uri resource))))))

(defun make-resource-shape (resource)
  "Construct the shape for `RESOURCE'."
  (format nil "~{~@[~A~%~]~}~%"
          `(,(resource-shape-uri resource)
            "  a sh:NodeShape;"
            ,(format nil "  sh:targetClass ~A." (mu-cl-resources::ld-class resource))
            ;; the properties
            ,@(loop for slot in (mu-cl-resources::ld-properties resource)
                    for shacl-property-uri = (resource-property-uri resource slot)
                    collect
                    (format nil "~A sh:property ~A.~%~A~%  sh:path ~A;~%  sh:minCount ~A~@[;~%  sh:maxCount ~A~]~@[;~%  sh:dataType ~A~]."
                            (resource-shape-uri resource)
                            shacl-property-uri
                            shacl-property-uri
                            (mu-cl-resources::ld-property slot)
                            (if (mu-cl-resources::required-p slot) 1 0)
                            (unless (find (mu-cl-resources::resource-type slot) '(:string-set :uri-set :language-string-set))
                              ;; these have multiple values, in whichcase there may not be an upper bound
                              1)
                            (case (mu-cl-resources::resource-type slot)
                              (:string "xsd:string")
                              (:number "xsd:decimal") ; could be float or integer too
                              (:integer "xsd:integer")
                              (:float "xsd:float")
                              (:boolean "xsd:boolean")
                              (:date "xsd:date")
                              (:datetime "xsd:dateTime")
                              (:time "xsd:time")
                              (:string-set "xsd:string")
                              (:language-string "xsd:string")
                              (:language-string-set "xsd:string")
                              (:g-year "xsd:gYear")
                              (:geometry "geo:wktLiteral")
                              (otherwise nil) ; :url and :uri-set will not get a dataType
                              )))
            ;; the relations
            ,@(loop for link in (mu-cl-resources::all-links resource)
                    for shacl-property-uri = (resource-property-uri resource link)
                    for inverse-path-uri = (resource-property-inverse-path-uri resource link)
                    append
                    `(,(format nil "~A sh:property ~A.~%~A~%  sh:path ~A;~%  sh:minCount 0~@[;~%  sh:maxCount ~A~];~%  sh:class ~A;~%  sh:nodeKind sh:IRI."
                               (resource-shape-uri resource)
                               shacl-property-uri
                               shacl-property-uri
                               (if (mu-cl-resources::inverse-p link)
                                   inverse-path-uri
                                   (mu-cl-resources::ld-link link))
                               (and (typep link 'mu-cl-resources::has-one-link) 1)
                               (mu-cl-resources::ld-class (mu-cl-resources::referred-resource link)))
                      ,@(when (mu-cl-resources::inverse-p link)
                          (list (format nil "~A sh:inversePath ~A." inverse-path-uri (mu-cl-resources::ld-link link))))))
            ,(format nil "~A sh:closed false." (resource-shape-uri resource)))))

(defun make-full-shape ()
  (format nil "~A~%  a sh:NodeShape;~%  sh:or (~{~%    ~A~})."
          (full-shape-uri)
          (mapcar #'resource-shape-uri (all-resources))))
