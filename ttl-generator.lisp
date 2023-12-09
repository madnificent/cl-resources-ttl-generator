(in-package :ttl-generator)

(mu-cl-resources::add-prefix "cms" "http://mu.semte.ch/vocabulary/cms/")

(declaim (optimize (debug 3) (speed 0)))

(defparameter *construct-union-classes* nil)

(defmacro with-union-store (&body body)
  `(let ((*union-store* nil)
         (*union-store-index* 0))
     (declare (special *union-store* *union-store-index*))
     ,@body))

(defun all-resources ()
  (format t "Getting all resources")
  (let (values)
    (org.shirakumo.luckless.hashtable:maphash
     (lambda (k v)
       (declare (ignore k))
       (push v values))
     mu-cl-resources::*resources*)
    (reverse values)))

(defun generate ()
  "Constructs the ttl file."
  (flet ((make-generation (target-file)
           (let ((ttl-specification
                   (with-union-store
                     (format nil "~A~&~%~A~&~%~A~&~%~A~&~%~A~&~%~A~&~%"
                             (make-ttl-prefixes)
                             (make-ttl-ontology-description)
                             (make-ttl-class-description)
                             (make-ttl-datatype-descriptions)
                             (make-ttl-relation-descriptions)
                             (make-ttl-union-classes)))))
             (when (find :docker *features*)
               (with-open-file (output target-file :direction :output :if-exists :supersede)
                 (format output "~A" ttl-specification)))
             (format t "~A" ttl-specification))))
    (let ((*construct-union-classes* t))
      (format t "~&~%Model with union classes~%~%~%")
      (make-generation "/config/output/model.ttl"))
    (let ((*construct-union-classes* nil))
      (format t "~&~%Model without union classes~%~%~%")
      (make-generation "/config/output/model-without-unions.ttl"))))

(defun make-ttl-prefixes ()
  "Constructs the prefixes which may have been used in the model."
  (format nil "~&~{@prefix ~A: <~A>.~%~}@prefix dc: <http://purl.org/dc/elements/1.1/>.~%"
          (loop for (prefix . iri)
             in (cl-fuseki:get-prefix-alist)
             append (list prefix iri))))

(defun make-ttl-ontology-description ()
  "Constructs the description of the ontology."
  (format nil "~{~A~&~}"
          (list "<http://mu.semte.ch/ontology> rdf:type owl:Ontology."
                "<http://mu.semte.ch/ontology> dc:title \"Generated resource ontology\". "
                "<http://mu.semte.ch/ontology> dc:description \"Generated resource ontology\". ")))

(defun make-ttl-class-description ()
  "Constructs description of the class entities."
  (format nil "~{~A rdf:type owl:Class;~@[~&   rdfs:subClassOf ~{~A~^, ~};~]~&  rdfs:label \"~A\".~&~}"
   (loop for resource in (all-resources)
      append
        (list (mu-cl-resources::ld-class resource)
              (loop for superclass in (mu-cl-resources::superclass-names resource)
                    collect (mu-cl-resources::ld-class (mu-cl-resources::find-resource-by-name superclass)))
              (string-downcase (symbol-name (mu-cl-resources::resource-name resource)))))))


(defun make-ttl-datatype-descriptions ()
  "Constructs datatype descriptions."
  (let ((properties-hash (make-hash-table :test 'equal)))
    (loop for resource in (all-resources)
          do
             (loop for property in (mu-cl-resources::ld-properties resource)
                   for ld-name = (format nil "~A" (mu-cl-resources::ld-property property))
                   for connected-properties = (gethash ld-name properties-hash nil)
                   do
                      (setf (gethash ld-name properties-hash) `((,property . ,resource) ,@connected-properties))))
    (format nil "~{~A rdf:type owl:DatatypeProperty;~&  rdfs:comment \"Resources type is ~A~@[~A~]\";~&  rdfs:label \"~A\";~&  rdfs:range rdfs:Literal;~&  rdfs:domain ~A.~&~}~%"
            (if *construct-union-classes*
                (loop for properties being the hash-values of properties-hash
                      for (property . resource) = (first properties) ;; TODO: join values in case of reuse
                      append
                      (list (mu-cl-resources::ld-property property)
                            (string-downcase (symbol-name (mu-cl-resources::resource-type property)))
                            nil ; no extra comment
                            (string-downcase (symbol-name (mu-cl-resources::json-key property)))
                            (if (> (length properties) 1)
                                (union-class-id (loop for (property . resource) in properties
                                                      collect
                                                      (mu-cl-resources::ld-class resource)))
                                (mu-cl-resources::ld-class resource))))
                (loop for properties being the hash-values of properties-hash
                      for used-once-p = (not (rest properties))
                      append
                      (loop for (property . resource) in properties
                            for ld-name = (if used-once-p
                                              (mu-cl-resources::ld-property property)
                                              (get-local-id))
                            append (list ld-name
                                         (string-downcase (symbol-name (mu-cl-resources::resource-type property)))
                                         (format nil ".  Original predicate is ~A." (mu-cl-resources::ld-property property))
                                         (string-downcase (symbol-name (mu-cl-resources::json-key property)))
                                         (mu-cl-resources::ld-class resource))))))))

(defun make-ttl-relation-descriptions ()
  "Constructs relationship descriptions."
  (let ((links-hash (make-hash-table :test 'equal)))
    (loop for resource in (all-resources)
       do
         (loop for relationship in (mu-cl-resources::all-links resource)
            for ld-name = (format nil "~A" (mu-cl-resources::ld-link relationship))
            for connected-links = (gethash ld-name links-hash nil)
            do
              (setf (gethash ld-name links-hash) `((,relationship . ,resource) ,@connected-links))))
    (format nil "~{~A rdf:type ~{~A~^, ~};~&  rdfs:comment \"~A\";~&  rdfs:label \"~A\";~&  rdfs:domain ~A;~&  rdfs:range ~A.~&~}~%"
            (if *construct-union-classes*
                (loop for relationships being the hash-values of links-hash
                      for (relationship . resource) = (first relationships) ;; TODO: join values in case of reuse
                      append
                      (list (mu-cl-resources::ld-link relationship)
                            (let* ((applicable-types (list "owl:ObjectProperty"))
                                   (relationship-entities (mapcar #'car relationships))
                                   (direct-relationships (remove-if-not #'mu-cl-resources::inverse-p relationship-entities))
                                   (inverse-relationships (remove-if #'mu-cl-resources::inverse-p relationship-entities)))
                              (when (and (> (length direct-relationships) 0)
                                         (every (lambda (r) (typep r 'mu-cl-resources::has-one-link)) direct-relationships))
                                (push "owl:FunctionalProperty" applicable-types))
                              (when (and (> (length inverse-relationships) 0)
                                         (every (lambda (r) (typep r 'mu-cl-resources::has-one-link)) inverse-relationships))
                                (push "owl:InverseFunctionalProperty" applicable-types))
                              applicable-types)
                            (format nil "Names are ~{~A~^, ~}."
                                    (loop for (relationship . resource) in relationships
                                          collect (if (mu-cl-resources::inverse-p relationship)
                                                      (format nil "- ~A" (mu-cl-resources::request-path relationship))
                                                      (mu-cl-resources::request-path relationship))))
                            (mu-cl-resources::request-path relationship)
                            (let ((types (remove-duplicates
                                          (loop for (relationship . resource) in relationships
                                                collect
                                                (if (mu-cl-resources::inverse-p relationship)
                                                    (mu-cl-resources::ld-class (resource-by-name (mu-cl-resources::resource-name relationship)))
                                                    (mu-cl-resources::ld-class resource)))
                                          :test #'string=
                                          :key (lambda (x) (format nil "~A" x)))))
                              (if (eql (length types) 1)
                                  (first types)
                                  (union-class-id types)))
                            (let ((types (remove-duplicates
                                          (loop for (relationship . resource) in relationships
                                                collect
                                                (if (mu-cl-resources::inverse-p relationship)
                                                    (mu-cl-resources::ld-class resource)
                                                    (mu-cl-resources::ld-class (resource-by-name (mu-cl-resources::resource-name relationship)))))
                                          :test #'string=
                                          :key (lambda (x) (format nil "~A" x)))))
                              (if (eql (length types) 1)
                                  (first types)
                                  (union-class-id types)))))
                (loop for all-relationships being the hash-values of links-hash
                      for deduplicated-relationships  ; filter out all relationships that are inverse which have a non-inverse counterpart
                        = (loop for (rel . res) in all-relationships
                                if (or
                                    ;; it's not an inverse, so we can collect it
                                    (not (mu-cl-resources::inverse-p rel))
                                    ;; it's an inverse which doesn't have a counterpart
                                    (not (find-if (lambda (direct-rel-description)
                                                    (let ((straight-resource (cdr direct-rel-description))
                                                          (straight-relation (car direct-rel-description)))
                                                      (unless (mu-cl-resources::inverse-p straight-relation)
                                                        (let ((straight-source-name (mu-cl-resources::resource-name straight-resource))
                                                              (straight-target-name (mu-cl-resources::resource-name straight-relation))
                                                              (inverse-source-name (mu-cl-resources::resource-name res))
                                                              (inverse-target-name (mu-cl-resources::resource-name rel)))
                                                          (and (not (mu-cl-resources::inverse-p straight-relation))
                                                               (equal straight-source-name inverse-target-name)
                                                               (equal straight-target-name inverse-source-name))))))
                                                  all-relationships)))
                                  collect (cons rel res))
                      for single-relationship-p = (= 1 (length deduplicated-relationships))
                      append (loop for (relationship . resource) in deduplicated-relationships
                                   for inverse-p = (mu-cl-resources::inverse-p relationship)
                                   for relation-left = (mu-cl-resources::ld-class resource)
                                   for relation-right = (mu-cl-resources::ld-class
                                                         (mu-cl-resources::find-resource-by-name
                                                          (mu-cl-resources::resource-name relationship)))
                                   for ld-source-class = (if inverse-p relation-right relation-left)
                                   for ld-target-class = (if inverse-p relation-left relation-right)
                                   append
                                   (list (if single-relationship-p
                                             (mu-cl-resources::ld-link relationship)
                                             (get-local-id))
                                         (list "owl:ObjectProperty") ; could be enhanced with 1->m m->1 1->1 but need to collect more info
                                         (if single-relationship-p
                                             ""
                                             (format nil "Original relationship is ~A" (mu-cl-resources::ld-link relationship)))
                                         (mu-cl-resources::request-path relationship)
                                         ld-source-class
                                         ld-target-class)))))))

(defun resource-by-name (name)
  (mu-cl-resources::find-resource-by-name name))

(defparameter *id-counter* 0)
(defun get-local-id ()
  "Yields a local identifier."
  (format nil "ext:local_id_~A" (incf *id-counter*)))

(defun union-class-id (classes)
  "Yields an identifier for the union class and ensures it exists"
  (declare (special *union-store* *union-store-index*))
  (let* ((properties (sort
                      (remove-duplicates (mapcar #'princ-to-string classes)
                                         :test #'string=)
                      #'string<))
         (idx (position properties *union-store* :test #'equal)))
    (flet ((print-union-index (idx)
             (format nil "_:union~A" idx)))
      (if idx
          (print-union-index idx)
          (progn
            (setf *union-store* `(,@*union-store* ,properties))
            (print-union-index (1- (length *union-store*))))))))


(defun make-ttl-union-classes ()
  (declare (special *union-store*))
  (format nil "~&~{_:union~A rdf:type owl:Class;~%  owl:unionOf (~{~A~^ ~}).~%~}"
          (loop
            for idx from 0
            for classes in *union-store*
            append `(,idx ,classes))))
