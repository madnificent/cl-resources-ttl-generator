(in-package :ttl-generator)

(mu-cl-resources::add-prefix "cms" "http://mu.semte.ch/vocabulary/cms/")

(declaim (optimize (debug 3) (speed 0)))

(defun all-resources ()
  "Yields back all known resources."
  (loop for val being
     the hash-values of mu-cl-resources::*resources*
     collect val))

(defun generate ()
  "Constructs the ttl file."
  (let ((ttl-specification
         (format nil "~A~&~%~A~&~%~A~&~%~A~&~%~A~&~%"
                 (make-ttl-prefixes)
                 (make-ttl-ontology-description)
                 (make-ttl-class-description)
                 (make-ttl-datatype-descriptions)
                 (make-ttl-relation-descriptions))))
    (when (find :docker *features*)
      (with-open-file (output "/config/output/model.ttl" :direction :output :if-exists :supersede)
        (format output "~A" ttl-specification)))
    (format t "~A" ttl-specification)))

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
  (format nil "~{~A rdf:type owl:Class;~&  rdfs:label \"~A\".~&~}"
   (loop for resource in (all-resources)
      append
        (list (mu-cl-resources::ld-class resource) (string-downcase (symbol-name (mu-cl-resources::resource-name resource)))))))

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
    (format nil "~{~A rdf:type owl:DatatypeProperty;~&  rdfs:comment \"Resources type is ~A\";~&  rdfs:label \"~A\";~&  rdfs:range rdfs:Literal;~&  rdfs:domain ~A.~&~}~%"
            (loop for properties being the hash-values of properties-hash
               for (property . resource) = (first properties) ;; TODO: join values in case of reuse
               append
                 (list (mu-cl-resources::ld-property property)
                       (string-downcase (symbol-name (mu-cl-resources::resource-type property)))
                       (string-downcase (symbol-name (mu-cl-resources::json-key property)))
                       (if (> (length properties) 1)
                           (format nil "[ rdf:type owl:Class; owl:unionOf (~{~A~^ ~}) ]"
                                   (loop for (property . resource) in properties
                                      collect
                                        (mu-cl-resources::ld-class resource)))
                           (mu-cl-resources::ld-class resource)))))))

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
    (format nil "~{~A rdf:type ~{~A~^, ~};~&  rdfs:comment \"Names are ~{~A~^, ~}\";~&  rdfs:label \"~A\";~&  rdfs:domain ~A;~&  rdfs:range ~A.~&~}~%"
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
                       (loop for (relationship . resource) in relationships
                          collect (if (mu-cl-resources::inverse-p relationship)
                                      (format nil "- ~A" (mu-cl-resources::request-path relationship))
                                      (mu-cl-resources::request-path relationship)))
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
                             (format nil "[ rdf:type owl:Class; owl:unionOf (~{~A~^ ~}) ]" types)))
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
                             (format nil "[ rdf:type owl:Class; owl:unionOf (~{~A~^ ~}) ]" types))))))))

(defun resource-by-name (name)
  (mu-cl-resources::find-resource-by-name name))
