(asdf:defsystem :resources-ttl-generator
  :name "resources-ttl-generator"
  :author "Aad Versteden <madnificent@gmail.com>"
  :version "0.0.1"
  :maintainer "Aad Versteden <madnificent@gmail.com>"
  :licence "MIT"
  :description "TTL generator for mu-cl-resources specification."
  :serial t
  :depends-on (mu-cl-resources)
  :components ((:file "packages")
               (:file "ttl-generator")))
