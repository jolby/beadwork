(asdf:defsystem "beadwork"
  :description "Beadwork — Common Lisp beads issue tracker"
  :author "Joel Boehland"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("sqlite"
               "com.inuoe.jzon"
               "clingon"
               "ironclad"
               "local-time"
               "alexandria"
               "cl-ppcre"
               "uiop")
  :serial nil
  :build-operation "program-op"
  :build-pathname #p"bw"
  :entry-point "beadwork:main"
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "conditions"  :depends-on ("package"))
                 (:file "model"       :depends-on ("package" "conditions"))
                 (:file "schema"      :depends-on ("package" "conditions" "model"))
                 (:file "storage"     :depends-on ("package" "conditions" "model" "schema"))
                 (:file "sync"        :depends-on ("package" "conditions" "model" "storage"))
                 (:file "id"          :depends-on ("package" "conditions" "model"))
                 (:file "cli"         :depends-on ("package" "conditions" "model" "storage" "sync" "id")))))
  :in-order-to ((asdf:test-op (asdf:test-op "beadwork/tests"))))

(asdf:defsystem "beadwork/tests"
  :description "Tests for Beadwork"
  :depends-on ("beadwork" "parachute")
  :components ((:module "test"
                :components
                ((:file "package")
                 (:file "base" :depends-on ("package")))))
  :perform (asdf:test-op (op c)
                    (uiop:symbol-call :parachute :test :beadwork)))
