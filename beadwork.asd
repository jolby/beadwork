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
  :build-operation "program-op"
  :build-pathname #p"bw"
  :entry-point "beadwork:main"
  :serial T
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "conditions")
                 (:file "model")
                 (:file "schema")
                 (:file "storage")
                 (:file "sync")
                 (:file "id")
                 (:file "cli"))))
  :in-order-to ((asdf:test-op (asdf:test-op "beadwork/tests"))))

(asdf:defsystem "beadwork/tests"
  :description "Tests for Beadwork"
  :depends-on ("beadwork" "parachute")
  :serial T
  :components ((:module "test"
                :components
                ((:file "package")
                 (:file "suite") 
                 (:file "base"))))
  :perform (asdf:test-op (op c)
                    (uiop:symbol-call :parachute :test :beadwork)))
