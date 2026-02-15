(defsystem "beadwork"
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
  :pathname "src/"
  :serial nil
  :components ((:file "package")
               (:file "conditions"  :depends-on ("package"))
               (:file "model"       :depends-on ("package" "conditions"))
               (:file "schema"      :depends-on ("package" "conditions" "model"))
               (:file "storage"     :depends-on ("package" "conditions" "model" "schema"))
               (:file "sync"        :depends-on ("package" "conditions" "model" "storage"))
               (:file "id"          :depends-on ("package" "conditions" "model"))
               (:file "cli"         :depends-on ("package" "conditions" "model" "storage" "sync" "id")))
  :in-order-to ((test-op (test-op "beadwork/tests"))))

(defsystem "beadwork/tests"
  :description "Tests for Beadwork"
  :depends-on ("beadwork" "fiveam")
  :pathname "tests/"
  :components ((:file "package")
               (:file "suite" :depends-on ("package")))
  :perform (test-op (op c)
                    (symbol-call :fiveam :run! :beadwork)))
