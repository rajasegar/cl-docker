(defsystem "cl-docker"
  :version "0.2.0"
  :author "Rajasegar Chandran"
  :license "MIT"
  :depends-on ("dexador" "yason" "flexi-streams" "drakma" "cl-ppcre")
  :components ((:module "src"
                :components
                ((:file "utils")
								 (:file "main" :depends-on ("utils")))))
  :description ""
  :in-order-to ((test-op (test-op "cl-docker/tests"))))

(defsystem "cl-docker/tests"
  :author ""
  :license ""
  :depends-on ("cl-docker"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-docker"
  :perform (test-op (op c) (symbol-call :rove :run c)))
