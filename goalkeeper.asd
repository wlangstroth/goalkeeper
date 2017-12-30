;;;; goalkeeper.asd

(asdf:defsystem #:goalkeeper
  :description "Goal management"
  :author "Will Langstroth <will@langstroth.com>"
  :license "MIT"
  :serial t
  :depends-on (#:chronograph #:cl-string-match)
  :components ((:file "package")
               (:file "goalkeeper")))
