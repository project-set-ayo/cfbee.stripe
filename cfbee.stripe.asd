;;;; cfbee.stripe.asd

(asdf:defsystem #:cfbee.stripe
  :description "Stripe system with payment and product/commodity integration."
  :author "Ayo Onipe <mail@ayoonipe.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:mito
	       #:cl-json
	       #:dexador
	       #:cl-ppcre
	       #:cl-slug
	       #:str
	       #:uuid)
  :components ((:file "package")
               (:file "cfbee.stripe")))
