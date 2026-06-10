(in-package #:cfbee.stripe)


(defparameter *stripe-api-url* "https://api.stripe.com/v1"
  "Stripe API URL.")


(defclass stripe-integration-mixin ()
  ((tax-code :initarg :tax-code
             :accessor stripe-tax-code
             :col-type (or (:varchar 255) :null)
             :initform nil)
   (stripe-product-id :initarg :stripe-product-id
                      :accessor stripe-product-id
                      :col-type (or (:varchar 255) :null)
                      :initform nil)
   (stripe-price-id :initarg :stripe-price-id
                    :accessor stripe-price-id
                    :col-type (or (:varchar 255) :null)
                    :initform nil))
  (:metaclass mito:dao-table-mixin))

;;;
;;; Protocol: Commodity Information Sync
;;;
;;; Systems that rely on stripe product/commodity sync must implement
;;; these for their models.

(defgeneric stripe-item-name (obj))
(defgeneric stripe-item-price-cents (obj))
(defgeneric stripe-item-currency (obj))
(defgeneric stripe-secret-key (obj))
(defgeneric stripe-account-id (obj)
  (:documentation "Returns the Stripe Connected Account ID (acct_1...) if applicable.")
  (:method (obj) nil))

;;;
;;; 
;;;

(defun stripe-request (secret-key endpoint method data &key stripe-account)
  (let ((url (concatenate 'string *stripe-api-url* endpoint))
        (headers (list (cons "Stripe-Version" "2026-03-25.preview"))))
    
    (when stripe-account
      (push (cons "Stripe-Account" stripe-account) headers))
    
    (cl-json:decode-json-from-string
     (dex:request url
                  :method method
                  :basic-auth (cons secret-key "")
                  :headers headers
                  :content (when data 
                             (remove-if (lambda (pair) (null (cdr pair))) data))))))

(defun sync-model (obj)
  (let ((secret-key (stripe-secret-key obj))
        (account-id (stripe-account-id obj))) 
    (when secret-key
      (let ((existing-prod-id (stripe-product-id obj)))
        (if existing-prod-id
            (progn
              (stripe-request secret-key (format nil "/products/~a" existing-prod-id) :post
                              `(("name" . ,(stripe-item-name obj))
                                ("tax_code" . ,(stripe-tax-code obj)))
                              :stripe-account account-id)
              
              (let* ((price-payload `(("product" . ,existing-prod-id)
                                      ("currency" . ,(stripe-item-currency obj))
                                      ("unit_amount" . ,(princ-to-string (stripe-item-price-cents obj)))
                                      ("tax_behavior" . "exclusive")))
                     (price-res (stripe-request secret-key "/prices" :post price-payload :stripe-account account-id)))
                (setf (stripe-price-id obj) (cdr (assoc :id price-res)))))

            (let* ((payload `(("currency" . ,(stripe-item-currency obj))
                              ("unit_amount" . ,(princ-to-string (stripe-item-price-cents obj)))
                              ("tax_behavior" . "exclusive")
                              ("product_data[name]" . ,(stripe-item-name obj))
                              ("product_data[tax_code]" . ,(stripe-tax-code obj))))
                   (res (stripe-request secret-key "/prices" :post payload :stripe-account account-id)))
              
              (setf (stripe-price-id obj) (cdr (assoc :id res)))
              (setf (stripe-product-id obj) (cdr (assoc :product res))))))))
  obj)

;;;
;;; Macro
;;;

(defmacro with-stripe-sync ((mito-obj) &body mutations)
  "Applies mutations, syncs to Stripe, and saves to DB atomically."
  `(progn
     ,@mutations
     (sync-model ,mito-obj)
     (mito:save-dao ,mito-obj)
     ,mito-obj))

;;;
;;; Protocol: Line Item Protocol
;;;
;;; Commodities bought using a Stripe Checkout Session need to
;;; implement this protocol

(defclass stripe-line-item-mixin ()
  ((stripe-price-id :col-type :text
		    :initarg :stripe-price-id
		    :reader stripe-price-id))
  (:metaclass mito:dao-table-mixin)
  (:documentation "Line item mixin for facilitating checkout sessions."))

(defgeneric stripe-line-item-price-id (obj)
  (:documentation "Returns the string `price_...` for the object."))

(defgeneric stripe-line-item-quantity (obj)
  (:documentation "Returns the integer quantity for the object."))

(defun create-checkout-session (secret-key success-url cancel-url line-items reference-id 
				&key stripe-account extra-params)
  (let ((payload `(("success_url" . ,success-url)
		   ("cancel_url" . ,cancel-url)
		   ("mode" . "payment")
		   ("client_reference_id" . ,reference-id)
		   ("automatic_tax[enabled]" . "true"))))
    
    (when extra-params
      (setf payload (append extra-params payload)))
    
    (loop for item in line-items
	  for i from 0
	  do (push (cons (format nil "line_items[~d][price]" i) (stripe-line-item-price-id item)) payload)
	     (push (cons (format nil "line_items[~d][quantity]" i) (princ-to-string (stripe-line-item-quantity item))) payload))
    
    (let ((res (stripe-request secret-key "/checkout/sessions" :post payload :stripe-account stripe-account)))
      (cdr (assoc :url res)))))

(defun verify-stripe-signature (raw-body signature-header endpoint-secret)
  "Cryptographically verifies a Stripe webhook payload to prevent spoofing."
  (let* ((parts (str:split "," signature-header))
         (t-part (find-if (lambda (s) (str:starts-with-p "t=" s)) parts))
         (v1-part (find-if (lambda (s) (str:starts-with-p "v1=" s)) parts)))
    (unless (and t-part v1-part)
      (return-from verify-stripe-signature nil))
    
    (let* ((timestamp (subseq t-part 2))
           (expected-sig (subseq v1-part 3))
           (signed-payload (format nil "~a.~a" timestamp raw-body))
           (hmac (ironclad:make-mac :hmac 
                                    (flexi-streams:string-to-octets endpoint-secret :external-format :utf-8) 
                                    :sha256)))
      
      (ironclad:update-mac hmac (flexi-streams:string-to-octets signed-payload :external-format :utf-8))
      
      (let* ((mac-bytes (ironclad:produce-mac hmac))
             (computed-sig (ironclad:byte-array-to-hex-string mac-bytes)))
        
        (string= expected-sig computed-sig)))))
