(in-package #:cfbee.stripe)


(defparameter *stripe-api-url* "https://api.stripe.com/v1"
  "Stripe API URL.")


(defclass stripe-integration-mixin ()
  ((tax-code :initarg :tax-code
             :accessor stripe-tax-code
             :col-type (or (:varchar 255) :null)
             :initform "txcd_99999999")
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
;;; Systems consuming the Stripe library must implement these for
;;; their models.

(defgeneric stripe-item-name (obj))
(defgeneric stripe-item-price-cents (obj))
(defgeneric stripe-item-currency (obj))
(defgeneric stripe-merchant-secret-key (obj))

;;;
;;; 
;;;

(defun stripe-request (secret-key endpoint method data)
  (let ((url (concatenate 'string *stripe-api-url* endpoint))
	(headers '(("Stripe-Version" . "2026-03-25.preview"))))
    (cl-json:decode-json-from-string
     (dex:request url
		  :method method
		  ;; Basic Auth expects ("username" . "password")
		  ;; stripe uses secret as username so empty password
		  :basic-auth (cons secret-key "")
		  :headers headers
		  ;; Clean up nil values
		  :content (when data 
			     (remove-if (lambda (pair) (null (cdr pair)))
					data))))))

(defun sync-model (obj)
  "Syncs any model that implements the Stripe protocol."
  (let ((secret-key (stripe-merchant-secret-key obj)))
    (when secret-key
      (let ((existing-prod-id (stripe-product-id obj)))
	(if existing-prod-id
	    (progn
	      (stripe-request secret-key 
			      (format nil "/products/~a" existing-prod-id) 
			      :post
			      `(("name" . ,(stripe-item-name obj))
				("tax_code" . ,(stripe-tax-code obj))))
	      
	      (let* ((price-payload `(("product" . ,existing-prod-id)
				      ("currency" . ,(stripe-item-currency obj))
				      ("unit_amount" . ,(princ-to-string
							 (stripe-item-price-cents obj)))
				      ("tax_behavior" . "exclusive")))
		     (price-res (stripe-request secret-key "/prices" :post price-payload)))
		(setf (stripe-price-id obj)
		      (cdr (assoc :id price-res)))))

	    (let* ((payload `(("currency" . ,(stripe-item-currency obj))
			      ("unit_amount" . ,(princ-to-string
						 (stripe-item-price-cents obj)))
			      ("tax_behavior" . "exclusive")
			      ("product_data[name]" . ,(stripe-item-name obj))
			      ("product_data[tax_code]" . ,(stripe-tax-code obj))))
		   (res (stripe-request secret-key "/prices" :post payload)))
	      
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

(defun create-checkout-session (merchant-secret-key success-url cancel-url line-items reference-id &rest extra-params)
  "Creates a tax-aware Stripe checkout session. EXTRA-PARAMS accepts arbitrary alist pairs for Stripe options."
  (let ((payload `(("success_url" . ,success-url)
                   ("cancel_url" . ,cancel-url)
                   ("mode" . "payment")
                   ("client_reference_id" . ,reference-id)
                   ("automatic_tax[enabled]" . "true"))))
    
    ;; Append arbitrary extra parameters (like customer_email, shipping_options, etc.)
    (when extra-params
      (setf payload (append extra-params payload)))
    
    ;; Stripe requires arrays formatted as: line_items[0][price]=price_123
    (loop for item in line-items
          for i from 0
          do (push (cons (format nil "line_items[~d][price]" i) 
                         (stripe-line-item-price-id item)) 
                   payload)
             (push (cons (format nil "line_items[~d][quantity]" i) 
                         (princ-to-string (stripe-line-item-quantity item))) 
                   payload))
    
    (let ((res (stripe-request merchant-secret-key "/checkout/sessions" :post payload)))
      ;; Return the Stripe-hosted checkout URL
      (cdr (assoc :url res)))))
