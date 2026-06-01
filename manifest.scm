;; This manifest defines the development env for this project.
;; Run 'guix shell' in this directory to activate it.

(specifications->manifest
 (list "sbcl"
       "git"
       "cl-cffi" 
       "openssl" ; Required for :dexador and :hunchentoot (SSL)
       ;; DB Foreign Libraries
       "sqlite"
       "postgresql"
       ;; Image Foreign Libraries (for :imago)
       "libpng"
       "libjpeg-turbo"))
