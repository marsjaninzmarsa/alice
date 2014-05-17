(in-package :alice.utils)

(define-constant +regexp-special-characters+ ".^$*+?()[{\|\\" :test #'string=)
(define-constant
    +regexp-escape-special-characters-regexp-part+
    (concatenate 'string
                 "(["
                 (coerce (mapcan (lambda (x) (list #\\ x))
                                 (coerce +regexp-special-characters+ 'list))
                         'string)
                 "])")
  :test #'string=)

(defun escape-for-regexp (text)
  (cl-ppcre:regex-replace-all +regexp-escape-special-characters-regexp-part+ text "\\\\\\1"))

(defun matches-regexp-p (regexp string)
  (not (null (cl-ppcre:scan regexp string))))

(defun extract-words (text)
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[\\w\\|]{2,}" text) ;FIXME since this function is general, should we really limit ousrelves to 2+-char deduplicated words?
   :test #'string=))
