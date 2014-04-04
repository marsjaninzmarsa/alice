(defpackage #:alice.language
  (:use #:cl)
  (:export #:extract-words
           #:stem-matches-p))

(in-package #:alice.language)


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

(define-constant +stems+ "(y|i|a|ie|owi|e|ę|iowi)" :test #'string=)

;; functions related to language processing

(defun extract-words (text)
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "[\\w\\|]{2,}" text)
   :test #'string=))

(defun stem-matches-p (word-checked target)
  "Check if `WORD-CHECKED' matches `TARGET', disregarding most popular inflection forms in Polish language. Eg. 'Janowi' should match 'Jan'."
  (and (stringp word-checked)
       (stringp target)
       (let* ((lowcase-word-checked (string-downcase word-checked))
              (lowcase-target (string-downcase target))
              (len (length lowcase-target))
              (suffixless-target (subseq lowcase-target 0 (- len 1))))
         (or (equalp lowcase-word-checked lowcase-target)
             (equalp lowcase-word-checked suffixless-target)
             (matches-regexp-p (make-stem-regexp lowcase-target) lowcase-word-checked)
             (matches-regexp-p (make-stem-regexp suffixless-target) lowcase-word-checked)))))


(defun make-stem-regexp (base-word)
  "Make a regexp that matches `BASE-WORD' extended by most common polish inflection suffixes."
  (concatenate 'string "^" (escape-for-regexp base-word) +stems+ "$"))

(defun matches-regexp-p (regexp string)
  (not (null (cl-ppcre:scan regexp string))))

(defun escape-for-regexp (text)
  (cl-ppcre:regex-replace-all +regexp-escape-special-characters-regexp-part+ text "\\\\\\1"))
