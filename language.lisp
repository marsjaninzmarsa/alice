(defpackage #:alice.language
  (:use #:cl)
  (:export #:stem-matches-p))

(in-package #:alice.language)

(defconstant +stems+ "(y|i|a|ie|owi|e|ę|iowi)")

;; functions related to language processing

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
  (concatenate 'string "^" base-word +stems+ "$"))

(defun matches-regexp-p (regexp string)
  (not (null (cl-ppcre:scan regexp string))))
