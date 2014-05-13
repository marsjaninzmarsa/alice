(in-package #:alice.grimoire)

(defparameter *wolfram-query-regexp* "\"(.*)\"" "A regexp to extract question part when performing Wolfram|Alpha search.")

(defun parse-message-for-wolfram-computation (text)
  (cl-ppcre:scan-to-strings *wolfram-query-regexp* text))
