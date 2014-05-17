(in-package :alice.core)

(defparameter *max-output-sequence-length* 4)

(defparameter *muted* nil)

;; REPL-utils ?
;; I mean, really, c'mon.
(defun mute ()
  (setf *muted* t))

(defun unmute ()
  (setf *muted* nil))

;; FIXME NOTE REALLY not sure if this file is a good place for it.
;; NOTE maybe, why not; if we drop the idea of centralizing all the "state" in one place, maybe we can store per-destination throttling in hashtables here.
(defparameter *throttled-output* nil "A buffer for throttling the output to avoid flooding the channel.")

;; FIXME how to make it handle announcing (/me) as well?
;; esp. if the data is in the "phrase" part?
;; FIXME don't we want to have this independent from assemble-utterance?
;; FIXME especially if we want to pass another set of phrases?
;; (I guess so, in the end...)
(defun say (to-where what &key to)
  (unless *muted*
  ;; FIXME turn into typecase to catch errors?
    (let ((utterance (assemble-utterance what)))
      (if (listp utterance)
          (mapc (lambda (utt)
                  (irc:say to-where utt :to to))
                (throttle (alexandria:flatten utterance) to-where))
          (irc:say to-where utterance :to to)))))

(defun throttle (messages destination)
  ;; TODO store it in per-destination dict
  (let ((len (length messages)))
    (if (> len *max-output-sequence-length*)
        (let* ((split-point (min *max-output-sequence-length*
                                 len))
               (to-say (subseq messages 0 split-point))
               (to-buffer (subseq messages split-point)))
          (setf *throttled-output* (and (> (length to-buffer) 0) to-buffer)) ;FIXME here goes the destination-keyed dict
          (concatenate 'list to-say (list (assemble-utterance :throttled-message))))
        (progn
          (setf *throttled-output* nil)
          messages))))

