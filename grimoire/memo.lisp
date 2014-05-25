(in-package #:alice.grimoire)

;; MEMOS
;; FIXME move this somewhere?
(defvar *memos* (make-hash-table :test 'equalp))

(defun make-memo (channel who what from-who &optional (timestamp (local-time:now)))
  (let ((target (alice.core:identify-person-canonical-name who)))
     (when target (list channel (alice.core:identify-person-canonical-name who) what from-who timestamp))))

(defun memo-to-string (memo)
  (format nil "Wiadomość od ~A nadana ~A o ~A ⇒ ~A" (fourth memo) (alice.language:format-date (fifth memo)) (alice.language:format-time (fifth memo)) (third memo)))

(defun save-memo (memo)
  "Save a memo for user."
  (let ((memos (gethash (second memo) *memos*)))
    (setf (gethash (second memo) *memos*)
          (append memos (list memo)))))

(defun make-memo-matcher (user destination)
  (lambda (memo)
    (and (equalp destination (first memo))
         (equalp user (second memo)))))

(defun find-matching-memos (user destination memos)
  (remove-if-not (make-memo-matcher user destination)
                 memos))

(defun remove-memo (memo memos)
  (remove-if (make-memo-matcher (second memo) (first memo))
             memos
             :count 1))

(defun check-for-memos (destination for-who)
  "See if user `FROM-WHO' writing at `DESTINATION' has any pending memos and if so, grab the first one and write it to him/her.
Also check for private memos (sent by query), and if any found, send it to him/her in private."
  (let ((who (alice.core:identify-person-canonical-name for-who)))
    (labels ((dispatch-memo (to-where to-who memo more?)
               (alice.core:say to-where (memo-to-string memo) :to to-who)
               (when more?
                 (alice.core:say to-where :more-memos :to to-who)))

             (handle-memos (from-where to-where to-who)
               "Find a first matching memo, dispatch it and remove from memo list."
               (let* ((all-memos (gethash who *memos*))
                      (matching-memos (find-matching-memos who from-where all-memos))
                      (memo (first matching-memos)))
                 (when memo
                   (setf (gethash who *memos*) (remove-memo memo all-memos))
                   (dispatch-memo to-where to-who memo (> (length matching-memos) 1))))))

      (handle-memos destination destination for-who) ;public memos
      (handle-memos nil for-who nil))))              ;private memos



(defun notify-via-memo (channel who what from-who is-private)
  "Create a memo for `WHO' on `CHANNEL' containing the text `WHAT' from person `FROM-WHO'."
  (let ((memo (make-memo (and (not is-private) channel)
                         who what from-who)))
    (if memo
        (progn (save-memo memo)
               :memo-saved)
        :memo-failed)))
