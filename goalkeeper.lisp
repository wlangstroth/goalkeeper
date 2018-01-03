;;;; goalkeeper.lisp

(in-package #:goalkeeper)

(defvar *goals* nil)

(defparameter *goals-filename* "goals.db")
(defparameter *goals-to-display* 6)

(defconstant +available-minutes-per-day+ 150)

(defstruct (goal)
  "A goal"
  created
  deadline
  estimated-time ;; in minutes
  happiness
  sadness-avoided
  description
  notes
  context
  completed)

(defun seconds-to-deadline (goal)
  (-
   (chronograph:universal-from-iso (goal-deadline goal))
   (get-universal-time)))

(defun days-to-deadline (goal)
  (floor (seconds-to-deadline goal) 86400))

(defun goal-time-available (goal)
  (1+ (* (days-to-deadline goal) +available-minutes-per-day+)))

(defun goal-incomplete (goal)
  (null (goal-completed goal)))

(defun goal-utility (goal)
  (+ (goal-happiness goal) (goal-sadness-avoided goal)))

(defun urgency (goal)
  (/
   (float (goal-utility goal))
   (* (log (goal-estimated-time goal)) (goal-time-available goal))))

(defun more-urgent (first-goal second-goal)
  (> (urgency first-goal) (urgency second-goal)))

(defun incomplete-goals ()
  (remove-if-not #'goal-incomplete (copy-seq *goals*)))

(defun later-completion (first-goal second-goal)
  (string> (goal-completed first-goal) (goal-completed second-goal)))

(defun completed-goals ()
  (sort
   (remove-if #'goal-incomplete (copy-seq *goals*))
   #'later-completion))

(defun complete-goal (goal)
  (setf (goal-completed goal) (chronograph:iso-now)))

(defun goals-by-urgency ()
  (sort (incomplete-goals) #'more-urgent))

(defun priority-goals (top-n)
  "Returns the top goals"
  (subseq (goals-by-urgency) 0 top-n))

(defun complete-top ()
  (complete-goal (first (priority-goals 1))))

(defun complete-goal-id (creation-stamp)
  (first (select (where :created creation-stamp))))

(defun display-goal-description (goal)
  (format t "• ~a~%" (goal-description goal)))

(defun display-goals ()
  (dolist (goal *goals*)
    (display-goal-description goal)))

(defun header-row () nil)

(defun limited-description (text limit)
  (coerce
   (loop
      for char in (coerce text 'list)
      for i from 1 to limit
      collecting char)
   'string))

(defun goal-row (goal)
  (format t "~a | ~3$ | ~a~%"
          (goal-deadline goal)
          (urgency goal)
          (limited-description (goal-description goal) 50)))

(defun display-priority-goals (top-n)
  (dolist (goal (priority-goals top-n))
    (display-goal-description goal)))

(defun display-goals-by-urgency ()
  (dolist (goal (goals-by-urgency))
    (goal-row goal))
  (length (goals-by-urgency)))

(defun display-completed-goals ()
  (dolist (goal (completed-goals))
    (format t "~a | ~a~%" (goal-completed goal) (goal-description goal))))

(defun priorities ()
  (display-priority-goals *goals-to-display*))

(defun where (&key created description deadline completed)
  #'(lambda (goal)
      (or
       (equal (goal-created goal) created)
       (equal (goal-description goal) description)
       (equal (goal-deadline goal) deadline)
       (equal (goal-completed goal) completed))))

(defun select (selector-fn)
  (remove-if-not selector-fn *goals*))

(defun description-search (search-string)
  (remove-if-not
   #'(lambda (goal)
       (cl-string-match:string-contains-kmp
        (string-downcase search-string)
        (string-downcase (goal-description goal))))
   *goals*))

(defun push-deadlines ()
  (mapcar
   #'(lambda (goal)
       (setf (goal-deadline goal)
             (chronograph:iso-from-universal
              (+ 86400 (chronograph:universal-from-iso (goal-deadline goal))))))
  *goals*))


(defun save-goals ()
  (with-open-file (out *goals-filename*
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *goals* out)))
  (format t "~a / ~a" (length (incomplete-goals)) (length *goals*)))

(defun load-goals ()
  (with-open-file (in *goals-filename*)
    (with-standard-io-syntax
      (setf *goals* (read in))))
  (length *goals*))

(defun add-goal (goal)
  (push goal *goals*)
  (save-goals))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-goal ()
  (make-goal
   :description (prompt-read "Description")
   :deadline (prompt-read "Deadline")
   :estimated-time
   (or (parse-integer (prompt-read "Estimated Time") :junk-allowed t) 1)
   :happiness
   (or (parse-integer (prompt-read "Happiness created") :junk-allowed t) 1)
   :sadness-avoided
   (or (parse-integer (prompt-read "Sadness avoided") :junk-allowed t) 1)
   :notes (prompt-read "Notes")
   :created (get-universal-time)))

(defun add-goals ()
  (loop (add-goal (prompt-for-goal))
      (if (not (y-or-n-p "Another? [y/n]: ")) (return))))


(load-goals)
