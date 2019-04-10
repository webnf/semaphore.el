;;; -*- lexical-binding: t -*-

(require 'f)
(require 'ert)
(require 'ert-async)
(message "Requiring %s" (f-expand  "semaphore.el"))
(require 'semaphore (f-expand  "semaphore.el"))

(defun threaded-test* (f &rest args)
  (let* ((m (make-mutex))
         (cvar (make-condition-variable m))
         (sentinel (gensym))
         ;; [result error backtrace]
         (result (vector sentinel sentinel nil)))
    (make-thread
     (lambda ()
       (condition-case err
           (let ((res (apply f args)))
             (with-mutex m
               (condition-notify cvar)
               (aset result 0 res)))
         (error (let ((bt (with-output-to-string
                            (backtrace))))
                  (with-mutex m
                    (condition-notify cvar)
                    (aset result 1 err)
                    (aset result 2 bt)))))))
    (with-mutex m
      (while (and (eq sentinel (aref result 0))
                  (eq sentinel (aref result 1)))
        (condition-wait cvar))
      (if (eq sentinel (aref result 0))
          (progn
            ;; TODO we need to lift some debugger logic from https://github.com/ohler/ert/blob/c619b56c5bc6a866e33787489545b87d79973205/lisp/emacs-lisp/ert.el
            ;; in order for wrapped stack traces to be useful
            ;; (message "Inner backtrace: \n %s" (aref result 2))
            (apply 'signal (aref result 1)))
        (aref result 0)))))

(defvar test/semaphore/active-count 0)
(defvar test/semaphore/completed-count 0)

(ert-deftest
    test/semaphore/basic-function ()
  (let* ((s (make-semaphore 7))
         (m (make-mutex))
         (complete-c (make-condition-variable m))
         (inc-active (lambda ()
                       (semaphore-acquire s)
                       (with-mutex m
                         (setq test/semaphore/active-count
                               (+ 1 test/semaphore/active-count)))))
         (dec-active (lambda ()
                       (with-mutex m
                         (setq test/semaphore/active-count
                               (- 1 test/semaphore/active-count)))
                       (semaphore-release s)))
         (worker (lambda (i)
                   (make-thread
                    (lambda ()
                      (condition-case err
                          (progn
                            ;; (message "Worker %d: starting" i)
                            (dotimes (n 100)
                              ;; (message "Worker %d: getting semaphore; round %d" i n)
                              (funcall inc-active)
                              (message "Worker %d: increasing active count %d" i test/semaphore/active-count)
                              ;; (message "Worker %d: working" i)
                              (unwind-protect
                                  (sleep-for (* 0.001
                                                (abs (/ (float (random))
                                                        (float most-positive-fixnum)))))
                                ;; (message "Worker %d: getting mutex" i)
                                (with-mutex m
                                  ;; (message "Worker %d: notifying" i)
                                  (condition-notify complete-c)
                                  ;; TODO test for synchronization here
                                  ;; (message "Worker %d: yielding" i)
                                  (thread-yield)
                                  ;; (message "Worker %d: returning semaphore" i)
                                  (funcall dec-active)
                                  ;; (message "Worker %d: increasing completion count" i)
                                  (setq test/semaphore/completed-count
                                        (+ 1 test/semaphore/completed-count)))
                                (thread-yield))))
                        (error (message "ERROR: %s" err))))))))
    (threaded-test*
     (lambda ()
       (dotimes (i 10)
         (funcall worker i))
       (with-mutex m
         (while (> 1000 test/semaphore/completed-count)
           (message ".. incomplete: %s" test/semaphore/completed-count)
           (condition-wait complete-c))
         (message "Complete: %d %d" test/semaphore/completed-count test/semaphore/active-count)
         (should (= 0 test/semaphore/active-count)))))))
