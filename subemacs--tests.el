;; -*- mode:emacs-lisp;coding:utf-8-unix;lexical-binding:t;byte-compile-dynamic:t; -*-

(require 'ert)
(require 'subemacs)


;;;;;; Macros


(defmacro subemacs--test-form (form)
  `(should (equal (subemacs-eval ',form)
                  ,form)))


(defmacro subemacs-progn--test-body (&rest body)
  `(should (equal (subemacs-progn ,@body)
                  (progn ,@body))))


;;;;;; Test cases


(ert-deftest subemacs-1-basic ()
  "Simple test cases for `subemacs-eval'"
  (subemacs--test-form (list 1 2 3))
  (subemacs--test-form load-path)
  (subemacs--test-form (require 'cl-lib))
  (subemacs--test-form (vector :a 1 :b 2
                                    (list (cons 'a 1)
                                          (cons 'b 2))))
  (subemacs-eval `(progn
                    (message "MsgLine 1")
                    (message "MsgLine 2")
                    (message "MsgLine 3")))
  (should (string=
            subemacs-last-stdout
            "MsgLine 1\nMsgLine 2\nMsgLine 3\n")))


(ert-deftest subemacs-2-long-sexps ()
  "Check if evaluating long expressions, where a temp file may be
needed, is supported."
  (let ((long-expression 
         (eval-when-compile
           `(list ,@(cl-loop for i from 0 to 9999 collect i)))))
    (should (equal (subemacs-eval long-expression)
                   (eval long-expression)))))


(ert-deftest subemacs-3-errors ()
  "Does `subemacs-eval' correctly re-signal errors?"
  (should-error (subemacs-eval '(error "foo")) :type 'error)
  (should-error (subemacs-eval '(car "foo")) :type 'wrong-type-argument)
  (put 'myerr 'error-conditions '(myerr error))
  (should-error (subemacs-eval '(progn 
                                  (put 'myerr 'error-conditions '(myerr error))
                                  (signal 'myerr 'foo)))
                :type 'myerr)
  (should-error (subemacs-eval '(progn (cl-check-type "foo" stringp)))
                :type 'void-function)
  (should-error (subemacs-eval '(progn (require 'cl-macs)
                                       (cl-check-type 1 stringp)))
                :type 'wrong-type-argument))


(ert-deftest subemacs-4-unhandleable-error ()
  "If an error is signaled, which does not correctly specify its
'error-conditions, a 'subemacs-error is expected."
  (should-error (subemacs-eval '(signal 'myerr 'foo)
                               :type 'subemacs-error)))

