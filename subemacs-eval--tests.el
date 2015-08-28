;; -*- mode:emacs-lisp;coding:utf-8-unix;lexical-binding:t;byte-compile-dynamic:t; -*-

(require 'ert)
(require 'subemacs-eval)


;;;;;; Macros


(defmacro subemacs-eval--test-form (form)
  `(should (equal (subemacs-eval ',form)
                  ,form)))


(defmacro subemacs-eval-progn--test-body (&rest body)
  `(should (equal (subemacs-eval-progn ,@body)
                  (progn ,@body))))


;;;;;; Test cases


(ert-deftest subemacs-eval-1-basic ()
  "Simple test cases for `subemacs-eval'"
  (subemacs-eval--test-form (list 1 2 3))
  (subemacs-eval--test-form load-path)
  (subemacs-eval--test-form (require 'cl-lib))
  (subemacs-eval--test-form (vector :a 1 :b 2
                                    (list (cons 'a 1)
                                          (cons 'b 2)))))


(ert-deftest subemacs-eval-2-long-sexps ()
  "Check if evaluating long expressions, where a temp file may be
needed, is supported."
  (let ((long-expression 
         (eval-when-compile
           `(list ,@(cl-loop for i from 0 to 9999 collect i)))))
    (should (equal (subemacs-eval long-expression)
                   (eval long-expression)))))


(ert-deftest subemacs-eval-3-errors ()
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


(ert-deftest subemacs-eval-4-unhandleable-error ()
  "If an error is signaled, which does not correctly specify its
'error-conditions, a 'subemacs-eval-error is expected."
  (should-error (subemacs-eval '(signal 'myerr 'foo)
                               :type 'subemacs-eval-error)))

