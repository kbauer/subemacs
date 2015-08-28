;; -*- mode:emacs-lisp;coding:utf-8-unix;lexical-binding:t;byte-compile-dynamic:t; -*-

(require 'cl-macs)


;;; TODO Consider option to run (load load-file-name) by default. 

;;;; Auxiliary Functions 


(defun subemacs--binary-path ()
  "Path to emacs executable of this emacs process if known"
  (concat 
   (or invocation-directory
       (error "Cannot determine path to emacs executable"))
   (or invocation-name
       (error "Cannot determine path to emacs executable"))))


(defun subemacs--output-buffer ()
  (get-buffer-create " *subemacs-eval*"))


(defun subemacs--reraise-error (err)
  (signal (car err) (cdr err)))


(defun subemacs--1-call-direct (form)
  (call-process (subemacs--binary-path) nil (subemacs--output-buffer) nil 
                "--batch" "--eval" (format "%S" form)))


(defun subemacs--2-call-with-tempfile (form)
  (let ((temp-file (make-temp-file "subemacs-eval-input-file-" nil ".el.tmp")))
    (unwind-protect
        (progn 
          (with-temp-file temp-file
            (print form (current-buffer)))
          (call-process (subemacs--binary-path) nil (subemacs--output-buffer) nil
                        "--batch" "--load" temp-file))
      (delete-file temp-file))))


(defconst subemacs--inherit-vars 
  (list 'load-path))


(defun subemacs--make-form (form)
  "Internal;
Create the full form passed to the subprocess for `subemacs-eval'
Passes values to parent process as alist."
  `(progn 
     ,@(cl-loop for var in subemacs--inherit-vars
                collect `(setq ,var ',(symbol-value var)))
     (print 
      (let ((subemacs-eval-error t))
        (unwind-protect
            (condition-case err
                (prog1 (list (cons 'value (eval ',form)))
                  (setq subemacs-eval-error nil))
              (error 
               (prog1 (list (cons 'error err))
                 (setq subemacs-eval-error nil))))
          (when subemacs-eval-error
            (print (cons 'error 
                         (list 'error 
                               (cons 'reason 'unknown-error-type)
                               (cons 'form ',form))))
            (kill-emacs 1)))))))


(put 'subemacs-eval-error 'error-conditions '(subemacs-eval-error error))


(defun subemacs--readable-form-p (form)
  "Is FORM a sexp, that can be `read' from its `print' representation?"
  (condition-case nil
      (prog1 t
        (equal form
               (with-temp-buffer 
                 (print form (current-buffer))
                 (goto-char (point-min))
                 (read (current-buffer)))))
    (invalid-read-syntax nil)))


(defun subemacs--read--expression (prompt &optional initial-contents)
  ;; Copied from `read--expression' in `simple.el' because I didn't
  ;; want to depend on an undocumented, possibly changing function. 
  (let ((minibuffer-completing-symbol t))
    (minibuffer-with-setup-hook
        (lambda ()
          (add-hook 'completion-at-point-functions
                    #'lisp-completion-at-point nil t)
          (run-hooks 'eval-expression-minibuffer-setup-hook))
      (read-from-minibuffer prompt initial-contents
                            read-expression-map t
                            'read-expression-history))))


(eval-and-compile
  (defun subemacs--assert (value predicate)
    (unless (funcall predicate value)
      (error "Assertion failed: %S, %S" value predicate))))


(defmacro subemacs--function-let (bindings &rest body)
  "Temporarily change bindings of functions in dynamic (i.e. global) scope.

Bindings uses as syntax

   BINDINGS = (BINDFORM ...)
   BINDFORM = (SYMBOL FUNCTION)

where SYMBOL identifies the function to be rebound and FUNCTION
its replacement (which may be a symbol).

Beware: Dangerous. Used on fundamental functions like `car' may
break the emacs session."
  (declare (indent 1))
  (cl-loop for (symbol function) in bindings 
           do (subemacs--assert symbol #'symbolp)
           do (subemacs--assert (eval function)
                                     (lambda (e) 
                                       (or (symbolp e) (functionp e)))))
  `(subemacs--function-let-1 
    (list ,@(cl-loop for (symbol function) in bindings 
                    collect `(cons #',symbol ,function)))
    (lambda () ,@body)))


(defun subemacs--function-let-1 (binding-alist body-func)
  (let ((original-bindings-alist 
         (cl-loop for (symbol . _) in binding-alist 
                  collect (cons symbol (symbol-function symbol)))))
    (unwind-protect 
        (cl-loop for (symbol . newfunc) in binding-alist
                 do (fset symbol newfunc)
                 finally return (funcall body-func))
      (cl-loop for (symbol . oldfunc) in original-bindings-alist
               do (fset symbol oldfunc)))))


;;;; Public functions and commands 


(defun subemacs-eval (form)
  "Evaluate FORM in an emacs subprocess.

FORM will be passed as a string to a new emacs session and
evaluated there. The result will be returned to the current
session as return value of the `subemacs-eval' function call.

When the form results in an error, that error is collected and
`signal'ed in the current session.

The `print' representation of FORM must be `read'able, which is
veryfied before starting a subprocess. 

The subprocess session is started with the --batch argument and
thus doesn't process the `.emacs' file. 

It automatically inherits the `load-path' of the current session.
It does not however inherit any other variable values or function
definitions. These have to be supplied by either `require'ing the
needed libraries (which is why the `load-path' is supplied by
default) or by explicitly constructing FORM to set them, e.g.

    \`(let ((remote-value ',local-value))
        (do-it remote-value))

The output of the subprocess that is NOT part of the final value
is forwarded to the `*Messages*' buffer.


Additional Remarks 
------------------

Do NOT try something like `(funcall (function ,(lambda () ...))).
Currently this construct would pass a closure to the subprocess
which carries along its lexical environment, but this may break
in future implementations of lexical closures.

When FORM is very long, limitations of the system level ‘exec’
interface may prevent passing FORM as an argument to the
subprocess. In this case a temporary file is used. Even on
Windows however, that limit should be in the range of tens of
thousands of characters. "
  (cl-check-type form subemacs--readable-form-p)
  ;; The use of a fixed buffer is for debugging primarily. 
  ;; It may be changed to a temporary buffer at any time. 
  (with-current-buffer (subemacs--output-buffer)
    (erase-buffer)
    (condition-case nil
        (subemacs--1-call-direct (subemacs--make-form form))
      (file-error 
       (subemacs--2-call-with-tempfile (subemacs--make-form form))))
    (let ((returned-form 
           (progn (goto-char (point-max))
                  (backward-sexp)
                  (read (current-buffer))))
          (message-contents
           (progn (goto-char (point-max))
                  (backward-sexp)
                  (buffer-substring-no-properties (point-min) (point)))))
      (message "%s" message-contents)
      (cond ((assq 'error returned-form)
             (subemacs--reraise-error (cdr (assq 'error returned-form))))
            ((assq 'value returned-form)
             (cdr (assq 'value returned-form)))
            (t (signal 'error
                       (list "Invalid form returned from subemacs"
                             returned-form)))))))


(defun subemacs-eval-expression (exp &optional insert-value)
  "Like `eval-expression' but performing the evaluation through
`subemacs-eval'. Shares the history with `eval-expression'."
  (interactive 
   (list (subemacs--read--expression "Subemacs-eval: ")
         current-prefix-arg))
  (let ((debug-on-error t))
    (eval-expression (list 'quote (subemacs-eval exp)) 
                     insert-value)))


(defun subemacs-eval-byte-compile-file (filename &optional load)
  "Like `byte-compile-file' but evaluated through `subemacs-eval'.

I.e. the file is compiled in a clean emacs session and has to
`require' all `features' it depends on itself.

This makes identifying missing `require's easier, as they will
raise compilation warnings, while in the main emacs sessions
dependencies loaded previously by other files will mask the
error.

"
  ;; Interactive form copied from `byte-compile-file'
  (interactive
   (let ((file buffer-file-name)
	 (file-dir nil))
     (and file
	  (derived-mode-p 'emacs-lisp-mode)
	  (setq file-dir (file-name-directory file)))
     (list (read-file-name (if current-prefix-arg
			       "Byte compile and load file: "
			     "Byte compile file: ")
			   file-dir buffer-file-name nil)
	   current-prefix-arg)))
  (let ((return
         (subemacs-eval
          `(progn
             (require 'bytecomp)
             (list
              (list 'return-value
                    (byte-compile-file ',filename nil))
              (list 'log-contents
                    (with-current-buffer (get-buffer-create byte-compile-log-buffer)
                      (buffer-string))))))))
    (with-current-buffer (get-buffer-create byte-compile-log-buffer)
      (let ((inhibit-read-only t))
        (with-selected-window (display-buffer (current-buffer))
          (goto-char (point-max))
          ;; (recenter 0)
          (insert (format "(%s) %s\n" (format-time-string "%T" (current-time)) filename))
          (insert (cadr (assq 'log-contents return)))
          (unless (eq major-mode 'compilation-mode)
            (compilation-mode)))))
    (when (and load
               (cadr (assq 'return-value return)))
      (load-file (byte-compile-dest-file filename)))
    (cadr (assq 'return-value return))))


(defun subemacs-eval-byte-recompile-directory (directory &optional arg force)
  "Like `byte-recompile-directory' but using
`subemacs-eval-byte-compile-file' insteaf of `byte-compile-file'."
  (interactive "DByte recompile directory: \nP")
  (subemacs--function-let 
      ((byte-compile-file #'subemacs-eval-byte-compile-file))
    (byte-recompile-directory directory arg force)))


;;;; Removed Forms 
;;;
;;; These forms may break without warning, as closures carrying along
;;; their lexical environment even to a subprocess is not documented
;;; and may depend on the current implementation in a not future-proof
;;; manner, so I removed them but left them in for reference. 
;;
;; (defun subemacs-eval-function (function)
;;   "Removed. Closures may allow 
;;   (subemacs-eval `(funcall #',function)))
;; 
;; 
;; (defmacro subemacs-eval-progn (&rest body)
;;   "doc"
;;   (declare (indent 0))
;;   `(subemacs-eval-function 
;;     (lambda () "Generated by `subemacs-eval-progn'" 
;;       ,@body)))
  
    



(provide 'subemacs-eval)
