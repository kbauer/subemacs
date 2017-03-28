;; -*- lexical-binding: t; lisp-indent-offset: nil -*-

(require 'subemacs)
(require 'cl-macs)



(cl-defun subemacs--make-process (&key form)
  (let((form-effective (subemacs--make-form form))
       (binary (subemacs--binary-path)))
    (make-process 
      :name "subemacs"
      :command (list 
                 (subemacs--binary-path)
                 "--batch" "--eval" (format "%S" form-effective))
      :stderr 
      (make-pipe-process 
        :name "subemacs-stderr"
        :filter (lambda (_ s)
                  (message "%s" 
                    (if (string-match-p "\n\\'" s)
                        (substring s 0 -1)
                      s))))
      :filter 
      (lambda (process string)
        
        
        
        
    


(while t
  (mu-timing (async-sandbox `(lambda () (executable-find "emacs"))))
  (with-selected-window (display-buffer (messages-buffer))
    (goto-char (point-max))
    (recenter -2))
  (REDISPLAY))







;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8-unix
;; byte-compile-dynamic: t
;; End:


