(define-package "subemacs" "1.0.0"
  "Evaluate emacs lisp code in a clean subprocess.

Functions that utilize this ability to byte compile emacs lisp in
a clean environment where nothing may shadow byte-compiler
warnings are provided."
  '((emacs "24.4")))
