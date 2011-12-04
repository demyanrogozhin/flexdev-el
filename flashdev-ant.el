;;; flashdev-ant.el --- Support for Apache Ant for Flash development

;; Author: Demyan Rogozhin <demyan.rogozhin@gmail.com>

;; This file is a part of flashdev.el.

(require 'compile)

(defun flashdev-ant-build (&optional target)
  "Call apache Ant via M-x comile and highlight errors."
  (interactive)
  (let* ((target (or target "main"))
         (compilation-error-regexp-alist-alist
          (list
           '(flex
             "^\\(.+\\)(\\([[:digit:]]+\\)): \\([^:]+: \\([[:digit:]]+\\)\\)?"
             1 2 4)))
         (compilation-error-regexp-alist
          (list
           'flex)))
    (compilation-start (concat "ant -emacs -s build.xml " target))))

(defun flashdev-ant-target (target)
  (interactive "MAnt Target: ")
  (flashdev-ant-build target))

(provide 'flashdev-ant)
;;; flashdev-ant.el ends here
