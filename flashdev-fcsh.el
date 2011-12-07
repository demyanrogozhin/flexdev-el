;;; flashdev-fcsh.el --- Flash Compiller SHell mode

;; Author: Denis Martinez <deuns.martinez@gmail.com>
;; Author: Demyan Rogozhin <demyan.rogozhin@gmail.com>

;; This file is a part of flashdev.el.

;;; Commentary: Based on fcsh-mode.el by Denis Martinez

(require 'compile)

(defcustom flashdev-fcsh-buffer "*flashdev-comilation-log*"
  "Buffer name for FCSH output"
  :group 'flashdev)

(defcustom flashdev-fcsh-params-filename "build.el"
  "Name of file in root dir of your project.
 This file must contain at least one FCSH command"
  :group 'flashdev)

(defvar flashdev-fcsh-id nil
  "ID of fcsh build of current project.")

(if (not (boundp 'fcsh-proc))
	(setq fcsh-proc (start-process "flashdev-fcsh" nil (concat flashdev-default-sdk "/fcsh"))))
(if (not (boundp 'flashdev-fcsh-queue))
	(setq flashdev-fcsh-queue (tq-create fcsh-proc)))

(defun flashdev-fcsh-cmd-params (cmd closure)
  (let ((question (concat cmd "\n"))
		(regexp "(fcsh) ")
		(fn 'flashdev-fcsh-handler))
	(tq-enqueue flashdev-fcsh-queue question regexp closure fn t)))

(defun flashdev-fcsh-cmd (cmd)
	  (flashdev-fcsh-cmd-params cmd nil))

(defun flashdev-fcsh-handler (params output)
  (let ((fcsh-return output))
	(if (not (get-buffer flashdev-fcsh-buffer))
		(with-current-buffer (generate-new-buffer flashdev-fcsh-buffer)
		  (make-local-variable 'compilation-error-regexp-alist-alist)
		  (setq compilation-error-regexp-alist-alist
				(list '(flex "^\\(.+\\)(\\([[:digit:]]+\\)): \\([^:]+: \\([[:digit:]]+\\)\\)?" 1 2 4)))
		  (make-local-variable 'compilation-error-regexp-alist)
		  (setq compilation-error-regexp-alist (list 'flex))
		  (compilation-minor-mode)))
	(with-current-buffer (get-buffer flashdev-fcsh-buffer)
	  (setq buffer-read-only nil)
	  (goto-char (point-max))
	  (insert fcsh-return)
	  (delete-backward-char 7)
	  (setq buffer-read-only t)
	  (font-lock-fontify-buffer))))

(defun flashdev-fcsh-get-params ()
"This function must be called from source buffer"
  (let ((params nil)
		(buffer buffer-file-name)
		(file (locate-dominating-file buffer-file-name flashdev-fcsh-params-filename)))
    (with-temp-buffer
      (when file
          (insert-file-contents (concat file flashdev-fcsh-params-filename))
          (setf params (read (current-buffer)))
		  (push (list 'buffer buffer) params)))
    params))

(defun flashdev-fcsh-get-id ()
  (let* ((params (flashdev-fcsh-get-params)))
 	(tq-enqueue flashdev-fcsh-queue "info\n" "(fcsh) "  params 'flashdev-fcsh-info-handler t)))

(defun flashdev-fcsh-info-handler (params info)
(let ((id nil)
	  (buffer (get-file-buffer (second (assq 'buffer params))))
	  (build (format "%s: %s %s"
					 (second (assq 'compiler params))
					 (second (assq 'flags params))
					 (second (assq 'script params)))))
  (with-temp-buffer
	(insert info)
	(search-forward build)
	(forward-line -1)
	(setq id (thing-at-point 'line)))
  (with-current-buffer buffer
	(setq flashdev-fcsh-id id))))

(provide 'flashdev-fcsh)
;;; flashdev-fcsh.el ends here

