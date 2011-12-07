;;; flashdev-fcsh.el --- Flex Compiller SHell mode

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

(defun flashdev-fcsh-run ()
;; (process-status fcsh-proc)
	  (setq fcsh-proc (start-process "flashdev-fcsh" nil (concat flashdev-default-sdk "/bin/fcsh")))
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
	  (setq buffer-read-only t))))

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

(defun flashdev-fcsh-add-build ()
  (let* ((params (flashdev-fcsh-get-params))
		 (build (format "%s %s -- %s"
					 (second (assq 'compiler params))
					 (second (assq 'flags params))
					 (second (assq 'script params)))))
	(flashdev-fcsh-cmd build)))

(defun flashdev-fcsh-get-id ()
  (let* ((params (flashdev-fcsh-get-params)))
 	(tq-enqueue flashdev-fcsh-queue "info\n" "(fcsh) "  params 'flashdev-fcsh-info-handler t)))

(defun flashdev-fcsh-info-handler (params info)
  (let* ((id nil)
		 (buffer (get-file-buffer (second (assq 'buffer params))))
		 (build (format "%s: %s -- %s"
						(second (assq 'compiler params))
						(second (assq 'flags params))
						(second (assq 'script params))))
		 (pos (string-match
			   (concat "\\([[:digit:]]+\\)\\(?:\n.*\\)"
					   (regexp-quote build)) info))
		 (num (match-string 1 info)))

		(with-current-buffer buffer
		  (setq id (if num (string-to-int num) 0))
		  (if (and (boundp flashdev-fcsh-id)
				   (not (= id flashdev-fcsh-id)))
			  (make-local-variable flashdev-fcsh-id))
		  (setq flashdev-fcsh-id id)
		  (cond ((= 0 flashdev-fcsh-id)
				 (if (locate-dominating-file buffer-file-name flashdev-fcsh-params-filename)
					 (progn
					   (flashdev-fcsh-add-build)
					   (flashdev-fcsh-get-id))
				   (error "File build.el not found")))))))

(defun flashdev-fcsh-build ()
  (interactive)
  (with-current-buffer
	  (if (not flashdev-fcsh-id)
			 (flashdev-fcsh-get-id)
		(flashdev-fcsh-cmd (concat "compile " (number-to-string flashdev-fcsh-id))))))

(provide 'flashdev-fcsh)
;;; flashdev-fcsh.el ends here

