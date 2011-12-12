;;; flashdev-fcsh.el --- Flex Compiller SHell mode

;; Author: Denis Martinez <deuns.martinez@gmail.com>
;; Author: Demyan Rogozhin <demyan.rogozhin@gmail.com>

;; This file is a part of flashdev.el.

;;; Commentary: Based on fcsh-mode.el by Denis Martinez

(require 'compile)
(require 'tq)

(defcustom flashdev-fcsh-buffer "*flashdev-comilation-log*"
  "Buffer name for FCSH output"
  :group 'flashdev)

(defcustom flashdev-fcsh-params-filename "build.el"
  "Name of file in root dir of your project.
 This file must contain at least one FCSH command"
  :group 'flashdev)

(defcustom flashdev-fcsh-regexp  "(fcsh) "
  "Prompt regexp in FCSH command"
  :group 'flashdev)

(defvar flashdev-fcsh-id)
(setq dbgcount 0)
(defun mydbg (str) (display-message-or-buffer (format "=%s= %s" dbgcount str)) (incf dbgcount)
  ;; (if (> dbgcount 12) (tq-close flashdev-fcsh-queue))
  )

(defun flashdev-fcsh-run ()
  "Start routine for fcsh process and transaction queue.
This function creates starts FCSH and `flashdev-fcsh-queue' - Transaction Queue for FCSH process"
	  (setq fcsh-proc (start-process "flashdev-fcsh" nil (concat flashdev-default-sdk "/bin/fcsh")))
	  (setq flashdev-fcsh-queue (tq-create fcsh-proc))
	  (tq-enqueue flashdev-fcsh-queue "" flashdev-fcsh-regexp nil 'flashdev-fcsh-handler t))

(defun flashdev-fcsh-cmd-params (cmd closure)
  "Adds command to `flashdev-fcsh-queue'.
Function `flashdev-fcsh-handler' called right after command executed."
  (let ((question (concat cmd "\n"))
		(regexp flashdev-fcsh-regexp)
		(fn 'flashdev-fcsh-handler))
	(tq-enqueue flashdev-fcsh-queue question regexp closure fn t)))

(defun flashdev-fcsh-cmd (cmd) "Stupid shortcut to `flashdev-fcsh-cmd-params'"
	  (flashdev-fcsh-cmd-params cmd nil))

(defun flashdev-fcsh-handler (params info)
  (let ((fcsh-return info))
	(mydbg (format "fcsh: params = %s; info: %s" params info))
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
		(dir (locate-dominating-file buffer-file-name flashdev-fcsh-params-filename)))
    (with-temp-buffer
      (if dir
		  (progn
			(insert-file-contents (concat dir flashdev-fcsh-params-filename))
			(setf params (read (current-buffer)))
			(push (list 'buffer buffer) params))))
    params))

(defun flashdev-fcsh-add-build (&optional build params)
"Add build new command to FCSH. "
  (let* ((params (if params params
				   (flashdev-fcsh-get-params)))
		 (build (format "%s %s -- %s"
						(second (assq 'compiler params))
						(second (assq 'flags params))
						(second (assq 'script params)))))
	(mydbg (format "fcsh: build-add! %s params = %s" build params))
	(flashdev-fcsh-cmd build)))
; 	(tq-enqueue flashdev-fcsh-queue  (concat build "\n") flashdev-fcsh-regexp params 'flashdev-fcsh-add-handler t)))

(defun flashdev-fcsh-add-handler (params info)
	(mydbg (format "fcsh: after-add! params = %s; output %s" params info)))

(defun flashdev-fcsh-get-id (&optional params)
  "Sends 'info' to FCSH. Output handled by `flashdev-fcsh-info-handler'"
  (let* ((params (if params params (flashdev-fcsh-get-params))))
	(mydbg (format "fcsh: get-info! %s" params))
 	(tq-enqueue flashdev-fcsh-queue "info\n" flashdev-fcsh-regexp  params 'flashdev-fcsh-info-handler t)))

(defun flashdev-fcsh-info-handler (params info)
  "This callback searchachs for ID in FCSH info output. Handler for `flashdev-fcsh-get-id'"
  (let* ((id nil)
		 (buffer (get-file-buffer (second (assq 'buffer params))))
		 (build (format "%s: %s -- %s"
						(second (assq 'compiler params))
						(second (assq 'flags params))
						(second (assq 'script params))))
		 (pos (string-match
			   (concat "id: \\([[:digit:]]+\\)\\(?:\n.*\\)"
					   (regexp-quote build)) info))
		 (num (match-string 1 info)))
		(with-current-buffer buffer
		  (setq id (if num (string-to-int num) 0))
		  (mydbg (format "fcsh: after-info! build = %s; params = %s; output: %s" id params info))
		  (cond ((zerop id)
				   (flashdev-fcsh-add-build params))
				;; ((and (boundp 'flashdev-fcsh-id)
				;;    (not (= id flashdev-fcsh-id)))
				;;    (make-local-variable flashdev-fcsh-id)))
				(t (setq flashdev-fcsh-id id))))))


(defun flashdev-fcsh-build ()
  "Finnaly send 'compile N' command where N is `flashdev-fcsh-id' in current buffer.
If `flashdev-fcsh-id' is unset try to find out what's build ID of this project."
  (interactive)
  (if (not (boundp 'flashdev-fcsh-queue))
	  (flashdev-fcsh-run)
	(if (not (consp flashdev-fcsh-queue))
		(error "Something wron with queue: " flashdev-fcsh-queue)))
  (with-current-buffer (current-buffer)
	  (if (not (boundp 'flashdev-fcsh-id))
			 (flashdev-fcsh-get-id)
		(if (zerop flashdev-fcsh-id)
			 (flashdev-fcsh-get-id)
		  (progn
			(mydbg  (concat "fcsh: compile " (number-to-string flashdev-fcsh-id)))
			(flashdev-fcsh-cmd (concat "compile " (number-to-string flashdev-fcsh-id))))))))

(provide 'flashdev-fcsh)
;;; flashdev-fcsh.el ends here

