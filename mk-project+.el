;;; mk-project+.el --- Enhancement of `mk-project.el'

;; Copyright (C) 2010-2013  Seungcheol Jung

;; Author: Seungcheol Jung <scjung.hyu at gmail dot com>
;; Version: 0.2.4

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO

;;; Code:

(require 'mk-project)

(defconst mk-proj+-version "0.2.4")

(defgroup mk-project+ nil
  "Enhancement of `mk-project'"
  :tag "Mk-project+"
  :prefix "mk-proj+-"
  :group 'mk-project)

(defcustom mk-proj+-conf-dir "~/.emacs.d/mk-project/"
  "Project configuration directory. Every configuration files is
stored under this directory."
  :type 'directory
  :group 'mk-project+)

(defcustom mk-proj+-revive-filename ".revive.el"
  "Name of `revive' configuration files."
  :type 'string
  :group 'mk-project+)

(defcustom mk-proj+-close-nonfile-buffers t
  "If non-nil, the function, which closes a project, also closes related
non-file buffers."
  :type 'boolean
  :group 'mk-project+)

(defcustom mk-proj+-close-nonproject-buffers t
  "If non-nil, the function, which closes a project, also closes
non-project buffers."
  :type 'boolean
  :group 'mk-project+)

(defcustom mk-proj+-default-ignore-patterns
  '("*.cm[ioax]" "*.cmxa" "*.o" "*.a" "*.elc"
    ".svn" "_darcs" ".git" ".bzr" ".hg" ".CVS")
  "Default ignore file patterns"
  :type '(repeat (string :tag "Glob pattern"))
  :group 'mk-project+)

(defcustom mk-proj+-use-helm t
  "If non-nil, use `helm' interface."
  :type 'boolean
  :group 'mk-project+)

(defsubst mk-proj+-dir (name)
  (expand-file-name name mk-proj+-conf-dir))

(defconst mk-proj+-vcs-file '((".svn"   . svn)
                              (".git"   . git)
                              (".bzr"   . bzr)
                              (".hg"    . hg)
                              (".CVS"   . cvs)
                              ("_darcs" . darcs))
  "Alist. It is used for determine the corresponding version control system
to a directory.")

(defun mk-proj+-vcs-of-dir (dir)
  (catch 'found
    (dolist (vcs mk-proj+-vcs-file nil)
      (when (file-exists-p (expand-file-name (car vcs) dir))
        (throw 'found (cdr vcs))))
    nil))

(defconst mk-proj+-cmd '(("Makefile" . ("Makefile" . "make"))
                         ("build.xml" . ("Ant" . "ant"))))

(defun mk-proj+-cmd-of-dir (dir)
  (catch 'found
    (dolist (vcs mk-proj+-cmd nil)
      (when (file-exists-p (expand-file-name (car vcs) dir))
        (throw 'found (cdr vcs))))
    (throw 'notfound nil)))

(defvar mk-proj+-cmd-table (make-hash-table :test 'equal))

(defun mk-proj+-get-cmds (name)
  (or (gethash name mk-proj+-cmd-table)
      (error (format "No commands for the project '%s'." name))))

(defun mk-proj+-register-cmds (name basedir &optional cmd-alist)
  (puthash name
           (or cmd-alist
               (catch 'notfound
                 (list (mk-proj+-cmd-of-dir basedir))))
           mk-proj+-cmd-table))

(defun mk-proj+-default-cmd (name)
  (cdr (car (mk-proj+-get-cmds name))))

(defun mk-proj+-prepare-dir (name)
  "Prepare a directory to store project values"
  (let ((dir (mk-proj+-dir name)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    dir))

(defun mk-proj+-dir-list ()
  (mk-proj-assert-proj)
  (with-temp-buffer
    (let ((default-directory (file-name-as-directory mk-proj-basedir))
          (vcs-path (mk-proj-get-vcs-path))
          (args `("find" nil ,(current-buffer) nil "." "-type" "d"
                  ,(mk-proj-find-cmd-ignore-args mk-proj-ignore-patterns))))
      (when vcs-path (setq args (append args `("-not" "-path" ,vcs-path))))
      (apply 'call-process-shell-command args)
      (split-string (buffer-string) "\n" t))))

(defun mk-proj+-file-list ()
  (mk-proj-assert-proj)
  (let ((file-index-buf (get-buffer mk-proj-fib-name)))
    (if file-index-buf
      (with-current-buffer file-index-buf
        (split-string (buffer-string) "\n" t))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enhancement of project file closing

(defun mk-proj+-buffer-p (buf)
  "Determine whether the given buffer is related to the current project or not.
If `mk-proj+-close-nonfile-buffers' is non-nil, unlike `mk-proj-buffer-p', this
function also returns non-nil for the non-file buffers that `default-directory'
is the base directory of the project."
  (let ((file-name (mk-proj-buffer-name buf))
        (basedir (file-name-as-directory (expand-file-name mk-proj-basedir))))
    (if file-name
        (or mk-proj+-close-nonproject-buffers
            (string-match (concat "^" (regexp-quote mk-proj-basedir))
                          file-name))
      (and mk-proj+-close-nonfile-buffers
           (not (minibufferp buf))
           (or mk-proj+-close-nonproject-buffers
               (with-current-buffer buf
                 (string= (file-name-as-directory
                           (expand-file-name default-directory))
                       basedir)))))))

(defun mk-proj+-buffers ()
  "Get a list of buffers that reside in the current project's basedir.
Unlike `mk-proj-buffers', the list also contains the non-file
buffers that `default-directory' is the base directory of the project."
  (let ((buffers nil))
    (dolist (b (buffer-list))
      (when (mk-proj+-buffer-p b) (push b buffers)))
    buffers))

(defun mk-proj+-closable-buffer-p (buf)
  (let ((name (buffer-name buf)))
    (and (not (string= name "*Messages*"))
         (not (string= name "*scratch*"))
         (or (not (buffer-file-name buf))
             (not (buffer-modified-p buf))))))

(defun mk-proj+-close-buffers ()
  "Close all unmodified buffers that reside in the project's basedir.
Unlike `project-close-files', the list also closes the non-file
buffers that `default-directory' is the base directory of the project."
  (interactive)
  (mk-proj-assert-proj)
  (dolist (b (mk-proj+-buffers))
    (when (mk-proj+-closable-buffer-p b)
      (kill-buffer b))))

(add-hook 'mk-proj-shutdown-hook 'mk-proj+-close-buffers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Integration with `revive.el'

;; dummy definitions to suppress compiler warnings
(defvar revive:configuration-file)
(declare-function resume "revive")
(defvar revive:configuration-file)
(declare-function save-current-configuration "revive")

(defsubst mk-proj+-revive-file (name)
  (expand-file-name mk-proj+-revive-filename
                    (mk-proj+-dir name)))

(defun mk-proj+-revive-availablep ()
  (and (functionp 'resume)
       (functionp 'save-current-configuration)))

(defun mk-proj+-assert-revive ()
  (unless (mk-proj+-revive-availablep)
    (error "No `revive' provided.")))

(defun mk-proj+-revive-load (name)
  "Restore previous `revive' session of the project, NAME."
  (when (mk-proj+-revive-availablep)
    (let ((revive-file (mk-proj+-revive-file name)))
      (when (file-exists-p revive-file)
        (setq revive:configuration-file revive-file)
        (resume)
        (message "Project environment file `%s' loaded." revive-file)))))

;; Restore `revive' session after loading a project automatically
(defadvice project-load (after revive-load-after-project-load)
  (mk-proj+-revive-load mk-proj-name))
(ad-activate 'project-load)

(defun mk-proj+-revive-save (name)
  "Save current `revive' session of the project, NAME."
  (when (mk-proj+-revive-availablep)
    (let ((revive-file (mk-proj+-revive-file name)))
      (setq revive:configuration-file revive-file)
      (save-current-configuration)
      (message "Project environment file `%s' saved." revive-file))))

;; Save `revive' session before closing a project automatically
(defadvice project-unload (before revive-save-before-project-unload)
  (when mk-proj-name (mk-proj+-revive-save mk-proj-name)))
(ad-activate 'project-unload)

;; Save `revive' session before quitting Emacs automatically
(defadvice mk-proj-kill-emacs-hook (before revive-save-before-kill-emacs)
  (when mk-proj-name (mk-proj+-revive-save mk-proj-name)))
(ad-activate 'mk-proj-kill-emacs-hook)

(defadvice project-load (before use-helm-project-load)
  (when mk-proj+-use-helm
    (unless (ad-get-arg 0)
      (ad-set-arg 0 (helm-comp-read "Project Name: "
                                    (mk-proj-names) :must-match t))
      )))
(ad-activate 'project-load)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End-user interface

(defun project-visit-file ()
  (interactive)
  (mk-proj-assert-proj)
  (let ((file (completing-read "File to visit: "
                               (append (mk-proj+-dir-list)
                                       (mk-proj+-file-list))
                               nil t)))
    (find-file (expand-file-name file mk-proj-basedir))))

(defun project-select-cmd ()
  (interactive)
  (mk-proj-assert-proj)
  (let* ((cmds (mk-proj+-get-cmds mk-proj-name))
         (cmd-names (mapcar 'car cmds)))
    (if (cdr cmd-names)
        (let ((cmd-name (completing-read "Command to execute: "
                                         cmd-names nil t)))
          (setq mk-proj-compile-cmd (cdr (assoc cmd-name cmds))))
      ; if there is only one command, use that one.
      (setq mk-proj-compile-cmd (cdr (car cmds))))))

(defun project-execute-cmd ()
  "Run the (compile) command for this project."
  (interactive)
  (mk-proj-assert-proj)
  (let ((default-directory mk-proj-basedir)
        (mk-proj-compile-cmd))
    (project-select-cmd)
    (cond ((stringp mk-proj-compile-cmd)
           (message (format "Executing the project command: %s"
                            mk-proj-compile-cmd))
           (let ((compile-command mk-proj-compile-cmd)
                 (compilation-read-command nil))
             (call-interactively 'compile)))
          ((functionp mk-proj-compile-cmd)
           (cond ((commandp mk-proj-compile-cmd)
                  (call-interactively mk-proj-compile-cmd))
                 (t (funcall mk-proj-compile-cmd))))
          (t (error "No compile command?")))))

(defun project-revive-load ()
  "TODO"
  (mk-proj-assert-proj)
  (mk-proj+-revive-load mk-proj-name))

(defun project-revive-save ()
  "TODO"
  (mk-proj-assert-proj)
  (mk-proj+-revive-save mk-proj-name))

(defun project-easy-def (name basedir &optional cmd-alist ignore-patterns)
  "Define a project easily. It is a high-level wrapper of `project-def' of
`mk-project.el'. If CMD-ALIST is not given, no commands are registered."
  (mk-proj+-prepare-dir name)
  (let ((prj-dir (mk-proj+-dir name)))
    (mk-proj+-register-cmds name basedir cmd-alist)
    (project-def name
                 `((startup-hook     nil)
                   (shutdown-hook    nil)
                   (ignore-patterns  ,(append mk-proj+-default-ignore-patterns
                                              ignore-patterns))
                   (basedir          ,(file-name-as-directory basedir))
                   (file-list-cache  ,(expand-file-name "files" prj-dir))
                   (open-files-cache ,(expand-file-name "open-files" prj-dir))
                   (compile-cmd      ,(condition-case nil
                                          (mk-proj+-default-cmd name)
                                        (error nil)))
                   (vcs              ,(mk-proj+-vcs-of-dir basedir))))))

(global-set-key (kbd "C-c p c") 'project-compile)
(global-set-key (kbd "C-c p l") 'project-load)
(global-set-key (kbd "C-c p a") 'project-ack)
(global-set-key (kbd "C-c p g") 'project-grep)
(global-set-key (kbd "C-c p o") 'project-multi-occur)
(global-set-key (kbd "C-c p u") 'project-unload)
(global-set-key (kbd "C-c p f") 'project-find-file) ; or project-find-file-ido
(global-set-key (kbd "C-c p i") 'project-index)
(global-set-key (kbd "C-c p s") 'project-status)
(global-set-key (kbd "C-c p d") 'project-dired)
(global-set-key (kbd "C-c p t") 'project-tags)
(global-set-key (kbd "C-c p <return>") 'project-execute-cmd)
(global-set-key (kbd "C-x C-o") 'project-visit-file)

(provide 'mk-project+)
;;; mk-project+.el ends here
