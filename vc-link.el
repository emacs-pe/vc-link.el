;;; vc-link.el --- Get the repository URL of a buffer  -*- lexical-binding: t -*-

;; Copyright (c) 2023 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/vc-link.el
;; Keywords: vc tools convenience
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;;; License:

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

;; Get the repository URL of a buffer using `vc'.
;;
;; Supported Services:
;; - <https://github.com/>
;; - <https://gitea.com/>
;; - <https://codeberg.org/>
;; - <https://git.lix.systems/>
;; - <https://gitlab.com/>
;; - <https://framagit.org/>
;; - <https://salsa.debian.org/>
;; - <https://foss.heptapod.net/>
;; - <https://gitlab.torproject.org/>
;; - <https://gitlab.freedesktop.org/>
;; - <https://git.savannah.gnu.org/>
;; - <https://bitbucket.org/>
;; - <https://pagure.io/>
;; - <https://src.fedoraproject.org/>
;; - <https://sr.ht/>

;; -------------------------------------------------------------------
;; Israel is committing genocide of the Palestinian people.
;;
;; The population in Gaza is facing starvation, displacement and
;; annihilation amid relentless bombardment and suffocating
;; restrictions on life-saving humanitarian aid.
;;
;; As of March 2025, Israel has killed over 50,000 Palestinians in the
;; Gaza Strip – including 15,600 children – targeting homes,
;; hospitals, schools, and refugee camps.  However, the true death
;; toll in Gaza may be at least around 41% higher than official
;; records suggest.
;;
;; The website <https://databasesforpalestine.org/> records extensive
;; digital evidence of Israel's genocidal acts against Palestinians.
;; Save it to your bookmarks and let more people know about it.
;;
;; Silence is complicity.
;; Protest and boycott the genocidal apartheid state of Israel.
;;
;;
;;                  From the river to the sea, Palestine will be free.
;; -------------------------------------------------------------------

;;; Code:
(require 'format-spec)

(defvar vc-link-forge-alist
  '(("github.com"             github    "https")
    ("gitea.com"              gitea     "https")
    ("codeberg.org"           gitea     "https")
    ("git.lix.systems"        gitea     "https")
    ("gitlab.com"             gitlab    "https")
    ("framagit.org"           gitlab    "https")
    ("salsa.debian.org"       gitlab    "https")
    ("foss.heptapod.net"      gitlab    "https")
    ("gitlab.torproject.org"  gitlab    "https")
    ("gitlab.freedesktop.org" gitlab    "https")
    ("git.savannah.gnu.org"   savannah  "https")
    ("bitbucket.org"          bitbucket "https")
    ("pagure.io"              pagure    "https")
    ("src.fedoraproject.org"  pagure    "https")
    ("sr.ht"                  sourcehut "https"))
  "An alist of forge instances.")

(defvar vc-link--vc-alist nil)

(defun vc-link--vc-alist (&optional rebuild)
  "Generate the `vc-link--vc-alist' alist.
If REBUILD is non-nil, generate it again."
  (if (and vc-link--vc-alist (null rebuild))
      vc-link--vc-alist
    (setq vc-link--vc-alist
          (mapcar (lambda (entry)
                    (apply #'vc-link--forge-setup entry))
                  vc-link-forge-alist))))

(defun vc-link--region ()
  "Capture lines of the selected region otherwise current line."
  (save-restriction
    (widen)
    (if (use-region-p)
        (let ((start (line-number-at-pos (region-beginning)))
              (end (line-number-at-pos (region-end))))
          (if (= start end)
              (list start)
            (cons start end)))
      (list (line-number-at-pos)))))

(defun vc-link--match-groups (string)
  "Return the captured regexp groups from `match-data' in STRING."
  (let (groups)
    (dotimes (i (/ (length (match-data)) 2))
      (push (match-string i string) groups))
    (nreverse groups)))

(declare-function vc-hg-command "vc-hg")

(defun vc-link-spec (backend &rest extra)
  "Return the format spec for vc BACKEND and EXTRA pairs."
  (unless (zerop (% (length extra) 2))
    (error "Obscure usage of this function appeared"))
  (let* ((root (vc-call-backend backend 'root default-directory))
         (rev (if (eq backend 'Hg)
                  (with-output-to-string ; `vc-hg' uses revisions, instead of changeset ids.
                    (vc-hg-command standard-output 0 nil "log" "-r" "." "--template" "{node}"))
                (vc-call-backend backend 'working-revision root)))
         (pairs (if buffer-file-name
                    (if (vc-call-backend backend 'registered buffer-file-name)
                        (let ((lines (vc-link--region)))
                          (list ?r rev ?s (car-safe lines) ?e (cdr-safe lines)
                                ?f (file-relative-name buffer-file-name root)))
                      (user-error "File is not registered in VC %s: %s" backend buffer-file-name))
                  (list ?r rev ?f (file-relative-name default-directory root)))))
    (apply #'format-spec-make (append pairs extra))))

(cl-defgeneric vc-link--forge-setup (host-domain forge-type protocol)
  "Build an entry for `vc-link--vc-alist'.")

(cl-defmethod vc-link--forge-setup (host-domain (_forge-type (eql 'github)) protocol)
  (cons
   (concat "[/@]" (regexp-quote host-domain) "[/:]\\([.A-Za-z0-9_/-]+?\\)\\(?:\\.git\\)?/?\\'")
   (lambda (backend groups)
     (let ((spec (vc-link-spec backend ?P protocol ?H host-domain ?n (nth 1 groups))))
       (format-spec (concat "%P://%H/%n/blob/%r/%f" (and (cdr (assq ?s spec)) "#L%s") (and (cdr (assq ?e spec)) "-L%e")) spec)))))

(cl-defmethod vc-link--forge-setup (host-domain (_forge-type (eql 'gitlab)) protocol)
  (cons
   (concat "[/@]" (regexp-quote host-domain) "[/:]\\([.A-Za-z0-9_/-]+?\\)\\(?:\\.git\\)?/?\\'")
   (lambda (backend groups)
     (let ((spec (vc-link-spec backend ?P protocol ?H host-domain ?n (nth 1 groups))))
       (format-spec (concat "%P://%H/%n/-/blob/%r/%f" (and (cdr (assq ?s spec)) "#L%s") (and (cdr (assq ?e spec)) "-L%e")) spec)))))

(cl-defmethod vc-link--forge-setup (host-domain (_forge-type (eql 'gitea)) protocol)
  (cons
   (concat "[/@]" (regexp-quote host-domain) "[/:]\\([.A-Za-z0-9_/-]+?\\)\\(?:\\.git\\)?/?\\'")
   (lambda (backend groups)
     (let ((spec (vc-link-spec backend ?P protocol ?H host-domain ?n (nth 1 groups))))
       (format-spec (concat "%P://%H/%n/src/commit/%r/%f" (and (cdr (assq ?s spec)) "#L%s") (and (cdr (assq ?e spec)) "-L%e")) spec)))))

(cl-defmethod vc-link--forge-setup (host-domain (_forge-type (eql 'bitbucket)) protocol)
  (cons
   (concat "[/@]" (regexp-quote host-domain) "[/:]\\([.A-Za-z0-9_/-]+?\\)\\(?:\\.git\\)?/?\\'")
   (lambda (backend groups)
     (let ((spec (vc-link-spec backend ?P protocol ?H host-domain ?n (nth 1 groups))))
       (format-spec (concat "%P://%H/%n/src/%r/%f" (and (cdr (assq ?s spec)) "#lines-%s") (and (cdr (assq ?e spec)) ":%e")) spec)))))

(cl-defmethod vc-link--forge-setup (host-domain (_forge-type (eql 'savannah)) protocol)
  (cons
   (concat "[/@]" (regexp-quote host-domain) "[/:]\\(?:\\(?:srv/\\)?git/\\)?\\([.A-Za-z0-9_/-]+?\\.git\\)\\'")
   (lambda (backend groups)
     (let ((spec (vc-link-spec backend ?P protocol ?H host-domain ?n (nth 1 groups))))
       ;; N.B. cgit, used by GNU Savannah, doesn't support multi-line links
       (format-spec (concat "%P://%H/cgit/%n/tree/%f?id=%r" (and (cdr (assq ?s spec)) "#n%s")) spec)))))

(cl-defmethod vc-link--forge-setup (host-domain (_forge-type (eql 'pagure)) protocol)
  (cons
   (concat "[/@]" (regexp-quote host-domain) "[/:]\\([.A-Za-z0-9_/-]+?\\)\\(?:\\.git\\)?/?\\'")
   (lambda (backend groups)
     (let ((spec (vc-link-spec backend ?P protocol ?H host-domain ?n (nth 1 groups))))
       (format-spec (concat "%P://%H/%n/blob/%r/f/%f" (and (cdr (assq ?s spec)) "#_%s") (and (cdr (assq ?e spec)) "-%e")) spec)))))

(cl-defmethod vc-link--forge-setup (host-domain (_forge-type (eql 'sourcehut)) protocol)
  (cons
   (concat "[/@]\\(\\(?:git\\|hg\\)." (regexp-quote host-domain) "\\)" "[/:]\\(~[.A-Za-z0-9_/-]+\\)")
   (lambda (backend groups)
     (let ((spec (vc-link-spec backend ?P protocol ?H (nth 1 groups) ?n (nth 2 groups))))
       ;; N.B. Sourcehut doesn't support multi-line links
       (format-spec (concat (if (eq backend 'Hg) "%P://%H/%n/browse/%f?rev=%r" "%P://%H/%n/tree/%r/%f") (and (cdr (assq ?s spec)) "#L%s")) spec)))))

;;;###autoload
(defun vc-link ()
  "Fetch the current vc url."
  (interactive)
  (if-let*
      ((file-or-dir (or buffer-file-name default-directory))
       (backend (vc-responsible-backend file-or-dir 'no-error))
       (repo (seq-some (lambda (remote)
                         (ignore-errors
                           (vc-call-backend backend 'repository-url
                                            file-or-dir remote)))
                       '("upstream" nil)))
       (link (seq-some (lambda (config)
                         (when (string-match (car-safe config) repo)
                           (funcall (cdr-safe config) backend (vc-link--match-groups repo))))
                       (vc-link--vc-alist))))
      (kill-new (message "%s" link))
    (user-error "Could not build a remote link")))

(provide 'vc-link)
;;; vc-link.el ends here
