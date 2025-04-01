;;; vc-link.el --- Get the repository URL of a buffer  -*- lexical-binding: t -*-

;; Copyright (c) 2023 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/vc-link.el
;; Keywords: vc tools convenience
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))

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
(require 'subr-x)
(require 'url-parse)
(require 'url-util)

(declare-function vc-hg-command "vc-hg")
(declare-function vc-git--run-command-string "vc-git")

(defgroup vc-link nil
  "Get the URL of the current buffer."
  :group 'vc
  :prefix "vc-link-")

(defcustom vc-link-forge-alist
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
  "An alist of forge instances."
  :type '(repeat (list (string :tag "Host domain")
                       (choice
                        (const github)
                        (const gitlab)
                        (const gitea)
                        (const bitbucket)
                        (const savannah)
                        (const pagure)
                        (const sourcehut)
                        (string :tag "URL format")
                        (plist :tag "Rules"
                               :options ((:url string)
                                         (:hg-url string)
                                         (:anchor string)
                                         (:anchor-range string)
                                         (:name (const last)))))
                       (string :tag "Protocol"))))

(defcustom vc-link-remote-names '("upstream" nil)
  "Names of the remotes to look for when building a link.
The value nil refers to the default remote e.g. \"origin\" for Git."
  :type '(repeat (choice (string :tag "Remote name")
                         (const :tag "Default remote" nil)))
  :safe #'listp)

(defconst vc-link-forge-types
  '((github
     :url "%P://%H/%n/blob/%r/%f"
     :anchor "#L%s"
     :anchor-range "#L%s-L%e")
    (gitlab
     :url "%P://%H/%n/-/blob/%r/%f"
     :anchor "#L%s"
     :anchor-range "#L%s-%e")
    (gitea
     :url "%P://%H/%n/src/commit/%r/%f"
     :anchor "#L%s"
     :anchor-range "#L%s-L%e")
    (bitbucket
     :url "%P://%H/%n/src/%r/%f"
     :anchor "#lines-%s"
     :anchor-range "#lines-%s:%e")
    (savannah
     :name last
     :url "%P://%H/cgit/%n/tree/%f?id=%r"
     :anchor "#n%s")
    (pagure
     :url "%P://%H/%n/blob/%r/f/%f"
     :anchor "#_%s"
     :anchor-range "#_%s-%e")
    (sourcehut
     :url "%P://%H/%n/tree/%r/%f"
     :hg-url "%P://%H/%n/browse/%f?rev=%r"
     :anchor "#L%s"))
  "The URL layout of each forge type of `vc-link-forge-alist'.

Template values:
    ?P    protocol
    ?H    host
    ?n    repository name
    ?r    revision
    ?f    filename
    ?s    start line
    ?e    end line")

(defun vc-link--forge-entry (host)
  "Fetch the `vc-link-forge-alist' entry for HOST."
  (when host
    (let ((labels (string-split host "\\.")))
      (seq-some (lambda (i)
                  (assoc-string (string-join (seq-drop labels i) ".")
                                vc-link-forge-alist t))
                (number-sequence 0 (1- (length labels)))))))

(defun vc-link--lines ()
  "Return the range of selected lines as (START . END)."
  (if (use-region-p)
      (let* ((beg (region-beginning))
             (last (line-number-at-pos (max beg (min (region-end) (1- (point-max)))) t))
             (first (line-number-at-pos beg t)))
        (cons first (unless (= first last) last)))
    (cons (line-number-at-pos (point) t) nil)))

(defun vc-link--parse-remote (remote)
  "Parse the repository REMOTE as (HOST . PATH)."
  (let* ((colon (string-search ":" remote))
         (prefix (and colon (substring remote 0 colon)))
         ;; Handle ssh URLs of the form git@github.com:owner/repo.git
         (ssh (and prefix
                   (not (string-search "/" prefix))
                   (not (string-search "://" remote))))
         (url (ignore-errors
                (url-generic-parse-url
                 (if ssh
                     (concat "ssh://" prefix "/" (substring remote (1+ colon)))
                   remote)))))
    (when-let* ((host (and url (url-host url)))
                ((not (seq-empty-p host)))
                (path (string-trim (or (and url (car (url-path-and-query url))) "") "/+" "/+"))
                ((not (seq-empty-p path))))
      (cons (downcase host) path))))

(defun vc-link--forge-link (entry backend host path rev file)
  "Generate the link from ENTRY for BACKEND, HOST, PATH, REV, and FILE."
  (pcase-let* ((`(,domain ,forge ,protocol) entry)
               (rules (pcase forge
                        ((pred symbolp)
                         (or (alist-get forge vc-link-forge-types) (user-error "Unknown forge type %S in `vc-link-forge-alist'" forge)))
                        ((pred stringp) (list :url forge))
                        (_ forge)))
               (name (if (eq (plist-get rules :name) 'last)
                         (file-name-nondirectory path)
                       (string-remove-suffix ".git" path)))
               (url (or (and (eq backend 'Hg) (plist-get rules :hg-url))
                        (plist-get rules :url)
                        (user-error "No `:url' for the forge of %s" domain)))
               (root (vc-call-backend backend 'root file))
               (file-rel (file-relative-name file root))
               (file-path (if (string-equal file-rel "./") "" (url-hexify-string file-rel (cons ?/ url-unreserved-chars))))
               (spec (format-spec-make ?P protocol ?H host ?n name ?r rev ?f file-path)))
    (if (file-directory-p file)
        (format-spec url spec)
      (pcase-let* ((`(,start . ,end) (vc-link--lines))
                   (anchor (or (and end (plist-get rules :anchor-range))
                               (plist-get rules :anchor))))
        (concat (format-spec url spec)
                (and anchor (format-spec anchor (format-spec-make ?s start ?e end))))))))

(defun vc-link--remote (backend file-or-dir)
  "Return the remote of BACKEND of FILE-OR-DIR as (REMOTE . URL)."
  (seq-some (lambda (remote)
              (when-let* ((url (ignore-errors
                                 (vc-call-backend backend 'repository-url
                                                  file-or-dir remote)))
                          ((not (seq-empty-p url))))
                (cons remote url)))
            vc-link-remote-names))

(defun vc-link--remote-has-revision-p (backend remote rev)
  "Check whether REMOTE of BACKEND is known to have REV."
  (pcase backend
    ('Git
     (seq-empty-p
      (ignore-errors
        (require 'vc-git)
        (vc-git--run-command-string nil "rev-list" "--max-count=1" rev "--not" (concat "--remotes=" (or remote "origin"))))))
    (_ t)))

;;;###autoload
(defun vc-link (&optional arg)
  "Fetch the current vc URL.
With prefix ARG, open the URL with `browse-url'."
  (interactive "P")
  (pcase-let*
      ((file (or buffer-file-name default-directory))
       (backend (or (vc-responsible-backend file 'no-error)
                    (user-error "No VC backend is responsible for %s" file)))
       (`(,remote . ,url) (or (vc-link--remote backend file)
                              (user-error "No repository URL found for %s" file)))
       (`(,host . ,path) (or (vc-link--parse-remote (string-trim url))
                             (user-error "The remote of %s is not a URL: %s" file url)))
       (entry (or (vc-link--forge-entry host)
                  (user-error "No forge in `vc-link-forge-alist' matches the remote URL %s" url)))
       (rev (or (if (eq backend 'Hg)
                    ;; `vc-hg' until Emacs 32 uses local revisions, instead of changeset hashes.
                    ;; See https://github.com/emacs-mirror/emacs/commit/7ce60be53e7ffbc1972dd8e98744adf6103eee9e
                    (progn
                      (require 'vc-hg)
                      (with-output-to-string
                        (vc-hg-command standard-output 0 nil "log" "-r" "." "--template" "{node}")))
                  (vc-call-backend backend 'working-revision file))
                (user-error "The repository of %s has no commits yet" file)))
       (link (vc-link--forge-link entry backend host path rev file)))
    (pcase (vc-state file backend)
      ('added
       (lwarn 'vc-link :warning "File is not committed yet, the link may not resolve: %s" file))
      ('unregistered
       (lwarn 'vc-link :warning "File is not registered in VC %s, the link may not resolve: %s" backend file)))
    (unless (vc-link--remote-has-revision-p backend remote rev)
      (lwarn 'vc-link :warning "Revision %s is not among the fetched refs of the %s remote, the link may not resolve: %s" rev (or remote "default") file))
    (if arg
        (browse-url link)
      (kill-new (message "%s" link)))
    link))

(provide 'vc-link)
;;; vc-link.el ends here
