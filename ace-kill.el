(require 'thingatpt)
(require 'hydra)

;; For convenience, ace-kill-hydra/body can be bound, e.g.,
;; (global-set-key (kbd "s-u") 'ace-kill-hydra/body)

(defface ak-candidate-face
  '((t (:foreground "#008b8b")))
  "Face for target candidates."
  :group 'ace-kill)

(defface ak-index-face
  '((t (:foreground "#90ee90")))
  "Face for candidate index."
  :group 'ace-kill)

(defvar ak-uuid-regexp thing-at-point-uuid-regexp
  "Regexp identifying UUIDv4.")

(defvar ak-ipaddr-regexp
  (rx (seq
       word-start
       (or (group "25" (any "0-5"))
           (group "2" (any "0-4") (any "0-9"))
           (group (opt (any "0-1")) (any "0-9") (opt (any "0-9"))))
       (= 3
          "."
          (or (group "25" (any "0-5"))
              (group "2" (any "0-4") (any "0-9"))
              (group (opt (any "0-1")) (any "0-9") (opt (any "0-9")))))
       word-end))
  "Regexp identifying IP (v4) address.")

(defvar ak-http-regexp
  (rx (seq word-start
           "http" (opt "s") "://" (+ (not space))
           word-end))
  "Regexp identifying http and https URLs.")

(defvar ak-email-regexp thing-at-point-email-regexp
  "Regexp identifying email addresses.")

(defvar ak-gcloud-access-token-regexp
  (rx (seq word-start "ya29." (+ (any "-a-zA-Z0-9_"))))
  "Regexp identifying Google Cloud access token.")

(defvar ak-google-oauth-token-regexp
  (rx (seq word-start
       (or (seq (= 3 (any "A-Z")) "-" (= 3 (any "A-Z")) "-" (= 3 (any "A-Z")))
           (seq (= 4 (any "A-Z")) "-" (= 4 (any "A-Z")))) word-end))
  "Regexp identifying Google device token.")

;; rccg-e28d-d1a39
(defvar ak-ip4g-rccg-name
  (rx (seq word-start "rccg-"
           (= 4 (any hex)) "-" (= 5 (any hex)))))

;; auxVolumeName: aux_volume-km-acc-3-3110221c-d2f784886785
(defvar ak-ip4g-aux-name
  (rx (seq word-start "aux_volume-"
           (+? (any ascii))
           "-" (= 8 (any hex))
           "-" (= 12 (any hex)))))

(defun ak-ip4g-aux-name-find ()
  (interactive)
  (ak--find-candidates ak-ip4g-aux-name))

(defun ak-ip-find ()
  "ace-kill search for IP address in visible window."
  (interactive)
  (ak--find-candidates ak-ipaddr-regexp))

(defun ak-uuid-find ()
  "ace-kill search for UUIDv4 in visible window."
  (interactive)
  (ak--find-candidates ak-uuid-regexp))

(defun ak-http-find ()
  "ace-kill search for http(s) URL in visible window."
  (interactive)
  (ak--find-candidates ak-http-regexp))

(defun ak-email-find ()
  "ace-kill search for email address in visible window."
  (interactive)
  (ak--find-candidates ak-email-regexp))

(defun ak-gcloud-access-token-find ()
  (interactive)
  (ak--find-candidates ak-gcloud-access-token-regexp))

(defun ak-google-oauth-token-find ()
  (interactive)
  (ak--find-candidates ak-google-oauth-token-regexp))

(defun ak-ip4g-rccg-name-find ()
  (interactive)
  (ak--find-candidates ak-ip4g-rccg-name))

(defun ak--find-candidates (regexp)
  "Search current visible window for REGEXP
 assigning an index to each match.

If the major mode is vterm-mode, put the buffer into
vterm-copy-mode then revert after an index is selected."
  (let ((matches (make-hash-table))
        (marker-list (cl-loop for i from ?a to ?z collect i))
        (vterm-copy-mode-toggled-p
         (if (eq major-mode #'vterm-mode)
             (if (not vterm-copy-mode)
                 (progn (vterm-copy-mode 1) t)))))
    (cl-flet ((pop-marker ()
                (let ((the-marker (car marker-list)))
                  (setq marker-list (cdr marker-list))
                  the-marker)))
      (save-excursion
        (move-to-window-line 0)
        (while (re-search-forward regexp nil t)
          (let* ((start (car (match-data t)))
                 (end (cadr (match-data t)))
                 (marker-idx (pop-marker))
                 (marker-ol (make-overlay start end))
                 (marker-str
                  (propertize (format "%c " marker-idx)
                              'face 'ak-index-face))
                 (font-ol (make-overlay start end)))
            (overlay-put font-ol 'face 'ak-candidate-face)
            (overlay-put marker-ol 'before-string marker-str)
            (puthash marker-idx
                     (list (match-string 0) font-ol marker-ol)
                     matches)))))
    (let* ((index (read-key "Index: "))
           (match (gethash index matches nil)))
      (if match
          (progn (kill-new (car match))
                 (message "Added %s to kill." (car match)))
        (message "Invalid index (%s)." index)))
    (maphash (lambda (k v)
               (delete-overlay (nth 1 v))
               (delete-overlay (nth 2 v))) matches)
    (if vterm-copy-mode-toggled-p (vterm-copy-mode 0))))


(defhydra ace-kill-hydra (:color pink
                                 :exit t
                                 :hint nil)
  "
_u_: UUIDv4               _i_: IP Address
_h_: HTTP(s) URL          _e_: Email Address
_g_: GCloud Access Token  _p_: Google OAuth Token
_r_: IP4G RCCG Name       _a_: IP4G Aux Vol Name

"
  ("r" ak-ip4g-rccg-name-find)
  ("a" ak-ip4g-aux-name-find)
  ("u" ak-uuid-find)
  ("i" ak-ip-find)
  ("h" ak-http-find)
  ("e" ak-email-find)
  ("g" ak-gcloud-access-token-find)
  ("p" ak-google-oauth-token-find)
  ("q" nil "quit" :color blue))

(provide 'ace-kill)
