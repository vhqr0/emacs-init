;;; -*- lexical-binding: t; no-native-compile: t -*-

(require 'init-core-lib)

;; (defun init-check-cjk-override-fixup-whitespace ()
;;   (interactive "*")
;;   (save-excursion
;;     (delete-horizontal-space)
;;     (unless (or (looking-at "^\\|$\\|\\s)")
;;                 (save-excursion
;;                   (forward-char -1)
;;                   (looking-at "$\\|\\s(\\|\\s'"))
;;                 (and (looking-at "[[:multibyte:]]")
;;                      (save-excursion
;;                        (forward-char -1)
;;                        (looking-at "[[:multibyte:]]"))))
;;       (insert ?\s))))

;; (init-add-advice :override 'fixup-whitespace #'init-check-cjk-override-fixup-whitespace)

;; (defun init-check-cjk-around-org-html-paragraph (func paragraph contents info)
;;   (let ((fixed-contents
;;          (replace-regexp-in-string
;;           "\\([[:multibyte:]]\\) *\n *\\([[:multibyte:]]\\)"
;;           "\\1\\2"
;;           contents)))
;;     (funcall func paragraph fixed-contents info)))

;; (init-add-advice :around 'org-html-paragraph #'init-check-cjk-around-org-html-paragraph)

(provide 'init-cn-basic)
