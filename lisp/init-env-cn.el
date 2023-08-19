;;; -*- lexical-binding: t; no-native-compile: t -*-

(eval-when-compile
  (require 'init-core-macs))

(defalias 'sd 'sdcv)

(defun-add-advice! :override fixup-whitespace
                   init--check-cjk-override-fixup-whitespace ()
  (interactive "*")
  (save-excursion
    (delete-horizontal-space)
    (unless (or (looking-at "^\\|$\\|\\s)")
                (save-excursion
                  (forward-char -1)
                  (looking-at "$\\|\\s(\\|\\s'"))
                (and (looking-at "[[:multibyte:]]")
                     (save-excursion
                       (forward-char -1)
                       (looking-at "[[:multibyte:]]"))))
      (insert ?\s))))

(defun-add-advice! :around org-html-paragraph
                   init--check-cjk-around-org-html-paragraph (func paragraph contents info)
  (let ((fixed-contents
         (replace-regexp-in-string
          "\\([[:multibyte:]]\\) *\n *\\([[:multibyte:]]\\)"
          "\\1\\2"
          contents)))
    (funcall func paragraph fixed-contents info)))

(provide 'init-env-cn)
