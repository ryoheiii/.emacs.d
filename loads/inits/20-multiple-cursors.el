;;; multiple-cursors
;;; 複数箇所を同時に編集する
;;; 2012-12-05
(use-package multiple-cursors
  :ensure t)
(use-package smartrep
  :ensure t)

;;; Code:
(declare-function smartrep-define-key "smartrep")

(global-set-key (kbd "C-M-c") 'mc/edit-lines)
(global-set-key (kbd "C-*")   'mc/mark-all-like-this)

(global-unset-key "\C-q")

(smartrep-define-key global-map "C-q"
  '(("p"      . 'mc/mark-previous-like-this)
    ("n"      . 'mc/mark-next-like-this)
    ("*"        . 'mc/mark-all-like-this)
    ("d"        . 'mc/mark-all-like-this-dwim)
    ("m"        . 'mc/mark-more-like-this-extended)
    ("u"        . 'mc/unmark-next-like-this)
    ("U"        . 'mc/unmark-previous-like-this)
    ("s"        . 'mc/skip-to-next-like-this)
    ("S"        . 'mc/skip-to-previous-like-this)
    ("i"        . 'mc/insert-numbers)
    ("o"        . 'mc/sort-regions)
    ("O"        . 'mc/reverse-regions)))

(provide '20-multiple-cursors)
;;; 20-multiple-cursors.el ends here
