;;; YaTeX
;; 140213
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq YaTeX-inhibit-prefix-letter t)
;;;   nil=YaTeX-kanji-code が nil なら coding-system に感知しない
;;;   0=no-converion -> Emacs 内部で使用されている文字コードで保存される (Emacs 23 以降では utf-8-emacs)
;;;   1=Shift JIS (Shift_JIS)
;;;   2=JIS (ISO-2022-JP)
;;;   3=EUC (EUC-JP)
;;;   4=UTF-8
(setq YaTeX-kanji-code nil)
(setq YaTeX-latex-message-code 'utf-8)
(setq tex-command "ptex2pdf -u -l -ot \"-kanji=utf8 -no-guess-input-enc -synctex=1\"")
(setq bibtex-command "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/mendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
(setq makeindex-command  "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/mendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
(setq dvi2-command "evince")
(setq dviprint-command-format "acroread `echo %s | sed -e \"s/\\.[^.]*$/\\.pdf/\"`")
(require 'dbus)

;; color
(add-hook 'yatex-mode-hook
'(lambda () (require 'font-latex)
            (font-latex-setup)
            (progn
              (modify-syntax-entry ?% "<" (syntax-table))
              (modify-syntax-entry 10 ">" (syntax-table))
              (make-variable-buffer-local 'outline-level)
              (setq outline-level 'latex-outline-level)
              (make-variable-buffer-local 'outline-regexp)
              (setq outline-regexp
                    (concat "[  \t]*\\\\\\(documentstyle\\|documentclass\\|chapter\\|"
                           "section\\|subsection\\|subsubsection\\|paragraph\\)"
                            "\\*?[ \t]*[[{]")
                    ))))

(defun un-urlify (fname-or-url)
  "A trivial function that replaces a prefix of file:/// with just /."
  (if (string= (substring fname-or-url 0 8) "file:///")
      (substring fname-or-url 7)
    fname-or-url))

(defun evince-inverse-search (file linecol &rest ignored)
  (let* ((fname (un-urlify file))
         (buf (find-file fname))
         (line (car linecol))
         (col (cadr linecol)))
    (if (null buf)
        (message "[Synctex]: %s is not opened..." fname)
      (switch-to-buffer buf)
      (goto-line (car linecol))
      (unless (= col -1)
        (move-to-column col)))))

(dbus-register-signal
 :session nil "/org/gnome/evince/Window/0"
 "org.gnome.evince.Window" "SyncSource"
 'evince-inverse-search)

(defun okular-forward-search ()
  (interactive)
  (progn
    (process-kill-without-query
     (start-process
      "okular"
      nil
      "okular"
      "--unique"
      (concat (expand-file-name
               (concat (file-name-sans-extension (or YaTeX-parent-file
                                                     (save-excursion
                                                       (YaTeX-visit-main t)
                                                       buffer-file-name)))
                       ".pdf"))
              "#src:"
              (number-to-string (save-restriction
                                  (widen)
                                  (count-lines (point-min) (point))))
              (buffer-file-name))))))

(add-hook 'yatex-mode-hook
          '(lambda ()
             (define-key YaTeX-mode-map (kbd "C-c o") 'okular-forward-search)))

(defun zathura-forward-search ()
  (interactive)
  (progn
    (process-kill-without-query
     (start-process
      "zathura"
      nil
      "zathura"
      "--synctex-forward"
      (concat (number-to-string (save-restriction
                                  (widen)
                                  (count-lines (point-min) (point))))
              ":0:"
              (buffer-name))
      (expand-file-name
       (concat (file-name-sans-extension (or YaTeX-parent-file
                                             (save-excursion
                                               (YaTeX-visit-main t)
                                               buffer-file-name)))
               ".pdf"))))))

(add-hook 'yatex-mode-hook
          '(lambda ()
             (define-key YaTeX-mode-map (kbd "C-c z") 'zathura-forward-search)))

(add-hook 'yatex-mode-hook
          '(lambda ()
             (auto-fill-mode -1)))

;;
;; RefTeX with YaTeX
;;
;(add-hook 'yatex-mode-hook 'turn-on-reftex)
(add-hook 'yatex-mode-hook
          '(lambda ()
             (reftex-mode 1)
             (define-key reftex-mode-map (concat YaTeX-prefix ">") 'YaTeX-comment-region)
             (define-key reftex-mode-map (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))
