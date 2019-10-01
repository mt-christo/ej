(cua-mode t)
(setq-default indent-tabs-mode nil)


(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


(elpy-enable)
;;(set-default 'truncate-lines t)
(define-key input-decode-map "\e[4~" 'end-of-line)
(global-set-key (kbd "<f5>")  'python-shell-send-region)
(global-set-key (kbd "C-q")  'ess-eval-region-or-line-and-step)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-enabled-themes (quote (wombat)))
 '(elpy-rpc-backend "rope")
 '(elpy-rpc-python-command "python3")
 '(package-selected-packages (quote (editorconfig elpy)))
 '(python-shell-interpreter "python3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(add-to-list 'load-path "/wrk/ess")
(load "ess-site")
(ess-toggle-underscore nil)


(require 'editorconfig)
(editorconfig-mode 1)


(require 'pc-select)
(global-set-key (kbd "S-<prior>") 'scroll-down-mark)
(global-set-key (kbd "S-<next>") 'scroll-up-mark)
(setq shift-select-mode t)
