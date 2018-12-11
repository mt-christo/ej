(require 'ess-site)
(ess-toggle-underscore nil)
;; python:
(setq python-shell-interpreter "python3")
(setq py-python-command "python3")
(setq python-shell-completion-native-enable nil)
(cua-mode t)
(setq-default indent-tabs-mode nil)
(package-initialize)
(elpy-enable)
;;(set-default 'truncate-lines t)
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
 '(elpy-rpc-backend "rope")
 '(elpy-rpc-python-command "python3")
 '(package-selected-packages (quote (elpy)))
 '(python-shell-interpreter "python3" t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(load-theme 'wombat)
