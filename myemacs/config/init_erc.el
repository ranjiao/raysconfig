;; user infomation
(setq erc-nick "ConfusedFly"
      erc-user-full-name "Ran Jiao")

(setq erc-default-coding-system '(utf-8 utf-8))

;; auto join room after connected to a server
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '(("oftc.net" "#awesome")))

(setq erc-interpret-mirc-color t) ;interpret mirc colors
(setq erc-kill-buffer-on-part t)
(setq erc-kill-queries-on-quit t)
(setq erc-kill-server-buffer-on-quit t)

;; auto start itc connection while loading emacs
;(erc :server "irc.freenode.net" :port 6667 :nick "ConfusedFly")
;(erc :server "irc.oftc.net" :port 6667 :nick "ConfusedFly")

(global-set-key "\C-cef"
                (lambda()
                  (interactive)
                  (erc :server "irc.freenode.net"
                       :port "6667"
                       :nick "ConfusedFly")))
(global-set-key "\C-ceg"
                (lambda()
                  (interactive)
                  (erc :server "irc.gnome.org"
                       :port "6667"
                       :nick "ConfusedFly")))

(require 'erc-fill)
(erc-fill-mode t)

(erc-timestamp-mode t)
(setq erc-timestamp-mode "[%R-%m/%d]")


