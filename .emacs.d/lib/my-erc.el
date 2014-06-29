(require 'erc)
(require 'erc-join)

(setq erc-nick '("tjarvstrand" "tjarvstrand_" "tjarvstrand__"))
(setq erc-user-fullname "Thomas Järvstrand")

(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
  '((".*irc\\.\\(hq\\.kred\\|internal\\.machines\\)"
     "#tech" "#staging" "#upgrade" "#core" "#fred")))

(setq erc-autojoin-channels-alist
  '((".*irc\\.\\(hq\\.kred\\|internal\\.machines\\)"
     "#test")))

;; check channels
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"

                                 "324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))


(setq erc-keywords '("tjarvstrand[_]?[_]?"))

(defun erc-global-notify (matched-type nick msg)
  (interactive)
  (when (and (eq matched-type 'current-nick)
             (not (string-match"^[sS]erver" nick))
             (not (string-match (format "^%s!" (erc-current-nick)) nick)))
    (let ((msg (format "%s: %s" (car (erc-parse-user nick) msg))))
      (message "sending pushover message %s" msg)
      (epushover-notify (url-hexify-string "ERC Message")
                        (url-hexify-string msg)))))

(add-hook 'erc-text-matched-hook 'erc-global-notify)
(erc-match-mode 1)

(defun my-erc-notifo-PRIVMSG (proc parsed)
  (let* ((nick (car (erc-parse-user (erc-response.sender parsed))))
         (target (car (erc-response.command-args parsed)))
         (contents (erc-response.contents parsed))
         (msg (format "<priv> %s: %s" nick contents)))
    (when (and (erc-current-nick-p target)
               (not (erc-is-message-ctcp-and-not-action-p msg)))
      (message "sending pushover message %s" msg)
      (epushover-notify (url-hexify-string "ERC Message") (url-hexify-string msg))
      nil)))
(add-hook 'erc-server-PRIVMSG-functions 'my-erc-notifo-PRIVMSG)

(erc :server "irc.hq.kred")
