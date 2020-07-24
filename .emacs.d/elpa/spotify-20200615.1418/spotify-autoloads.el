;;; spotify-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "spotify" "spotify.el" (0 0 0 0))
;;; Generated autoloads from spotify.el
 (autoload 'spotify-play "spotify" "Call Play on spotify player." t)
 (autoload 'spotify-pause "spotify" "Call Pause on spotify player." t)
 (autoload 'spotify-playpause "spotify" "Call PlayPause on spotify player." t)
 (autoload 'spotify-next "spotify" "Call Next on spotify player." t)
 (autoload 'spotify-previous "spotify" "Call Previous on spotify player." t)
 (autoload 'spotify-current "spotify" "Return the current song playing in spotify application." t)
 (autoload 'spotify-quit "spotify" "Quit the spotify application." t)
 (autoload 'spotify-enable-song-notifications "spotify" "Enable notifications for the currently playing song in spotify application." t)
 (autoload 'spotify-disable-song-notifications "spotify" "Disable notifications for the currently playing song in spotify application." t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "spotify" '("spotify-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; spotify-autoloads.el ends here
