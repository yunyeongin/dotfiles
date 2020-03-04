;;function for add to load path
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

(add-to-load-path "elisp" "conf" "public_repos")

;;　カスタムファイルを別ファイルにする
(setq custom-file (locate-user-emacs-file "custom.el"))

(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

(load custom-file)

(require 'init-loader)
(init-loader-load "~/.emacs.d/conf") ;設定ファイルがあるディレクトリを指定

;;Macだけに読み込ませる内容をかく
(when (eq system-type 'darwin)
  ;; MacのEmacsでファイル名を正しく扱うために設定
  (require 'ucs-normalize)
  (setq file-name-coading-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

;;ターミナル伊賀はツールバー、スクロールバーを非表示
(when window-system
  ;; tool-barを非表示
  (tool-bar-mode 0)
  ;; scroll-barを非表示
  (scroll-bar-mode 0)
  (set-face-attribute 'show-paren-match nil
      :background 'unspecified
      :underline "#ff6a6a"))
(unless
    (set-face-attribute 'show-paren-match nil
      :background 'unspecified
      :underline "color-103"))

;;カラム番号も表示
(column-number-mode t)

;; ファイルサイズを表示
(size-indication-mode t)

;; 時計を表示(好みに応じてフォーマットを変更可能)
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time-mode t)
;; バッテリー残量を表示
(display-battery-mode t)

;; タイトルバーにファイルのフルパスを表示
(setq frame-title-format "%f")

;;行番号を常に表示
(global-linum-mode t)

;;インデントにタブ文字を使用しない
(setq-default indent-tabs-mode nil)

(set-face-background 'default "#303030")

;; リージョンの背景色を変更
(set-face-background 'region "darkgreen")

;; AsciiフォントをMenloに
(set-face-attribute 'default nil
                    :family "Menlo"
                    :height 120)


(defface my-hl-line-face
  ;; 背景がdarkならば背景色を
  '((((class color) (background dark))
     (:background "NavyBlue" t))
    ;; 背景がlightならば背景色を青に
    (((class color) (background light))
     (:background "LightSkyBlue" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;;　基本文字の色を白にする
(set-face-foreground 'default "white")

;;paren-mode: 対応する格好を強調して表示する
(setq show-paren-delay 0) ;表示までの秒数 初期値0.125
(show-paren-mode t) ; 有効化
;; parenのスタイル: expressionは括弧内も強調表示
(setq show-paren-style 'expression)

;;(set-face-attribute 'show-paren-match nil
  ;;    :background 'unspecified
    ;;  :underline "#ff6a6a")
