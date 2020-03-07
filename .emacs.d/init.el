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

(require 'package); package.elを有効化
;; パッケージリポジトリにMarmaladeとMELPAを追加
 (add-to-list
  'package-archives
  '("marmalade" . "https://marmalade-repo.org/packages/"))
;; (add-to-list
;;  'package-archives
;;  '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list
;;  'package-archives
;;  '("gnu" . "https://elpa.gnu.org/packages/"))
;; (add-to-list
;;  'package-archives
;;  '("org" . "https://orgmode.org/elpa/"))
;;  
 (package-initialize)
 (setq package-archives
       '(("gnu" . "http://elpa.gnu.org/packages/")
         ("melpa" . "http://melpa.org/packages/")
         ("org" . "http://orgmode.org/elpa/")))

;; (package-initialize) ; インストール済みのElispを読み込む

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

;;(set-face-background 'default "#303030")

;; リージョンの背景色を変更
;;(set-face-background 'region "darkgreen")

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
;;(set-face-foreground 'default "white")

;;paren-mode: 対応する格好を強調して表示する
(setq show-paren-delay 0) ;表示までの秒数 初期値0.125
(show-paren-mode t) ; 有効化
;; parenのスタイル: expressionは括弧内も強調表示
(setq show-paren-style 'expression)

(set-face-attribute 'show-paren-match nil
      :background 'unspecified
      :underline "#ff6a6a")

(load-theme 'zenburn t)
(global-auto-revert-mode t)

;; emacs-lisp-modeのフックをセット
;;(add-hook 'emacs-lisp-mode-hook
  ;;        '(lamdbda ()
    ;;                (when (require 'eldoc nil t)
      ;;                (setq eldoc-idle-delay 0.2)
        ;;              (setq eldoc-echo-area-use-multiline-p t)
          ;;            (turn-on-eldoc-mode))))


;;helm 設定
(require 'helm-config)
(require 'helm-descbinds)
(helm-descbinds-mode)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

;; M-yにhelm-show-kill-ringを割り当てる
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-M-o") 'helm-occur)
(global-set-key (kbd "C-x b") 'helm-mini)  
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; auto-complateの設定
(when (require 'auto-complete-config nil t)
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default)
  (setq ac-use-menu-map t)
  (setq ac-ignore-case nil))

;; wgrepの設定
(require 'wgrep nil t)

;;undohistの設定
(when (require 'undohist nil t)
  (undohist-initialize))

;; undo-treeの設定
(when (require 'undo-tree nil t)
  ;; C-'にリドゥを割り当てる
  (define-key global-map (kbd "C-'") 'undo-tree-redo)
  (global-undo-tree-mode))

;; elscreen の設定
(when (require 'elscreen nil t)
  (elscreen-start))

;; howmメモ保存の場所
(setq howm-directory (concat user-emacs-directory "~/howm"))

;; howm-menuの言語を日本語に
;; (setq howm-menu-lang 'ja)

;;howmメモを1日１ファイルにする
(setq howm-file-name-format "%Y/%m/%Y-%m-%d.howm")

;; howm-modeを読み込む
(when (require 'howm-mode nil t)
  ;; C-c でhowm-menuを起動
  (define-key global-map (kbd "C-c ,,") 'howm-menu))

;; howmメモを保存と同時に閉じる
(defun howm-save-buffer-and-kill ()
  "howmメモを保存と同時に閉じます。"
  (interactive)
  (when (and (buffer-file-name)
             (howm-buffer-p))
    (save-buffer)
    (kill-buffer nil)))

;;C-c C-c でメモを保存と同時にバッファを閉じる
(define-key howm-mode-map (kbd "C-c C-c") 'howm-save-buffer-and-kill)

;; cua-modeの設定
(cua-mode t) ;cua-modeをオン
(setq cua-enable-cua-keys nil) ;CUAキーバインドを無効にする

;; web-mode設定
(when (require 'web-mode nil t)
  ;; 自動的にweb-modeを起動したい拡張子を追加する。
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  ;; web-modeのインデント設定用フック
  ;; (defun web-mode-hook ()
  ;; "Hooks for Web mode."
  ;; (setq web-mode-markup-indent-offset 2) ;HTML's indent
  ;; (setq web-mode-css-indent-offset 2) ;CSS's indent
  ;; (setq web-mode-code-indent-offset 2) ; JS,PHP,Ruby,etc's indent
  ;; (setq web-mode-comment-style 2) ; web-mode内のコメントのインデント
  ;; (setq web-mode-style-padding 1) ;<style>内のインデント開始レベル
  ;; (setq web-mode-script-padding 1) ;<script>内のインデント開始レベル
  ;; )
  ;; (add-hook 'web-mode-hook 'web-mode-hook)
  )

;; js2-modeの設定
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; React (JSX) を使う場合はこちら
;; (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))


;;flycheckの設定
(add-hook 'after-init-hook #'global-flycheck-mode)

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

(setq gtags-suggested-key-mapping t)
(setq gtags-auto-update t)

;; helm-gtagsの設定
(custom-set-variables
 '(helm-gtags-suggested-key-mapping t)
 '(helm-gtags-auto-update t))

;; projectile
(when (require 'projectile nil t)
  (projectile-mode)
  (add-to-list
   'projectile-globally-ignored-directories
   "node_modules")

  (setq projectile-enable-caching t))

(define-key projectile-mode-map
  (kbd "s-p") 'projectile-command-map)

;; helm-projectile

(when (require 'helm-projectile nil t)
  (setq projectile-completion-system 'helm))

;; git-gutter-fringe
(require 'git-gutter-fringe)
(global-git-gutter-mode t)
(global-set-key (kbd "C-x p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x n") 'git-gutter:next-hunk)

;; multi-termの設定
(when (require 'multi-term nil t)
  ;; 使用するシェルを指定
  (setq multi-term-program "/usr/local/bin/zsh"))

;; 既存のソースを読み込む
(require 'helm-elisp)
(require 'helm-man)

;; 基本となるソースを定義
(setq helm-for-document-sources
      '(helm-source-info-elisp
        helm-source-info-cl
        helm-source-info-pages
        helm-source-man-pages))


;; man cache
(setq woman-cache-filename "~/.emacs.d/.wmncach.el")

;; man path
(setq woman-manpath '("/usr/share/man"
                      "/usr/local/share/man"
                      "/usr/local/share/man/ja"))


;; helm-for-documentコマンドを定義
(defun helm-for-document ()
  "Preconfigured 'helm' for helm-for-document."
  (interactive)
  (let ((default (thing-at-point 'symbol)))
    (helm :sources
          (nconc
           (mapcar (lambda (func)
                     (funcall func default))
                   helm-apropos-function-list)
           helm-for-document-sources)
          :buffer "*helm for document*")))

;; s-dにhelm-for-documentを割り当て
(define-key global-map (kbd "s-d") 'helm-for-document)

(provide 'init)
;;; init.el ends here
