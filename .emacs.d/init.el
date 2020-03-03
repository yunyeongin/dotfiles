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
  (scroll-bar-mode 0))

