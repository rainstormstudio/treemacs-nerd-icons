;;; treemacs-nerd-icons.el --- Emacs Nerd Font Icons theme for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2023 Hongyu Ding <rainstormstudio@yahoo.com>

;; Author: Hongyu Ding <rainstormstudio@yahoo.com>
;; Keywords: lisp
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3") (nerd-icons "0.0.1") (treemacs "0.0"))
;; URL: https://github.com/rainstormstudio/treemacs-nerd-icons
;; Keywords: files, icons, treemacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; nerd-icons theme for treemacs

;;; Code:

(require 'nerd-icons)
(require 'treemacs)

(defface treemacs-nerd-icons-root-face
  '((t (:inherit nerd-icons-dorange)))
  "Face used for the root icon in nerd-icons theme."
  :group 'treemacs-faces)

(defface treemacs-nerd-icons-file-face
  '((t (:inherit nerd-icons-orange)))
  "Face used for the directory and file icons in nerd-icons theme."
  :group 'treemacs-faces)

(defface treemacs-nerd-icons-error-face
  '((t (:inherit nerd-icons-red)))
  "Face used for the directory and file icons in nerd-icons theme."
  :group 'treemacs-faces)

(defface treemacs-nerd-icons-warning-face
  '((t (:inherit nerd-icons-yellow)))
  "Face used for the directory and file icons in nerd-icons theme."
  :group 'treemacs-faces)

(defface treemacs-nerd-icons-info-face
  '((t (:inherit nerd-icons-blue)))
  "Face used for the directory and file icons in nerd-icons theme."
  :group 'treemacs-faces)

(defcustom treemacs-nerd-icons-icon-size 1.0
  "The default icon size in treemacs."
  :group 'treemacs
  :type 'float)

(defvar treemacs-nerd-icons-tab (propertize "\t" :face 'treemacs-nerd-icons-file-face))

(treemacs-create-theme "nerd-icons"
  :config
  (let* ((sep treemacs-nerd-icons-tab)
         (face 'treemacs-nerd-icons-file-face)
         (size treemacs-nerd-icons-icon-size)
         (chevron_down (nerd-icons-octicon "nf-oct-chevron_down" :face face :height (* 0.75 size) :v-adjust 0.1))
         (chevron_right (nerd-icons-octicon "nf-oct-chevron_right" :face face :height (* 0.75 size) :v-adjust 0.1)))
    (dolist (item nerd-icons-extension-icon-alist)
      (let* ((extension (car item))
             (func (cadr item))
             (args (append (list (cadr (cdr item))) `(:v-adjust 0.0 :height ,size) (cdr (cddr item))))
             (icon (apply func args)))
        (let* ((icon-pair (cons (format " %s%s%s" sep icon sep) (format " %s%s%s" sep icon sep)))
               (gui-icons (treemacs-theme->gui-icons treemacs--current-theme))
               (tui-icons (treemacs-theme->tui-icons treemacs--current-theme))
               (gui-icon  (car icon-pair))
               (tui-icon  (cdr icon-pair)))
          (ht-set! gui-icons extension gui-icon)
          (ht-set! tui-icons extension tui-icon))))

    ;; directory and other icons
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-octicon "nf-oct-repo" :face 'treemacs-nerd-icons-root-face :height (* 1.2 size)) sep)
     :extensions (root-closed root-open)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_down sep (nerd-icons-sucicon "nf-custom-folder_open" :face face :height size) sep)
     :extensions (dir-open)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_right sep (nerd-icons-sucicon "nf-custom-folder_oct" :face face :height size) sep)
     :extensions (dir-closed)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_down sep (nerd-icons-octicon "nf-oct-code" :face face :height size) sep)
     :extensions ("src-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_right sep (nerd-icons-octicon "nf-oct-code" :face face :height size) sep)
     :extensions ("src-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_down sep (nerd-icons-sucicon "nf-custom-folder_open" :face face :height size) sep)
     :extensions ("build-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_right sep (nerd-icons-mdicon "nf-md-folder_cog" :face face :height size) sep)
     :extensions ("build-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_down sep (nerd-icons-sucicon "nf-custom-folder_open" :face face :height size) sep)
     :extensions ("test-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_right sep (nerd-icons-mdicon "nf-md-folder_check" :face face :height size) sep)
     :extensions ("test-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_down sep (nerd-icons-sucicon "nf-custom-folder_open" :face face :height size) sep)
     :extensions ("bin-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_right sep (nerd-icons-mdicon "nf-md-folder_zip" :face face :height size) sep)
     :extensions ("bin-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_down sep (nerd-icons-sucicon "nf-custom-folder_open" :face face :height size) sep)
     :extensions ("git-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_right sep (nerd-icons-sucicon "nf-custom-folder_git" :face face :height size) sep)
     :extensions ("git-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_down sep (nerd-icons-sucicon "nf-custom-folder_open" :face face :height size) sep)
     :extensions ("github-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_right sep (nerd-icons-sucicon "nf-custom-folder_github" :face face :height size) sep)
     :extensions ("github-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_down sep (nerd-icons-sucicon "nf-custom-folder_open" :face face :height size) sep)
     :extensions ("public-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_right sep (nerd-icons-mdicon "nf-md-folder_eye" :face face :height size) sep)
     :extensions ("public-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_down sep (nerd-icons-sucicon "nf-custom-folder_open" :face face :height size) sep)
     :extensions ("private-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_right sep (nerd-icons-mdicon "nf-md-folder_lock" :face face :height size) sep)
     :extensions ("private-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_down sep (nerd-icons-sucicon "nf-custom-folder_open" :face face :height size) sep)
     :extensions ("temp-open" "tmp-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_right sep (nerd-icons-mdicon "nf-md-folder_question" :face face :height size) sep)
     :extensions ("temp-closed" "tmp-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_down sep (nerd-icons-sucicon "nf-custom-folder_open" :face face :height size) sep)
     :extensions ("readme-open" "docs-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_right sep (nerd-icons-mdicon "nf-md-folder_file" :face face :height size) sep)
     :extensions ("readme-closed" "docs-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_down sep (nerd-icons-sucicon "nf-custom-folder_open" :face face :height size) sep)
     :extensions ("screenshots-open" "icons-open")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_right sep (nerd-icons-mdicon "nf-md-folder_image" :face face :height size) sep)
     :extensions ("screenshots-closed" "icons-closed")
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_down sep (nerd-icons-octicon "nf-oct-package" :face face :height size) sep)
     :extensions (tag-open)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s%s%s" chevron_right sep (nerd-icons-octicon "nf-oct-package" :face face :height size) sep)
     :extensions (tag-closed)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-octicon "nf-oct-tag" :face face :height size) sep)
     :extensions (tag-leaf)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-octicon "nf-oct-flame" :face 'treemacs-nerd-icons-error-face :height size) sep)
     :extensions (error)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-octicon "nf-oct-stop" :face 'treemacs-nerd-icons-warning-face :height size) sep)
     :extensions (warning)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-octicon "nf-oct-info" :face 'treemacs-nerd-icons-info-face :height size) sep)
     :extensions (info)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-mdicon "nf-md-mail" :face face :height size) sep)
     :extensions (mail)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-octicon "nf-oct-bookmark" :face face :height size) sep)
     :extensions (bookmark)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-mdicon "nf-md-monitor" :face face :height size) sep)
     :extensions (screen)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-mdicon "nf-md-home" :face face :height size) sep)
     :extensions (house)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-faicon "nf-fa-list" :face face :height size) sep)
     :extensions (list)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-mdicon "nf-md-repeat" :face face :height size) sep)
     :extensions (repeat)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-faicon "nf-fa-suitcase" :face face :height size) sep)
     :extensions (suitcase)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-mdicon "nf-md-close" :face face :height size) sep)
     :extensions (close)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-octicon "nf-oct-calendar" :face face :height size) sep)
     :extensions (calendar)
     :fallback 'same-as-icon)
    (treemacs-create-icon
     :icon (format "%s%s" (nerd-icons-faicon "nf-fa-briefcase" :face face :height size) sep)
     :extensions (briefcase)
     :fallback 'same-as-icon)

    (treemacs-create-icon
     :icon (format " %s%s%s" sep (nerd-icons-faicon "nf-fa-file_o" :face face :height size) sep)
     :extensions (fallback)
     :fallback 'same-as-icon)))

;;;###autoload
(defun treemacs-nerd-icons-config ()
  "Install treemacs configuration."
  (treemacs-load-theme "nerd-icons"))

(provide 'treemacs-nerd-icons)
;;; treemacs-nerd-icons.el ends here
