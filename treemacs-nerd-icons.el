;;; treemacs-nerd-icons.el --- Emacs Nerd Font Icons theme for treemacs -*- lexical-binding: t -*-

;; Copyright (C) 2023 Hongyu Ding <rainstormstudio@yahoo.com>

;; Author: Hongyu Ding <rainstormstudio@yahoo.com>
;; Keywords: lisp
;; Version: 0.0.1

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

;; emacs-nerd-icons theme for treemacs

;;; Code:

(require 'emacs-nerd-icons)
(require 'treemacs)

(defface treemacs-emacs-nerd-icons-root-face
  '((t (:inherit font-lock-string-face)))
  "Face used for the root icon in emacs-nerd-icons theme."
  :group 'treemacs-faces)

(defface treemacs-emacs-nerd-icons-file-face
  '((t (:inherit font-lock-doc-face)))
  "Face used for the directory and file icons in emacs-nerd-icons theme."
  :group 'treemacs-faces)

(defvar treemacs-emacs-nerd-icons-tab (if (bound-and-true-p treemacs-emacs-nerd-icons-tab-font)
                                          (propertize "\t" 'face `((:family ,treemacs-emacs-nerd-icons-tab-font)))
                                        "\t"))

(treemacs-create-theme "emacs-nerd-icons"
  :config
  (progn
    (dolist (item emacs-nerd-icons-extension-icon-alist)
      (let ((extensions (list (nth 0 item)))
            (fn (nth 1 item))
            (key (nth 2 item))
            (plist (nthcdr 3 item)))
        (treemacs-create-icon
         :icon (format "  %s%s" (apply fn key plist) treemacs-emacs-nerd-icons-tab)
         :extensions extensions)))
    
    ;; directory and other icons
    (treemacs-create-icon :icon (format "%s%s" (emacs-nerd-icons-octicon "nf-oct-repo"   :face 'treemacs-emacs-nerd-icons-root-face) treemacs-emacs-nerd-icons-tab)
                          :extensions (root-closed root-open)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s%s" (emacs-nerd-icons-octicon "nf-oct-chevron_down"   :face 'treemacs-emacs-nerd-icons-file-face) treemacs-emacs-nerd-icons-tab (emacs-nerd-icons-octicon "nf-oct-file_directory"  :face 'treemacs-emacs-nerd-icons-file-face) treemacs-emacs-nerd-icons-tab)
                          :extensions (dir-open)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s%s" (emacs-nerd-icons-octicon "nf-oct-chevron_right"   :face 'treemacs-emacs-nerd-icons-file-face) treemacs-emacs-nerd-icons-tab (emacs-nerd-icons-octicon "nf-oct-file_directory"  :face 'treemacs-emacs-nerd-icons-file-face) treemacs-emacs-nerd-icons-tab)
                          :extensions (dir-closed)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s%s" (emacs-nerd-icons-octicon "nf-oct-chevron_down"   :face 'treemacs-emacs-nerd-icons-file-face) treemacs-emacs-nerd-icons-tab (emacs-nerd-icons-octicon "nf-oct-package"  :face 'treemacs-emacs-nerd-icons-file-face) treemacs-emacs-nerd-icons-tab)
                          :extensions (tag-open)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s%s%s" (emacs-nerd-icons-octicon "nf-oct-chevron_right"   :face 'treemacs-emacs-nerd-icons-file-face) treemacs-emacs-nerd-icons-tab (emacs-nerd-icons-octicon "nf-oct-package"  :face 'treemacs-emacs-nerd-icons-file-face) treemacs-emacs-nerd-icons-tab)
                          :extensions (tag-closed)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (emacs-nerd-icons-octicon "nf-oct-tag"   :face 'treemacs-emacs-nerd-icons-file-face) treemacs-emacs-nerd-icons-tab)
                          :extensions (tag-leaf)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (emacs-nerd-icons-octicon "nf-oct-flame"  :face 'emacs-nerd-icons-red) treemacs-emacs-nerd-icons-tab)
                          :extensions (error)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (emacs-nerd-icons-octicon "nf-oct-stop"  :face 'emacs-nerd-icons-yellow) treemacs-emacs-nerd-icons-tab)
                          :extensions (warning)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (emacs-nerd-icons-octicon "nf-oct-info"   :face 'emacs-nerd-icons-blue) treemacs-emacs-nerd-icons-tab)
                          :extensions (info)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (emacs-nerd-icons-mdicon "nf-md-mail"   :face 'treemacs-emacs-nerd-icons-file-face) treemacs-emacs-nerd-icons-tab)
                          :extensions (mail)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (emacs-nerd-icons-octicon "nf-oct-bookmark"   :face 'treemacs-emacs-nerd-icons-file-face) treemacs-emacs-nerd-icons-tab)
                          :extensions (bookmark)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (emacs-nerd-icons-mdicon "nf-md-monitor"   :face 'treemacs-emacs-nerd-icons-file-face) treemacs-emacs-nerd-icons-tab)
                          :extensions (screen)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (emacs-nerd-icons-mdicon "nf-md-home"   :face 'treemacs-emacs-nerd-icons-file-face) treemacs-emacs-nerd-icons-tab)
                          :extensions (house)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (emacs-nerd-icons-faicon "nf-fa-list"   :face 'treemacs-emacs-nerd-icons-file-face) treemacs-emacs-nerd-icons-tab)
                          :extensions (list)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (emacs-nerd-icons-mdicon "nf-md-repeat"   :face 'treemacs-emacs-nerd-icons-file-face) treemacs-emacs-nerd-icons-tab)
                          :extensions (repeat)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (emacs-nerd-icons-faicon "nf-fa-suitcase"   :face 'treemacs-emacs-nerd-icons-file-face) treemacs-emacs-nerd-icons-tab)
                          :extensions (suitcase)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (emacs-nerd-icons-mdicon "nf-md-close"   :face 'treemacs-emacs-nerd-icons-file-face) treemacs-emacs-nerd-icons-tab)
                          :extensions (close)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (emacs-nerd-icons-octicon "nf-oct-calendar"   :face 'treemacs-emacs-nerd-icons-file-face) treemacs-emacs-nerd-icons-tab)
                          :extensions (calendar)
                          :fallback 'same-as-icon)
    (treemacs-create-icon :icon (format "%s%s" (emacs-nerd-icons-faicon "nf-fa-briefcase"   :face 'treemacs-emacs-nerd-icons-file-face) treemacs-emacs-nerd-icons-tab)
                          :extensions (briefcase)
                          :fallback 'same-as-icon)

    (treemacs-create-icon :icon (format "  %s%s" (emacs-nerd-icons-faicon "nf-fa-file_o" :face 'treemacs-nerd-icons-file-face) treemacs-emacs-nerd-icons-tab)
                          :extensions (fallback)
                          :fallback 'same-as-icon)
    ))

(provide 'treemacs-nerd-icons)
;;; treemacs-nerd-icons.el ends here
