;;; treemacs.el --- A tree style file viewer package -*- lexical-binding: t -*-

;; Copyright (C) 2018 Alexander Miller

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
;;; API required for writing extensions for/with treemacs.

;;; Code:

(require 'dash)
(require 'treemacs-branch-creation)
(require 'treemacs-impl)
(require 'treemacs-interface)
(eval-when-compile
  (require 'cl-lib))

(defsubst treemacs-as-icon (string &rest more-properties)
  "Turn STRING into an icon for treemacs.
Optionally include MORE-PROPERTIES (like `face' or `display')."
  (declare (indent 1))
  (apply #'propertize string 'icon t more-properties))

(cl-defmacro treemacs-render-node
    (&key icon
          label-form
          state
          face
          key-form
          more-properties)
  "Macro that produces the strings required to render a single treemacs node.
To be used as a `:render-action' for `treemacs-define-expandable-node'.

ICON is a simple string serving as the node's icon, and must be created with
`treemacs-as-icon'.

LABEL-FORM must return the string that will servce as the node's label text,
based on the element that should be rendered being bound as `item'. So for
example if rendering a list of buffers RENDER-FORM would look like
`(buffer-name item)'.

STATE is the symbol that will identify the type of the node.

FACE is its face.

KEY-FORM is the form to will give the node a unique key, necessary for
compatiblity and integration with follow-mode and filewatch-mode.

MORE-PROPERTIES is a plist that can arbitrarily appended for quick retrieval
later."
  `(list prefix ,icon
         (propertize ,label-form
                     'button '(t)
                     'category 'default-button
                     'face ,face
                     'help-echo nil
                     :state ,state
                     :parent btn
                     :depth depth
                     :key ,key-form
                     ,@more-properties)))

(cl-defmacro treemacs-define-leaf-node (name icon &key ret-action)
  "Define a type of node that is a leaf and cannot be further expanded.

Based on the given NAME this macro will define a `treemacs-%s-state' state
variable and a `treemacs-%s-icon' icon variable.

The ICON is a string that should be created with `treemacs-as-icon'.

RET-ACTION is a function reference that will be invoked when RET is pressed on
a node of this type."
  (declare (indent 1))
  (let ((state-name (intern (format "treemacs-%s-state" name)))
        (icon-name  (intern (format "treemacs-%s-icon" name))))
    `(progn
       (defvar ,state-name ',state-name)
       (defvar ,icon-name ,icon)
       ,(when ret-action
          `(treemacs-define-RET-action ,state-name ,ret-action)))))

(cl-defmacro treemacs-define-expandable-node
    (name &key
          icon-open
          icon-closed
          query-function
          render-action
          root)
  "Define a type of node that can be further expanded.

ICON-OPEN and ICON-CLOSED are strings and must be created by `treemacs-as-icon'.

QUERY-FUNCTION is a form and will be invoked when the node is expanded. It must
provide the list of elements that will be rendered with RENDER-ACTION.

RENDER-ACTION is another form that will render the single items provided by
QUERY-FUNCTION. For every RENDER-FORM invocation the element to be rendered is
bound under the name `item'. The form itself should end in a call to
`treemacs-render-node'.

ROOT is a special form, only needed for the definition of top-level nodes. It is
a list of 3 elements: the root node's label, its face and its key-form. This
special provision is necessary for root nodes, as they are not rendered by means
of another extension function calling `treemacs-render-node'."
  (declare (indent 1))
  (let ((open-icon-name    (intern (concat "treemacs-icon-" (symbol-name name) "-open")))
        (closed-icon-name  (intern (concat "treemacs-icon-" (symbol-name name) "-closed")))
        (open-state-name   (intern (concat "treemacs-" (symbol-name name) "-open-state")))
        (closed-state-name (intern (concat "treemacs-" (symbol-name name) "-closed-state")))
        (expand-name       (intern (concat "treemacs--expand-" (symbol-name name))))
        (collapse-name     (intern (concat "treemacs--collapse-" (symbol-name name)))))
    `(progn
       (defvar ,open-icon-name ,icon-open)
       (defvar ,closed-icon-name ,icon-closed)
       (defvar ,open-state-name ',open-state-name)
       (defvar ,closed-state-name ',closed-state-name)

       (defun ,expand-name (_)
         ,(format "Expand treemacs nodes of type `%s'." name)
         (-when-let (btn (treemacs-current-button))
           (let ((items ,query-function)
                 (depth (1+ (button-get btn :depth))))
             (treemacs--button-open
              :button btn
              :new-state ',open-state-name
              :new-icon ,open-icon-name
              :immediate-insert t
              :open-action
              (treemacs--create-buttons
               :nodes items
               :depth depth
               :node-name item
               :node-action ,render-action)
              :post-open-action
              (treemacs-on-expand
               (button-get btn :key) btn (-some-> (button-get btn :parent) (button-get :key)))))))

       (defun ,collapse-name (_)
         ,(format "Collapse treemacs nodes of type `%s'." name)
         (-when-let (btn (treemacs-current-button))
           (treemacs--button-close
            :button btn
            :new-state ',closed-state-name
            :new-icon ,closed-icon-name
            :post-close-action
            (treemacs-on-collapse (treemacs--tags-path-of btn)))))

       (treemacs-define-TAB-action ',open-state-name #',collapse-name)
       (treemacs-define-TAB-action ',closed-state-name #',expand-name)

       ,(when root
          (-let [(label face key-form) root]
            `(cl-defun ,(intern (format "treemacs--render-%s-node" name)) (&optional (depth 0) parent)
               (insert
                ,closed-icon-name
                (propertize ,label
                            'button '(t)
                            'category 'default-button
                            'face ,face
                            :key ,key-form
                            :depth depth
                            :parent parent
                            :state ,closed-state-name))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun treemacs--get-buffer-groups ()
  "Get the list of buffers, grouped by their major mode."
  (->> (buffer-list)
       (--reject (eq ?\ (aref (buffer-name it) 0)))
       (--group-by (buffer-local-value 'major-mode it))))

(defun treemacs--visit-buffer (_)
  "Switch to buffer saved in node at point."
  (let* ((btn (treemacs-current-button))
         (buf (button-get btn :buffer)))
    (when (buffer-live-p buf)
      (select-window (next-window))
      (switch-to-buffer buf))))

(treemacs-define-leaf-node buffer-leaf
  (treemacs-as-icon "• " 'face 'font-lock-builtin-face)
  :ret-action #'treemacs--visit-buffer)

(treemacs-define-expandable-node buffer-group
  :icon-open (treemacs-as-icon "- " 'face 'font-lock-string-face)
  :icon-closed (treemacs-as-icon "+ " 'face 'font-lock-string-face)
  :query-function (button-get btn :buffers)
  :render-action
  (treemacs-render-node
   :icon treemacs-buffer-leaf-icon
   :label-form (buffer-name item)
   :state treemacs-buffer-leaf-state
   :face 'font-lock-doc-face
   :key-form item
   :more-properties (:buffer item)))

(treemacs-define-expandable-node buffers-root
  :icon-open (treemacs-as-icon "- " 'face 'font-lock-string-face)
  :icon-closed (treemacs-as-icon "+ " 'face 'font-lock-string-face)
  :query-function (treemacs--get-buffer-groups)
  :root ("Buffers" 'font-lock-type-face 'Buffers)
  :render-action
  (treemacs-render-node
   :icon treemacs-icon-buffer-group-closed
   :label-form (symbol-name (car item))
   :state treemacs-buffer-group-closed-state
   :face 'font-lock-keyword-face
   :key-form (car item)
   :more-properties (:buffers (cdr item))))

(provide 'treemacs-extensions)

;;; treemacs-extensions.el ends here
