;;; pgne.el --- Electric pairing that works with tree-sitter  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Andreas Fuchs

;; Author: Andreas Fuchs <asf@boinkor.net>
;; Keywords: languages, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package works similar to electric-pair-mode and the
;; auto-balancing smartparens mode, in that it lets you type an
;; opening sequence and then will insert the corresponding closing
;; sequence. In contrast to electric-pair-mode, pgne works with
;; multiple character long opening sequences (think "def"/"end" in
;; ruby and "let"/"in" in nix-lang).
;;
;; Why "pgne"? It's an electric company that on occasion will set a
;; pair of trees on fire.

;;; Code:

(require 'treesit)

(cl-defstruct (pgne-trigger (:constructor pgne--make-trigger)
                            (:copier nil))
  major-mode
  predicate
  trigger
  end-sequence
  pad-with-whitespace
  in-comments-or-strings
  activate-in-good-parse-states
  )

(defvar-local pgne--local-triggers nil
  "List of triggers for a given buffer.

Updated by setting up `pgne-mode' in the buffer.")

(defconst pgne-pair-list '()
  "Pairs that pgne recognizes, in the form of a `pgne-trigger'.

Upon activation of `pgne-mode', the pairings matching their major
 mode are compiled into an internal representation of triggers,
 so if this list changes, you have to re-activate `pgne-mode'.")

(cl-defun pgne--node-matches-query-p (query node)
  (treesit-query-capture node query))

(cl-defun pgne--node-does-not-match-queries-p (queries node)
  (not (cl-reduce (lambda (last query)
                    (or last
                        (pgne--node-matches-query-p query node)))
                  queries
                  :initial-value nil)))

(cl-defun pgne-define-pair (&key mode treesit-language node-query negative-node-queries predicate
                                 trigger end-sequence pad-with-whitespace in-comments-or-strings
                                 activate-in-good-parse-states)
  (cl-assert (and mode end-sequence treesit-language)
             nil "Need MODE, TREESIT-LANGUAGE and END-SEQUENCE set.")
  (when node-query
    (cl-assert (not (and predicate node-query))
               nil "PREDICATE and NODE-QUERY can't both be set.")
    (setq predicate (let ((query (treesit-query-compile treesit-language node-query))
                          (negative-queries (mapc (lambda (q) (treesit-query-compile treesit-language q))
                                                  negative-node-queries)))
                      (lambda (node)
                        (and (pgne--node-matches-query-p query node)
                             (pgne--node-does-not-match-queries-p negative-queries (treesit-node-parent node)))))))
  (add-to-list 'pgne-pair-list
               (pgne--make-trigger :major-mode mode
                                   :predicate predicate
                                   :trigger trigger
                                   :end-sequence end-sequence
                                   :pad-with-whitespace pad-with-whitespace
                                   :in-comments-or-strings in-comments-or-strings
                                   :activate-in-good-parse-states activate-in-good-parse-states)))

(defun pgne-trigger/whitespace (event)
  (or (eql event ?\n)
      (not (not (memq (char-syntax event) '(?\s ?-))))))

(defun pgne-trigger/paren (event)
  (not (not (= (char-syntax event) ?\())))

(defun pgne--position-for-trigger (trigger)
  (cl-ecase trigger
    (pgne-trigger/whitespace (1- (point)))
    (pgne-trigger/paren (point))))

(progn
  (setq pgne-pair-list nil)
  (pgne-define-pair :mode 'nix-ts-mode
                    :treesit-language 'nix
                    :node-query '("let" @keyword)
                    :trigger 'pgne-trigger/whitespace
                    :end-sequence "in"
                    :pad-with-whitespace t
                    :activate-in-good-parse-states t)

  (pgne-define-pair :mode 'nix-ts-mode
                    :treesit-language 'nix
                    :node-query '("=" @eq)
                    :trigger 'pgne-trigger/whitespace
                    :end-sequence ";"
                    :activate-in-good-parse-states t)

  (pgne-define-pair :mode 'nix-ts-mode
                    :treesit-language 'nix
                    :node-query '("if" @eq)
                    :trigger 'pgne-trigger/whitespace
                    :end-sequence "then else"
                    :pad-with-whitespace t
                    :activate-in-good-parse-states t)

  (pgne-define-pair :mode 'nix-ts-mode
                    :treesit-language 'nix
                    :node-query '("{" @keyword)
                    :trigger 'pgne-trigger/paren
                    :end-sequence "}")

  (pgne-define-pair :mode 'nix-ts-mode
                    :treesit-language 'nix
                    :node-query '("[" @keyword)
                    :trigger 'pgne-trigger/paren
                    :end-sequence "]")

  (pgne-define-pair :mode 'nix-ts-mode
                    :treesit-language 'nix
                    :node-query '("(" @keyword)
                    :trigger 'pgne-trigger/paren
                    :end-sequence ")"))

(defun pgne--compile-pairs-to-triggers ()
  "Compiles pairs to triggers for a major mode."
  (let ((triggers (make-hash-table)))
    (dolist (pair pgne-pair-list)
      (when (eq (pgne-trigger-major-mode pair) major-mode)
        ;; insert pair on the set of triggers for a key:
        (push pair (gethash (pgne-trigger-trigger pair) triggers))))
    triggers))

;;:autoload
(define-minor-mode pgne-mode
  "A minor mode for inserting balanced pairs, based on tree-sitter queries."
  :lighter "pgne"
  (cond (pgne-mode
         (add-hook 'post-self-insert-hook 'pgne-match-and-pair 100 t)
         (setq-local pgne--local-triggers (pgne--compile-pairs-to-triggers)))
        (t
         (remove-hook 'post-self-insert-hook 'pgne-match-and-pair t))))

(cl-defun pgne--trigger-event-p (event)
  "Return t if the event in question is a balancing trigger in the current buffer."
  (maphash (lambda (trigger _)
             (when (funcall trigger event)
               (cl-return-from pgne--trigger-event-p t)))
           pgne--local-triggers))

(defun pgne--subtree-contains-error-p (&optional node)
  "Return non-nil if the current subtree contains an ERROR node."
  (string=
   (treesit-node-type
    (treesit-parent-until (or node (treesit-node-at (point)))
                          (lambda (node) (treesit-node-check node 'has-error))))
   "ERROR"))

(cl-defun pgne-match-and-pair ()
  (let ((last-event last-command-event))
    (maphash
     (lambda (event-trigger triggers)
       (when (funcall event-trigger last-event)
         (let* ((node-point (pgne--position-for-trigger event-trigger))
                (node (treesit-node-at node-point))
                (subtree-has-error-p (pgne--subtree-contains-error-p node)))
           (dolist (trigger triggers)
             (save-excursion
               (when (and
                      (or (pgne-trigger-in-comments-or-strings trigger)
                          (not (nth 3 (syntax-ppss))))
                      (or subtree-has-error-p
                          (pgne-trigger-activate-in-good-parse-states trigger))
                      (funcall (pgne-trigger-predicate trigger) node))
                 ;; This is the one we should use:
                 (when (pgne-trigger-pad-with-whitespace trigger)
                   (insert " "))
                 (insert (pgne-trigger-end-sequence trigger))
                 (when (pgne-trigger-pad-with-whitespace trigger)
                   (insert " "))
                 (cl-return-from pgne-match-and-pair t)))))))
     pgne--local-triggers)))


(provide 'pgne)
;;; pgne.el ends here
