;;; org-learn.el --- Implements SuperMemo's incremental learning algorithm

;; Copyright (C) 2009-2017 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw at gnu dot org>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: http://orgmode.org
;; Version: 6.32trans
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; The file implements the learning algorithm described at
;; http://supermemo.com/english/ol/sm5.htm, which is a system for reading
;; material according to "spaced repetition".  See
;; http://en.wikipedia.org/wiki/Spaced_repetition for more details.
;;
;; To use, turn on state logging and schedule some piece of information you
;; want to read.  Then in the agenda buffer type


;;; defining functions not available in Hy
(defn push (element listname)
  (setv listname (cons element listname)))


;;   "If non-None, always reschedule items, even if retention was \"perfect\"."
;;   :type 'boolean
;;   :group 'org-learn
(def org-learn-always-reschedule None)

;;   "Controls the rate at which EF is increased or decreased.
;; Must be a number between 0 and 1 (the greater it is the faster
;; the changes of the OF matrix)."
;;   :type 'float
;;   :group 'org-learn
(def org-learn-fraction 0.5)

(defn initial-optimal-factor (n ef)
  (if (= 1 n)
      4
    ef))

(defn get-optimal-factor (n ef of-matrix)
  (do
    (setv factors (get of-matrix n))
    (or (and factors
            (do
              (setv ef-of (get (cdr factors) ef))
              (and ef-of (cdr ef-of))))
       (initial-optimal-factor n ef))))

(defn set-optimal-factor (n ef of-matrix of)
  (do
    (setv factors (get of-matrix n))
    (if factors
       (do
         (setv ef-of (get (cdr factors) ef))
         (if ef-of
             (setcdr ef-of of)
             (push (cons ef of) (cdr factors))))
       (push (cons n (list* (cons ef of))) of-matrix)))
  of-matrix)

(defn inter-repetition-interval (n ef &optional of-matrix)
  (setv of (get-optimal-factor n ef of-matrix))
  (if (= 1 n)
     of
    (* of (inter-repetition-interval (dec n) ef of-matrix))))

(defn modify-e-factor (ef quality)
  (if (< ef 1.3)
      1.3
    (+ ef (- 0.1 (* (- 5 quality) (+ 0.08 (* (- 5 quality) 0.02)))))))

(defn modify-of (of q fraction)
  (setv temp (* of (+ 0.72 (* q 0.07))))
  (+ (* (- 1 fraction) of) (* fraction temp)))

(defn calculate-new-optimal-factor (interval-used quality used-of
						   old-of fraction)
  "This implements the SM-5 learning algorithm in Lisp.
INTERVAL-USED is the last interval used for the item in question.
QUALITY is the quality of the repetition response.
USED-OF is the optimal factor used in calculation of the last
interval used for the item in question.
OLD-OF is the previous value of the OF entry corresponding to the
relevant repetition number and the E-Factor of the item.
FRACTION is a number belonging to the range (0,1) determining the
rate of modifications (the greater it is the faster the changes
of the OF matrix).

Returns the newly calculated value of the considered entry of the
OF matrix."
  ;; the value proposed for the modifier in case of q=5
  (setv mod5 (/ (inc interval-used) interval-used))
  ;; the value proposed for the modifier in case of q=2
  (setv mod2 (/ (dec interval-used) interval-used))
  ;; the number determining how many times the OF value will
  ;; increase or decrease
  (setv modifier None)
  (if (< mod5 1.05)
     (setv mod5 1.05))
  (if (< mod2 0.75)
     (setv mod5 0.75))
  (if (> quality 4)
     (setv modifier (inc (* (- mod5 1) (- quality 4))))
    (setv modifier (- 1 (* (/ (- 1 mod2) 2) (- 4 quality)))))
  (if (< modifier 0.05)
     (setv modifier 0.05))
  (setv new-of (* used-of modifier))
  (if (> quality 4)
    (if (< new-of old-of)
        (setv new-of old-of)))
  (if (< quality 4)
    (if (> new-of old-of)
        (setv new-of old-of)))
  (setv new-of (+ (* new-of fraction) (* old-of (- 1 fraction))))
  (if (< new-of 1.2)
     (setv new-of 1.2)
    new-of))

(def initial-repetition-state '(-1 1 2.5 None))

(defn determine-next-interval (n ef quality of-matrix)
  (assert (> n 0))
  (assert (and (>= quality 0) (<= quality 5)))
  (if (< quality 3)
      (list* (inter-repetition-interval n ef) (inc n) ef None)
    (do
      (setv next-ef (modify-e-factor ef quality))
      (setv of-matrix
      (set-optimal-factor n next-ef of-matrix
                          (modify-of (get-optimal-factor n ef of-matrix)
             quality org-learn-fraction))
      ef next-ef)
      ;; For a zero-based quality of 4 or 5, don't repeat
      (if (and (>= quality 4)
              (not org-learn-always-reschedule))
         (list* 0 (inc n) ef of-matrix)
       (list* (inter-repetition-interval n ef of-matrix) (inc n)
              ef of-matrix)))))

;;; org-learn.el ends here
