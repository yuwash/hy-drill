;;; -*- coding: utf-8-unix -*-
;;; org-drill.el - Self-testing using spaced repetition
;;;
;;; Copyright (C) 2010-2015  Paul Sexton
;;;
;;; Author: Paul Sexton <eeeickythump@gmail.com>
;;; Version: 2.6.1
;;; Keywords: flashcards, memory, learning, memorization
;;; Repository at http://bitbucket.org/eeeickythump/org-drill/
;;;
;;; This file is not part of GNU Emacs.
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;
;;;
;;; Synopsis
;;; ========
;;;
;;; Within an Org mode outline or outlines, headings and associated content are
;;; treated as "flashcards". Spaced repetition algorithms are used to conduct
;;; interactive "drill sessions", where a selection of these flashcards is
;;; presented to the student in random order. The student rates his or her
;;; recall of each item, and this information is used to schedule the item for
;;; later revision.
;;;
;;; Each drill session can be restricted to topics in the current buffer
;;; (default), one or several files, all agenda files, or a subtree. A single
;;; topic can also be tested.
;;;
;;; Different "card types" can be defined, which present their information to
;;; the student in different ways.
;;;
;;; See the file README.org for more detailed documentation.


(defcustom org-drill-sm5-initial-interval
  4.0
  "In the SM5 algorithm, the initial interval after the first
successful presentation of an item is always 4 days. If you wish to change
this, you can do so here."
  :group 'org-drill
  :type 'float)


(defcustom org-drill-learn-fraction
  0.5
  "Fraction between 0 and 1 that governs how quickly the spaces
between successive repetitions increase, for all items. The
default value is 0.5. Higher values make spaces increase more
quickly with each successful repetition. You should only change
this in small increments (for example 0.05-0.1) as it has an
exponential effect on inter-repetition spacing."
  :group 'org-drill
  :type 'float)


;;; SM2 Algorithm =============================================================


(defun determine-next-interval-sm2 (last-interval n ef quality
                                                  failures meanq total-repeats)
  "Arguments:
- LAST-INTERVAL -- the number of days since the item was last reviewed.
- REPEATS -- the number of times the item has been successfully reviewed
- EF -- the 'easiness factor'
- QUALITY -- 0 to 5

Returns a list: (INTERVAL REPEATS EF FAILURES MEAN TOTAL-REPEATS OFMATRIX), where:
- INTERVAL is the number of days until the item should next be reviewed
- REPEATS is incremented by 1.
- EF is modified based on the recall quality for the item.
- OF-MATRIX is not modified."
  (if (zerop n) (setq n 1))
  (if (null ef) (setq ef 2.5))
  (setq meanq (if meanq
                  (/ (+ quality (* meanq total-repeats 1.0))
                     (1+ total-repeats))
                quality))
  (assert (> n 0))
  (assert (and (>= quality 0) (<= quality 5)))
  (if (<= quality org-drill-failure-quality)
      ;; When an item is failed, its interval is reset to 0,
      ;; but its EF is unchanged
      (list -1 1 ef (1+ failures) meanq (1+ total-repeats)
            org-drill-sm5-optimal-factor-matrix)
    ;; else:
    (let* ((next-ef (modify-e-factor ef quality))
           (interval
            (cond
             [(<= n 1) 1]
             [(= n 2)
              (cond
               [org-drill-add-random-noise-to-intervals-p
                (case quality
                  (5 6)
                  (4 4)
                  (3 3)
                  (2 1)
                  (t -1))]
               [t 6])]
             [t (* last-interval next-ef)])))
      (list (if org-drill-add-random-noise-to-intervals-p
                (+ last-interval (* (- interval last-interval)
                                    (org-drill-random-dispersal-factor)))
              interval)
            (1+ n)
            next-ef
            failures meanq (1+ total-repeats)
            org-drill-sm5-optimal-factor-matrix))))


;;; SM5 Algorithm =============================================================



(defun initial-optimal-factor-sm5 (n ef)
  (if (= 1 n)
      org-drill-sm5-initial-interval
    ef))

(defun get-optimal-factor-sm5 (n ef of-matrix)
  (let ((factors (assoc n of-matrix)))
    (or (and factors
             (let ((ef-of (assoc ef (cdr factors))))
               (and ef-of (cdr ef-of))))
        (initial-optimal-factor-sm5 n ef))))


(defun inter-repetition-interval-sm5 (last-interval n ef &optional of-matrix)
  (let ((of (get-optimal-factor-sm5 n ef (or of-matrix
                                             org-drill-sm5-optimal-factor-matrix))))
    (if (= 1 n)
        of
      (* of last-interval))))


(defun determine-next-interval-sm5 (last-interval n ef quality
                                                  failures meanq total-repeats
                                                  of-matrix &optional delta-days)
  (if (zerop n) (setq n 1))
  (if (null ef) (setq ef 2.5))
  (assert (> n 0))
  (assert (and (>= quality 0) (<= quality 5)))
  (unless of-matrix
    (setq of-matrix org-drill-sm5-optimal-factor-matrix))
  (setq of-matrix (cl-copy-tree of-matrix))

  (setq meanq (if meanq
                  (/ (+ quality (* meanq total-repeats 1.0))
                     (1+ total-repeats))
                quality))

  (let ((next-ef (modify-e-factor ef quality))
        (old-ef ef)
        (new-of (modify-of (get-optimal-factor-sm5 n ef of-matrix)
                           quality org-drill-learn-fraction))
        (interval nil))
    (when (and org-drill-adjust-intervals-for-early-and-late-repetitions-p
               delta-days (minusp delta-days))
      (setq new-of (org-drill-early-interval-factor
                    (get-optimal-factor-sm5 n ef of-matrix)
                    (inter-repetition-interval-sm5
                     last-interval n ef of-matrix)
                    delta-days)))

    (setq of-matrix
          (set-optimal-factor n next-ef of-matrix
                              (round-float new-of 3))) ; round OF to 3 d.p.

    (setq ef next-ef)

    (cond
     ;; "Failed" -- reset repetitions to 0,
     [(<= quality org-drill-failure-quality)
      (list -1 1 old-ef (1+ failures) meanq (1+ total-repeats)
            of-matrix)]     ; Not clear if OF matrix is supposed to be
                                        ; preserved
     ;; For a zero-based quality of 4 or 5, don't repeat
     ;; ((and (>= quality 4)
     ;;       (not org-learn-always-reschedule))
     ;;  (list 0 (1+ n) ef failures meanq
     ;;        (1+ total-repeats) of-matrix))     ; 0 interval = unschedule
     [t
      (setq interval (inter-repetition-interval-sm5
                      last-interval n ef of-matrix))
      (if org-drill-add-random-noise-to-intervals-p
          (setq interval (* interval (org-drill-random-dispersal-factor))))
      (list interval
            (1+ n)
            ef
            failures
            meanq
            (1+ total-repeats)
            of-matrix)])))


;;; Simple8 Algorithm =========================================================


(defun org-drill-simple8-first-interval (failures)
  "Arguments:
- FAILURES: integer >= 0. The total number of times the item has
  been forgotten, ever.

Returns the optimal FIRST interval for an item which has previously been
forgotten on FAILURES occasions."
  (* 2.4849 (exp (* -0.057 failures))))


(defun org-drill-simple8-interval-factor (ease repetition)
  "Arguments:
- EASE: floating point number >= 1.2. Corresponds to `AF' in SM8 algorithm.
- REPETITION: the number of times the item has been tested.
1 is the first repetition (ie the second trial).
Returns:
The factor by which the last interval should be
multiplied to give the next interval. Corresponds to `RF' or `OF'."
  (+ 1.2 (* (- ease 1.2) (expt org-drill-learn-fraction (log repetition 2)))))


(defun org-drill-simple8-quality->ease (quality)
  "Returns the ease (`AF' in the SM8 algorithm) which corresponds
to a mean item quality of QUALITY."
  (+ (* 0.0542 (expt quality 4))
     (* -0.4848 (expt quality 3))
     (* 1.4916 (expt quality 2))
     (* -1.2403 quality)
     1.4515))


(defun determine-next-interval-simple8 (last-interval repeats quality
                                                      failures meanq totaln
                                                      &optional delta-days)
  "Arguments:
- LAST-INTERVAL -- the number of days since the item was last reviewed.
- REPEATS -- the number of times the item has been successfully reviewed
- EASE -- the 'easiness factor'
- QUALITY -- 0 to 5
- DELTA-DAYS -- how many days overdue was the item when it was reviewed.
  0 = reviewed on the scheduled day. +N = N days overdue.
  -N = reviewed N days early.

Returns the new item data, as a list of 6 values:
- NEXT-INTERVAL
- REPEATS
- EASE
- FAILURES
- AVERAGE-QUALITY
- TOTAL-REPEATS.
See the documentation for `org-drill-get-item-data' for a description of these."
  (assert (>= repeats 0))
  (assert (and (>= quality 0) (<= quality 5)))
  (assert (or (null meanq) (and (>= meanq 0) (<= meanq 5))))
  (let ((next-interval nil))
    (setf meanq (if meanq
                    (/ (+ quality (* meanq totaln 1.0)) (1+ totaln))
                  quality))
    (cond
     [(<= quality org-drill-failure-quality)
      (incf failures)
      (setf repeats 0
            next-interval -1)]
     [(or (zerop repeats)
          (zerop last-interval))
      (setf next-interval (org-drill-simple8-first-interval failures))
      (incf repeats)
      (incf totaln)]
     [t
      (let* ((use-n
              (if (and
                   org-drill-adjust-intervals-for-early-and-late-repetitions-p
                   (numberp delta-days) (plusp delta-days)
                   (plusp last-interval))
                  (+ repeats (min 1 (/ delta-days last-interval 1.0)))
                repeats))
             (factor (org-drill-simple8-interval-factor
                      (org-drill-simple8-quality->ease meanq) use-n))
             (next-int (* last-interval factor)))
        (when (and org-drill-adjust-intervals-for-early-and-late-repetitions-p
                   (numberp delta-days) (minusp delta-days))
          ;; The item was reviewed earlier than scheduled.
          (setf factor (org-drill-early-interval-factor
                        factor next-int (abs delta-days))
                next-int (* last-interval factor)))
        (setf next-interval next-int)
        (incf repeats)
        (incf totaln))])
    (list
     (if (and org-drill-add-random-noise-to-intervals-p
              (plusp next-interval))
         (* next-interval (org-drill-random-dispersal-factor))
       next-interval)
     repeats
     (org-drill-simple8-quality->ease meanq)
     failures
     meanq
     totaln
     )))
