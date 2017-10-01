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


(import math)
(import copy)
(import re)
(import org_learn)


;;; defining functions not available in Hy
(defn incf (item)
  (inc item)
  item)


;;;   "Tag which topics must possess in order to be identified as review topics
;;; by `org-drill'."
;;;   :group 'org-drill
;;;   :type 'string
(def org-drill-question-tag
  "drill")


;;;   "Each drill session will present at most this many topics for review.
;;; Nil means unlimited."
;;;   :group 'org-drill
;;;   :type '(choice integer (const None))
(def org-drill-maximum-items-per-session
  30)


;;;   "Maximum duration of a drill session, in minutes.
;;; Nil means unlimited."
;;;   :group 'org-drill
;;;   :type '(choice integer (const None))
(def org-drill-maximum-duration
  20)


;;;   "If non-nil, when you fail an item it still counts towards the
;;; count of items reviewed for the current session. If nil (default),
;;; only successful items count towards this total."
;;;   :group 'org-drill
;;;   :type 'boolean
(def org-drill-item-count-includes-failed-items-p
  None)

;;;   "If the quality of recall for an item is this number or lower,
;;; it is regarded as an unambiguous failure, and the repetition
;;; interval for the card is reset to 0 days.  If the quality is higher
;;; than this number, it is regarded as successfully recalled, but the
;;; time interval to the next repetition will be lowered if the quality
;;; was near to a fail.
;;; 
;;; By default this is 2, for SuperMemo-like behaviour. For
;;; Mnemosyne-like behaviour, set it to 1.  Other values are not
;;; really sensible."
;;;   :group 'org-drill
;;;   :type '(choice (const 2) (const 1)))
(def org-drill-failure-quality
  2)


;;;   "What percentage of items do you consider it is 'acceptable' to
;;; forget each drill session? The default is 10%. A warning message
;;; is displayed at the end of the session if the percentage forgotten
;;; climbs above this number."
;;;   :group 'org-drill
;;;   :type 'integer
(def org-drill-forgetting-index
  10)


;;;   "If an item is forgotten more than this many times, it is tagged
;;; as a 'leech' item."
;;;   :group 'org-drill
;;;   :type '(choice integer (const None))
(def org-drill-leech-failure-threshold
  15)


;;;   "How should 'leech items' be handled during drill sessions?
;;; Possible values:
;;; - None :: Leech items are treated the same as normal items.
;;; - skip :: Leech items are not included in drill sessions.
;;; - warn :: Leech items are still included in drill sessions,
;;;   but a warning message is printed when each leech item is
;;;   presented."
;;;   :group 'org-drill
;;;   :type '(choice (const warn) (const skip) (const None))
(def org-drill-leech-method
  'skip)


;;;   "Use a special face to highlight cloze-deleted text in org mode
;;; buffers?"
;;;   :group 'org-drill
;;;   :type 'boolean
(def org-drill-use-visible-cloze-face-p
  None)


;;;   "Conceal the contents of the main heading of each item during drill
;;; sessions? You may want to enable this behaviour if item headings or tags
;;; contain information that could 'give away' the answer."
;;;   :group 'org-drill
;;;   :type 'boolean
(def org-drill-hide-item-headings-p
  None)


;;;   "Foreground colour used to display the count of remaining new items
;;; during a drill session."
;;;   :group 'org-drill
;;;   :type 'color
(def org-drill-new-count-color
  "royal blue")

;;;   "Foreground colour used to display the count of remaining mature items
;;; during a drill session. Mature items are due for review, but are not new."
;;;   :group 'org-drill
;;;   :type 'color
(def org-drill-mature-count-color
  "green")

;;;   "Foreground colour used to display the count of remaining failed items
;;; during a drill session."
;;;   :group 'org-drill
;;;   :type 'color
(def org-drill-failed-count-color
  "red")

;;;   "Foreground colour used to display the count of reviewed items
;;; during a drill session."
;;;   :group 'org-drill
;;;   :type 'color
(def org-drill-done-count-color
  "sienna")

;;;   "String used within org buffers to delimit cloze deletions."
;;;   :group 'org-drill
;;;   :type 'string
(def org-drill-left-cloze-delimiter
  "[")

;;;   "String used within org buffers to delimit cloze deletions."
;;;   :group 'org-drill
;;;   :type 'string
(def org-drill-right-cloze-delimiter
  "]")


;;;   "String which, if it occurs within a cloze expression, signifies that the
;;; rest of the expression after the string is a `hint', to be displayed instead of
;;; the hidden cloze during a test."
(def org-drill-hint-separator "||")

(defn org-drill--compute-cloze-regexp ()
  (+ "\\("
     (re.escape org-drill-left-cloze-delimiter)
     "[[:cntrl:][:graph:][:space:]]+?\\)\\(\\|"
     (re.escape org-drill-hint-separator)
     ".+?\\)\\("
     (re.escape org-drill-right-cloze-delimiter)
     "\\)"))

(defn org-drill--compute-cloze-keywords ()
  (list* (list* (org-drill--compute-cloze-regexp)
                (cut '(1 'org-drill-visible-cloze-face None))
                (cut '(2 'org-drill-visible-cloze-hint-face True))
                (cut '(3 'org-drill-visible-cloze-face None))
                )))

(def org-drill-cloze-regexp
  (org-drill--compute-cloze-regexp))


(def org-drill-cloze-keywords
  (org-drill--compute-cloze-keywords))


;; Variables defining what keys can be pressed during drill sessions to quit the
;; session, edit the item, etc.
;;;   "If this character is pressed during a drill session, quit the session."
(def org-drill--quit-key (ord "q"))
;;;   "If this character is pressed during a drill session, suspend the session
;;; with the cursor at the current item.."
(def org-drill--edit-key (ord "e"))
;;;   "If this character is pressed during a drill session, show help."
(def org-drill--help-key (ord "?"))
;;;   "If this character is pressed during a drill session, skip to the next
;;; item."
(def org-drill--skip-key (ord "s"))
;;;   "If this character is pressed during a drill session, edit the tags for
;;; the current item."
(def org-drill--tags-key (ord "t"))


;;;   "Alist associating card types with presentation functions. Each
;;; entry in the alist takes the form:
;;; 
;;; ;;; (CARDTYPE QUESTION-FN [ANSWER-FN DRILL-EMPTY-P])
;;; 
;;; Where CARDTYPE is a string or nil (for default), and QUESTION-FN
;;; is a function which takes no arguments and returns a boolean
;;; value.
;;; 
;;; When supplied, ANSWER-FN is a function that takes one argument --
;;; that argument is a function of no arguments, which when called,
;;; prompts the user to rate their recall and performs rescheduling
;;; of the drill item. ANSWER-FN is called with the point on the
;;; active item's heading, just prior to displaying the item's
;;; 'answer'. It can therefore be used to modify the appearance of
;;; the answer. ANSWER-FN must call its argument before returning.
;;; 
;;; When supplied, DRILL-EMPTY-P is a boolean value, default nil.
;;; When non-nil, cards of this type will be presented during tests
;;; even if their bodies are empty."
;;;   :group 'org-drill
;;;   :type '(alist :key-type (choice string (const None))
;;;                 :value-type function)
(def org-drill-card-type-alist
  '((None org-drill-present-simple-card)
    ("simple" org-drill-present-simple-card)
    ("simpletyped" org-drill-present-simple-card-with-typed-answer)
    ("twosided" org-drill-present-two-sided-card None True)
    ("multisided" org-drill-present-multi-sided-card None True)
    ("hide1cloze" org-drill-present-multicloze-hide1)
    ("hide2cloze" org-drill-present-multicloze-hide2)
    ("show1cloze" org-drill-present-multicloze-show1)
    ("show2cloze" org-drill-present-multicloze-show2)
    ("multicloze" org-drill-present-multicloze-hide1)
    ("hidefirst" org-drill-present-multicloze-hide-first)
    ("hidelast" org-drill-present-multicloze-hide-last)
    ("hide1_firstmore" org-drill-present-multicloze-hide1-firstmore)
    ("show1_lastmore" org-drill-present-multicloze-show1-lastmore)
    ("show1_firstless" org-drill-present-multicloze-show1-firstless)
    ("conjugate"
     org-drill-present-verb-conjugation
     org-drill-show-answer-verb-conjugation)
    ("decline_noun"
     org-drill-present-noun-declension
     org-drill-show-answer-noun-declension)
    ("spanish_verb" org-drill-present-spanish-verb)
    ("translate_number" org-drill-present-translate-number)))


;;;   "The scope in which to search for drill items when conducting a
;;; drill session. This can be any of:
;;; 
;;; file                 The current buffer, respecting the restriction if any.
;;;                      This is the default.
;;; tree                 The subtree started with the entry at point
;;; file-no-restriction  The current buffer, without restriction
;;; file-with-archives   The current buffer, and any archives associated with it.
;;; agenda               All agenda files
;;; agenda-with-archives All agenda files with any archive files associated
;;;                      with them.
;;; directory            All files with the extension '.org' in the same
;;;                      directory as the current file (includes the current
;;;                      file if it is an .org file.)
;;;  (FILE1 FILE2 ...)   If this is a list, all files in the list will be scanned.
;;; "
;;;   ;; Note -- meanings differ slightly from the argument to org-map-entries:
;;;   ;; 'file' means current file/buffer, respecting any restriction
;;;   ;; 'file-no-restriction' means current file/buffer, ignoring restrictions
;;;   ;; 'directory' means all *.org files in current directory
;;;   :group 'org-drill
;;;   :type '(choice (const :tag "The current buffer, respecting the restriction if any." file)
;;;                  (const :tag "The subtree started with the entry at point" tree)
;;;                  (const :tag "The current buffer, without restriction" file-no-restriction)
;;;                  (const :tag "The current buffer, and any archives associated with it." file-with-archives)
;;;                  (const :tag "All agenda files" agenda)
;;;                  (const :tag "All agenda files with any archive files associated with them." agenda-with-archives)
;;;                  (const :tag "All files with the extension '.org' in the same directory as the current file (includes the current file if it is an .org file.)"  directory)
;;;                  (repeat :tag "List of files to scan for drill items." file))
(def org-drill-scope
  'file)


;;;   "If non-nil, a string specifying a tags/property/TODO query. During
;;; drill sessions, only items that match this query will be considered."
;;;   :group 'org-drill
;;;   :type '(choice (const None) string)
(def org-drill-match
  None)


;;;   "If non-nil, prompt to save all modified buffers after a drill session
;;; finishes."
;;;   :group 'org-drill
;;;   :type 'boolean
(def org-drill-save-buffers-after-drill-sessions-p
  True)


;;;   "Which SuperMemo spaced repetition algorithm to use for scheduling items.
;;; Available choices are:
;;; - SM2 :: the SM2 algorithm, used in SuperMemo 2.0
;;; - SM5 :: the SM5 algorithm, used in SuperMemo 5.0
;;; - Simple8 :: a modified version of the SM8 algorithm. SM8 is used in
;;;   SuperMemo 98. The version implemented here is simplified in that while it
;;;   'learns' the difficulty of each item using quality grades and number of
;;;   failures, it does not modify the matrix of values that
;;;   governs how fast the inter-repetition intervals increase. A method for
;;;   adjusting intervals when items are reviewed early or late has been taken
;;;   from SM11, a later version of the algorithm, and included in Simple8."
;;;   :group 'org-drill
;;;   :type '(choice (const sm2) (const sm5) (const simple8))
(def org-drill-spaced-repetition-algorithm
  'sm5)


;;;   "Obsolete and will be removed in future. The SM5 optimal factor
;;; matrix data is now stored in the variable
;;; `org-drill-sm5-optimal-factor-matrix'."
;;;   :group 'org-drill
;;;   :type 'sexp
(def org-drill-optimal-factor-matrix
  None)


;;;   "DO NOT CHANGE THE VALUE OF THIS VARIABLE.
;;; 
;;; Persistent matrix of optimal factors, used by the SuperMemo SM5
;;; algorithm. The matrix is saved at the end of each drill session.
;;; 
;;; Over time, values in the matrix will adapt to the individual user's
;;; pace of learning."
(def org-drill-sm5-optimal-factor-matrix
  None)


(or (in 'savehist-additional-variables
        'org-drill-sm5-optimal-factor-matrix)
    (setv savehist-additional-variables
      (cons 'savehist-additional-variables
            'org-drill-sm5-optimal-factor-matrix)))


(defn org-drill--transfer-optimal-factor-matrix ()
  (if (and org-drill-optimal-factor-matrix
           (none? org-drill-sm5-optimal-factor-matrix))
      (setv org-drill-sm5-optimal-factor-matrix
            org-drill-optimal-factor-matrix)))


;;;   In the SM5 algorithm, the initial interval after the first
;;; successful presentation of an item is always 4 days. If you wish to change
;;; this, you can do so here.
;;;   :group 'org-drill
;;;   :type 'float
(def org-drill-sm5-initial-interval
  4.0)


;;;   "If true, the number of days until an item's next repetition
;;; will vary slightly from the interval calculated by the SM2
;;; algorithm. The variation is very small when the interval is
;;; small, but scales up with the interval."
;;;   :group 'org-drill
;;;   :type 'boolean
(def org-drill-add-random-noise-to-intervals-p
  None)


;;;   "If true, when the student successfully reviews an item 1 or more days
;;; before or after the scheduled review date, this will affect that date of
;;; the item's next scheduled review, according to the algorithm presented at
;;;  [[http://www.supermemo.com/english/algsm11.htm#Advanced%20repetitions]].
;;; 
;;; Items that were reviewed early will have their next review date brought
;;; forward. Those that were reviewed late will have their next review
;;; date postponed further.
;;; 
;;; Note that this option currently has no effect if the SM2 algorithm
;;; is used."
;;;   :group 'org-drill
;;;   :type 'boolean
(def org-drill-adjust-intervals-for-early-and-late-repetitions-p
  None)


;;;   "For card types 'hide1_firstmore', 'show1_lastmore' and 'show1_firstless',
;;; this number determines how often the 'less favoured' situation
;;; should arise. It will occur 1 in every N trials, where N is the
;;; value of the variable.
;;; 
;;; For example, with the hide1_firstmore card type, the first piece
;;; of clozed text should be hidden more often than the other
;;; pieces. If this variable is set to 4 (default), the first item
;;; will only be shown 25% of the time (1 in 4 trials). Similarly for
;;; show1_lastmore, the last item will be shown 75% of the time, and
;;; for show1_firstless, the first item would only be shown 25% of the
;;; time.
;;; 
;;; If the value of this variable is NIL, then weighting is disabled, and
;;; all weighted card types are treated as their unweighted equivalents."
;;;   :group 'org-drill
;;;   :type '(choice integer (const None))
(def org-drill-cloze-text-weight
  4)


;;;   "When in cram mode, items are considered due for review if
;;; they were reviewed at least this many hours ago."
;;;   :group 'org-drill
;;;   :type 'integer
(def org-drill-cram-hours
  12)


;;; NEW items have never been presented in a drill session before.
;;; MATURE items HAVE been presented at least once before.
;;; - YOUNG mature items were scheduled no more than
;;;   ORG-DRILL-DAYS-BEFORE-OLD days after their last
;;;   repetition. These items will have been learned 'recently' and will have a
;;;   low repetition count.
;;; - OLD mature items have intervals greater than
;;;   ORG-DRILL-DAYS-BEFORE-OLD.
;;; - OVERDUE items are past their scheduled review date by more than
;;;   LAST-INTERVAL * (ORG-DRILL-OVERDUE-INTERVAL-FACTOR - 1) days,
;;;   regardless of young/old status.


;;;   "When an item's inter-repetition interval rises above this value in days,
;;; it is no longer considered a 'young' (recently learned) item."
;;;   :group 'org-drill
;;;   :type 'integer
(def org-drill-days-before-old
  10)


;;;   "An item is considered overdue if its scheduled review date is
;;; more than (ORG-DRILL-OVERDUE-INTERVAL-FACTOR - 1) * LAST-INTERVAL
;;; days in the past. For example, a value of 1.2 means an additional
;;; 20% of the last scheduled interval is allowed to elapse before
;;; the item is overdue. A value of 1.0 means no extra time is
;;; allowed at all - items are immediately considered overdue if
;;; there is even one day's delay in reviewing them. This variable
;;; should never be less than 1.0."
;;;   :group 'org-drill
;;;   :type 'float
(def org-drill-overdue-interval-factor
  1.2)


;;;   Fraction between 0 and 1 that governs how quickly the spaces
;;; between successive repetitions increase, for all items. The
;;; default value is 0.5. Higher values make spaces increase more
;;; quickly with each successful repetition. You should only change
;;; this in small increments (for example 0.05-0.1) as it has an
;;; exponential effect on inter-repetition spacing.
;;;   :group 'org-drill
;;;   :type 'float
(def org-drill-learn-fraction
  0.5)


;;;   "Global variable that can be bound to a correct answer when an
;;; item is being presented. If this variable is non-nil, the default
;;; presentation function will show its value instead of the default
;;; behaviour of revealing the contents of the drilled item.
;;; 
;;; This variable is useful for card types that compute their answers
;;; -- for example, a card type that asks the student to translate a
;;; random number to another language. "
(def drill-answer None)


;;;   "Global variable that can be bound to the last answer typed by
;;; the user. Used by card types that ask the user to type in an
;;; answer, rather than just pressing spacebar to reveal the
;;; answer."
(def drill-typed-answer None)


;;;   "If non-nil, when concealing cloze deletions, force the length of
;;; the ellipsis to match the length of the missing text. This may be useful
;;; to preserve the formatting in a displayed table, for example."
;;;   :group 'org-drill
;;;   :type 'boolean
(def org-drill-cloze-length-matches-hidden-text-p
  None)


(def *org-drill-session-qualities* None)
(def *org-drill-start-time* 0)
(def *org-drill-new-entries* None)
(def *org-drill-dormant-entry-count* 0)
(def *org-drill-due-entry-count* 0)
(def *org-drill-overdue-entry-count* 0)
(def *org-drill-due-tomorrow-count* 0)
;;;   "List of markers for items that are considered 'overdue', based on
;;; the value of ORG-DRILL-OVERDUE-INTERVAL-FACTOR."
(def *org-drill-overdue-entries* None)
;;;   "List of markers for mature entries whose last inter-repetition
;;; interval was <= ORG-DRILL-DAYS-BEFORE-OLD days."
(def *org-drill-young-mature-entries* None)
;;;   "List of markers for mature entries whose last inter-repetition
;;; interval was greater than ORG-DRILL-DAYS-BEFORE-OLD days."
(def *org-drill-old-mature-entries* None)
(def *org-drill-failed-entries* None)
(def *org-drill-again-entries* None)
(def *org-drill-done-entries* None)
;;;   "Set to the marker for the item currently being tested."
(def *org-drill-current-item* None)
;;;   "Are we in 'cram mode', where all items are considered due
;;; for review unless they were already reviewed in the recent past?"
(def *org-drill-cram-mode* None)
(def org-drill-scheduling-properties
  '("LEARN_DATA" "DRILL_LAST_INTERVAL" "DRILL_REPEATS_SINCE_FAIL"
    "DRILL_TOTAL_REPEATS" "DRILL_FAILURE_COUNT" "DRILL_AVERAGE_QUALITY"
    "DRILL_EASE" "DRILL_LAST_QUALITY" "DRILL_LAST_REVIEWED"))
;;;   "If non-nil, entries more than 90 days overdue are regarded as 'lapsed'.
;;; This means that when the item is eventually re-tested it will be
;;; treated as 'failed' (quality 2) for rescheduling purposes,
;;; regardless of whether the test was successful."
(def org-drill--lapse-very-overdue-entries-p None)


;;;; Utilities ================================================================


(defn round-float (floatnum fix)
  "Round the floating point number FLOATNUM to FIX decimal places.
Example: (round-float 3.56755765 3) -> 3.568"
  (setv n (math.pow 10 fix))
  (/ (float (round (* floatnum n))) n))


;;; SM2 Algorithm =============================================================


(defn determine-next-interval-sm2 (last-interval n ef quality
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
  (if (zero? n) (setv n 1))
  (if (none? ef) (setv ef 2.5))
  (setv meanq (if meanq
                  (/ (+ quality (* meanq total-repeats 1.0))
                     (inc total-repeats))
                quality))
  (assert (> n 0))
  (assert (and (>= quality 0) (<= quality 5)))
  (if (<= quality org-drill-failure-quality)
      ;; When an item is failed, its interval is reset to 0,
      ;; but its EF is unchanged
      (list* -1 1 ef (inc failures) meanq (inc total-repeats)
            org-drill-sm5-optimal-factor-matrix)
    ;; else:
    (do
           (setv next-ef (org_learn.modify-e-factor ef quality))
           (setv interval
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
                  (True -1))]
               [True 6])]
             [True (* last-interval next-ef)]))
           (list* (if org-drill-add-random-noise-to-intervals-p
                     (+ last-interval (* (- interval last-interval)
                                         (org-drill-random-dispersal-factor)))
                   interval)
                 (inc n)
                 next-ef
                 failures meanq (inc total-repeats)
                 org-drill-sm5-optimal-factor-matrix))))


;;; SM5 Algorithm =============================================================



(defn initial-optimal-factor-sm5 (n ef)
  (if (= 1 n)
      org-drill-sm5-initial-interval
    ef))

(defn get-optimal-factor-sm5 (n ef of-matrix)
  (do (setv factors (get of-matrix n))
    (or (and factors
             (do (setv ef-of (get (drop 1 factors) ef))
               (and ef-of (drop 1 ef-of))))
        (initial-optimal-factor-sm5 n ef))))


(defn inter-repetition-interval-sm5 (last-interval n ef &optional of-matrix)
  (do
    (setv of (get-optimal-factor-sm5 n ef (or of-matrix
                                              org-drill-sm5-optimal-factor-matrix)))
    (if (= 1 n)
        of
      (* of last-interval))))


(defn determine-next-interval-sm5 (last-interval n ef quality
                                                  failures meanq total-repeats
                                                  of-matrix &optional delta-days)
  (if (zero? n) (setv n 1))
  (if (none? ef) (setv ef 2.5))
  (assert (> n 0))
  (assert (and (>= quality 0) (<= quality 5)))
  (unless of-matrix
    (setv of-matrix org-drill-sm5-optimal-factor-matrix))
  (setv of-matrix (copy.deepcopy of-matrix))

  (setv meanq (if meanq
                  (/ (+ quality (* meanq total-repeats 1.0))
                     (inc total-repeats))
                quality))

  (do
    (setv next-ef (org_learn.modify-e-factor ef quality))
    (setv old-ef ef)
    (setv new-of (org_learn.modify-of (get-optimal-factor-sm5 n ef of-matrix)
                                      quality org-drill-learn-fraction))
    (setv interval None)
    (when (and org-drill-adjust-intervals-for-early-and-late-repetitions-p
               delta-days (minusp delta-days))
      (setv new-of (org-drill-early-interval-factor
                    (get-optimal-factor-sm5 n ef of-matrix)
                    (inter-repetition-interval-sm5
                     last-interval n ef of-matrix)
                    delta-days)))

    (setv of-matrix
          (org_learn.set-optimal-factor n next-ef of-matrix
                              (round-float new-of 3))) ; round OF to 3 d.p.

    (setv ef next-ef)

    (cond
     ;; "Failed" -- reset repetitions to 0,
     [(<= quality org-drill-failure-quality)
      (list* -1 1 old-ef (inc failures) meanq (inc total-repeats)
            of-matrix)]     ; Not clear if OF matrix is supposed to be
                                        ; preserved
     ;; For a zero-based quality of 4 or 5, don't repeat
     ;; ((and (>= quality 4)
     ;;       (not org-learn-always-reschedule))
     ;;  (list* 0 (inc n) ef failures meanq
     ;;        (inc total-repeats) of-matrix))     ; 0 interval = unschedule
     [True
      (setv interval (inter-repetition-interval-sm5
                      last-interval n ef of-matrix))
      (if org-drill-add-random-noise-to-intervals-p
          (setv interval (* interval (org-drill-random-dispersal-factor))))
      (list* interval
            (inc n)
            ef
            failures
            meanq
            (inc total-repeats)
            of-matrix)])))


;;; Simple8 Algorithm =========================================================


(defn org-drill-simple8-first-interval (failures)
  "Arguments:
- FAILURES: integer >= 0. The total number of times the item has
  been forgotten, ever.

Returns the optimal FIRST interval for an item which has previously been
forgotten on FAILURES occasions."
  (* 2.4849 (math.exp (* -0.057 failures))))


(defn org-drill-simple8-interval-factor (ease repetition)
  "Arguments:
- EASE: floating point number >= 1.2. Corresponds to `AF' in SM8 algorithm.
- REPETITION: the number of times the item has been tested.
1 is the first repetition (ie the second trial).
Returns:
The factor by which the last interval should be
multiplied to give the next interval. Corresponds to `RF' or `OF'."
  (+ 1.2 (* (- ease 1.2) (math.pow org-drill-learn-fraction (math.log repetition 2)))))


(defn org-drill-simple8-quality->ease (quality)
  "Returns the ease (`AF' in the SM8 algorithm) which corresponds
to a mean item quality of QUALITY."
  (+ (* 0.0542 (math.pow quality 4))
     (* -0.4848 (math.pow quality 3))
     (* 1.4916 (math.pow quality 2))
     (* -1.2403 quality)
     1.4515))


(defn determine-next-interval-simple8 (last-interval repeats quality
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
  (assert (or (none? meanq) (and (>= meanq 0) (<= meanq 5))))
  (do
    (setv next-interval None)
    (setv meanq (if meanq
                    (/ (+ quality (* meanq totaln 1.0)) (inc totaln))
                  quality))
    (cond
     [(<= quality org-drill-failure-quality)
      (incf failures)
      (setv repeats 0
            next-interval -1)]
     [(or (zero? repeats)
          (zero? last-interval))
      (setv next-interval (org-drill-simple8-first-interval failures))
      (incf repeats)
      (incf totaln)]
     [True
      (do
        (setv use-n
                (if (and
                     org-drill-adjust-intervals-for-early-and-late-repetitions-p
                     (numeric? delta-days) (pos? delta-days)
                     (pos? last-interval))
                    (+ repeats (min 1 (/ delta-days last-interval 1.0)))
                  repeats))
        (setv factor (org-drill-simple8-interval-factor
                 (org-drill-simple8-quality->ease meanq) use-n))
        (setv next-int (* last-interval factor))
        (when (and org-drill-adjust-intervals-for-early-and-late-repetitions-p
                   (numeric? delta-days) (minusp delta-days))
          ;; The item was reviewed earlier than scheduled.
          (setv factor (org-drill-early-interval-factor
                        factor next-int (abs delta-days))
                next-int (* last-interval factor)))
        (setv next-interval next-int)
        (incf repeats)
        (incf totaln))])
    (list*
     (if (and org-drill-add-random-noise-to-intervals-p
              (pos? next-interval))
         (* next-interval (org-drill-random-dispersal-factor))
       next-interval)
     repeats
     (org-drill-simple8-quality->ease meanq)
     failures
     meanq
     totaln
     )))
