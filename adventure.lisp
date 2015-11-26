#!/bin/sbcl --script

;;;; adventure.lisp by Michael Szegedy
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defvar *debug* nil
  "Whether or not to show debugging text.")

;;; Macros
(defmacro game-in ()
  "Standard input macro."
  `(split-spaces (read-line)))
(defmacro game-out (string &rest rest)
  "Standard output macro."
  `(progn (format t ,string ,@rest)
          (finish-output nil)))
(defmacro debug-out (string &rest rest)
  "Standard debug output macro. Puts a \"@ \" before every line."
  (if *debug*
      `(game-out
        ,(concatenate
          'string
          "@ "
          ;; This could probably be optimized using POSITION
          (loop for i from 0 to (1- (length string))
                if (and (>= i 2)
                        (equal (subseq string (- i 2) i) "~%"))
                  collect #\@ and
                  collect #\Space
                collect (elt string i)))
        ,@rest)))
(defmacro ini-hash-table (pairs)
  "Makes a hash table from a flat list of key-value pairs. Call with
`#.'. Credit to Antonio Bonifati from StackOverflow for this implementation."
  `(let ((hash (make-hash-table)))
     (loop for (key value) on ,pairs by #'cddr do
       (setf (gethash key hash) value))
     hash))

;;; Functions
(defun hash-table-keys (hash-table)
  "Returns a list with the keys of a hash table."
  (if (hash-table-p hash-table)
      (loop for key being the hash-keys of hash-table collect key)))
(defun split-spaces (string)
  "Splits a string into symbols using continuous groups of spaces as
separators."
  ;; A more elegant implementation would use FORMAT, I think.
  (mapcar
   (lambda (x) (intern (string-upcase x)))
   ;; Stolen from The Common Lisp Cookbook.
   (loop for i = 0 then (1+ j)
         as j = (position #\Space string :start i)
         if (not (equal (subseq string i j) "")) ; TODO: optimize
         collect (subseq string i j)
         while j)))

;;; Generics
;; First appear in ITEM
(defgeneric get-name (item
                      &key
                        all-caps
                        as-symbol
                        force-article
                        force-quantity
                        parens-quantity
                        no-article
                        no-quantity)
  (:documentation
   "Returns a string or symbol with the name of the item."))
(defgeneric use (item &key func-name args)
  (:documentation
   "Calls the given command from the USES of the item."))
(defgeneric game-describe (thing &key type)
  (:documentation
   "Prints out a description of an object."))
;; First appear in LINK
;; First appear in AREA
(defgeneric set-item (container item)
  (:documentation
   "Sets the area to include the specified item."))
(defgeneric get-item (container key)
  (:documentation
   "Gets the item with the specified name from the area."))
(defgeneric take-item (area item-name)
  (:documentation
   "Gets the item with the specified name from the area, and then removes it
from the area."))
(defgeneric set-link (area link)
  (:documentation
   "Sets the area to include the specified link."))
(defgeneric get-link (area link-name)
  (:documentation
   "Gets the link with the specified name from the area."))
(defgeneric map-command (area command-name)
  (:documentation
   "Returns the command-name that the area maps a command-name to, or, if
command-name isn't mapped, then the command-name itself."))
;; First appear in COMMAND-TABLE
(defgeneric set-command (table command)
  (:documentation
   "Sets the command-table to include the specified command."))
(defgeneric get-command (table command-name)
  (:documentation
   "Gets the command with the specified name from the command-table."))
(defgeneric call (table command-name &key func-name args)
  (:documentation
   "Calls the given command from the command-table with the given arguments."))
(defgeneric get-family (table command-name)
  (:documentation
   "Gets the other names you could call a command under."))
(defgeneric part-of-family-p (table tested-name base-name)
  (:documentation
   "Tells whether TESTED-NAME refers to the same command as BASE-NAME."))
;; First appear in WORLD
(defgeneric set-area (world area)
  (:documentation
   "Sets the world to include the specified area."))
(defgeneric get-area (world area-name)
  (:documentation
   "Gets the area with the specified name from the world."))

;;; Classes
(defclass command ()
  ((names
    :accessor names
    :initarg :names
    :documentation
    "A list of names that correspond to this command.")
   (functions
    :accessor functions
    :initarg :functions
    :documentation
    "A hash of functions that the command can perform."))
  (:documentation
   "Stores a command that the player can call. A base class for more
specialized commands. You cannot directly call a COMMAND; you need to call it
from the command table."))

(defclass item ()
  ((name
    :accessor name
    :initarg :name
    :documentation
    "The symbol that refers to the item.")
   (plural
    :accessor plural
    :initarg :plural
    :initform nil
    :documentation
    "The plural version of the name of the item.")
   (is-definite
    :accessor is-definite
    :initarg :is-definite
    :initform nil
    :documentation
    "Whether the item should be referred to using a definite article or not.")
   (is-proper
    :accessor is-proper
    :initarg :is-proper
    :initform nil
    :documentation
    "Whether the item is a proper noun or not.")
   (is-quantifiable
    :accessor is-quantifiable
    :initarg :is-quantifiable
    :initform t
    :documentation
    "Whether the item is quantifiable or not.")
   (starts-with-vowel
    :accessor starts-with-vowel
    :initarg :starts-with-vowel
    :initform nil
    :documentation
    "Whether or not the item's name starts with a vowel.")
   (is-all-caps
    :accessor is-all-caps
    :initarg :is-all-caps
    :initform nil
    :documentation
    "Whether or not the item's name should be written out exclusively with
capital letters.")
   (quantity
    :accessor quantity
    :initarg :quantity
    :initform 1
    :documentation
    "The quantity of the item.")
   (description
    :accessor description
    :initarg :description
    :documentation
    "A description of the item.")
   (uses
    :accessor uses
    :initform (make-hash-table)
    :documentation
    "A set of functions that the item can perform on the world."))
  (:documentation
   "Stores an item."))
(defmethod get-name ((item item)
                     &key
                       (all-caps (is-all-caps item))
                       (as-symbol nil)
                       (force-article nil)  ; overridden by no-article
                       (force-quantity nil) ; overridden by no-quantity
                       (parens-quantity nil)
                       (no-article nil)
                       (no-quantity nil))
  ;; This is probably the worst thing I have ever written in Lisp
  (let ((raw-name
         (concatenate
          'string
          ;; The article
          (if (and (or (and (or (is-definite item)      ; oh god my eyes
                                (= (quantity item) 1))
                            (is-quantifiable item)
                            (not (is-proper item)))
                       force-article)
                   (not no-article))
              (concatenate
               'string
               (if (is-definite item)
                   "the"
                   (if (starts-with-vowel item)
                       "an"
                       "a"))
               " "))
          ;; The quantity
          (if (and (or (and (not (= (quantity item) 1)) ; fuck NLP
                            (is-quantifiable item))
                       force-quantity)
                   (not no-article))
              (concatenate
               'string
               (format nil "~r" (quantity item))
               " "
               (if parens-quantity
                   (concatenate
                    'string
                    (format nil "(~a)" (quantity item))
                    " "))))
          ;; The name itself
          (if (= (quantity item) 1)
              (format nil "~a" (name item))
              (if (plural item)
                  (format nil "~a" (plural item))
                  (format nil "~aS" (name item)))))))
    (let ((caps-name (if all-caps
                         (string-upcase raw-name)
                         raw-name)))
      (if as-symbol
          (intern caps-name)
          caps-name))))
(defmethod use ((item item)
                &key
                  (func-name 'default)
                  args)
  (let ((func (gethash func-name (uses item))))
    (if func
        (progn
          (debug-out
           "using item ~s with func-name ~s~%"
           (name item)
           func-name)
          (apply func args))
        (progn
          (debug-out
           "no use found for item ~s under func-name ~s~%"
           (name item)
           func-name)
          (game-out
           "You cannot use ~a that way.~%"
           (get-name item))))))
(defmethod game-describe ((item item) &key (type 'description))
  (cond
    ((eq type 'description)
     (game-out (description item))
     (terpri))))

(defclass link ()
  ((name
    :accessor names
    :initarg :names
    :documentation
    "The symbols that refer to the link.")
   (destination
    :accessor destination
    :initarg :destination
    :documentation
    "The name of the area that the link points to.")
   (description
    :accessor description
    :initarg :description
    :documentation
    "A description of the link.")
   (action
    :accessor action
    :initarg :action
    :documentation
    "The text that appears when the link is gone."))
  (:documentation
   "Stores a link from one area to another."))
(defmethod game-describe ((link link) &key (type 'description))
  (cond
    ((eq type 'description)
     (game-out (description link))
     (terpri))
    ((eq type 'action)
     (game-out (action link))
     (terpri))))

(defclass area ()
  ((name
    :accessor name
    :initarg :name
    :documentation
    "The symbol that refers to the area.")
   (description
    :accessor description
    :initarg :description
    :documentation
    "A description of the area.")
   (command-map
    :accessor command-map
    :initform (make-hash-table)
    :documentation
    "A hashmap that maps command names to other command names.")
   (links
    :accessor links
    :initform (make-hash-table)
    :documentation
    "A hash that stores the links in the area.")
   (link-names
    :accessor link-names
    :initform nil
    :documentation
    "A list that stores all of the NAMES lists of the links in the area.")
   (items
    :accessor items
    :initform (make-hash-table)
    :documentation
    "A list of items inside the area."))
  (:documentation
   "Stores an area."))
(defmethod set-item ((container area) (item item))
  (setf (gethash (name item) (items container)) item))
(defmethod get-item ((container area) key)
  (gethash key (items container)))
(defmethod take-item ((area area) item-name)
  (let ((item (get-item area item-name)))
    (if item
        (progn
          (remhash (name item) (items area))
          item))))
(defmethod set-link ((area area) (link link))
  (setf (link-names area) (adjoin (names link) (link-names area)))
  (loop for name in (names link) do
    (setf (gethash name (links area)) link)))
(defmethod get-link ((area area) link-name)
  (gethash link-name (links area)))
(defmethod map-command ((area area) command-name)
  (let ((new-name (gethash command-name (command-map area))))
    (if new-name
        (progn
          (debug-out
           "area ~s mapped command ~s to ~s~%"
           (name area)
           command-name
           new-name)
          new-name)
        (progn
          (debug-out
           "area ~s found no map for command ~s~%"
           (name area)
           command-name)
          command-name))))
(defmethod game-describe ((area area) &key (type 'whole))
  (game-out (description area))
  (terpri)
  (let ((link-names (mapcar #'car (link-names area)))
        (item-names (hash-table-keys (items area))))
    ;; Describe items
    (cond
      ((= (length item-names) 0))
      ((= (length item-names) 1)
       (game-out
        "This area contains ~a.~%"
        (get-name (get-item area (car item-names)))))
      ((= (length item-names) 2)
       (game-out
        "This area contains ~a and ~a.~%"
        (get-name (get-item area (car item-names)))
        (get-name (get-item area (cadr item-names)))))
      (t
       (game-out "This area contains ")
       (loop for item-name in (cdr item-names) do
         (game-out
          "~a, "
          (get-name (get-item area item-name))))
       (game-out
        "and ~a.~%"
        (get-name (get-item area (car item-names))))))
    ;; Describe paths
    (cond
      ((= (length link-names) 0))
      ((= (length link-names) 1)
       (game-out "The only obvious path is ~a." (car link-names))
       (terpri))
      ((= (length link-names) 2)
       (game-out
        "Obvious paths are ~a and ~a."
        (car link-names)
        (cadr link-names))
       (terpri))
      (t
       (game-out "Obvious paths are ")
       (loop for link-name in (cdr link-names) do
         (game-out "~a, " link-name))
       (game-out "and ~a." (car (links area)))
       (terpri)))))

(defclass inventory ()
  ((items
    :accessor items
    :initform (make-hash-table)
    :documentation
    "Stores the items in a hash table.")
   (item-names
    :accessor item-names
    :initform nil
    :documentation
    "A list containing item names in which each item only appears once.")
   (fetch-modus
    :accessor fetch-modus
    :initform 'hashmap
    :documentation
    "How the items are to be retrieved."))
  (:documentation
   "Stores the items the user has."))
(defmethod game-describe ((thing inventory) &key (type 'list))
  (cond
    ((eq type 'list)
     t)))
(defmethod set-item ((container inventory) item)
  (let ((modus (fetch-modus container)))
    (cond
      ((eq modus 'hashmap)
       (setf (gethash (name item) (items container)) item)))))
(defmethod get-item ((container inventory) key)
  (let ((modus (fetch-modus container)))
    (cond
      ((eq modus 'hashmap)
       (gethash key (items container))))))

(defclass command-table ()
  ((commands
    :accessor commands
    :initform (make-hash-table)
    :initarg :commands
    :documentation
    "Stores the commands in a hash table."))
  (:documentation
   "Stores all the commands user can run."))
(defmethod set-command ((table command-table) (command command))
  (loop for name in (names command) do
    (setf (gethash name (commands table)) command)))
(defmethod get-command ((table command-table) command-name)
  (gethash command-name (commands table)))
(defmethod call ((table command-table)
                 command-name
                 &key
                   (func-name 'default)
                   args)
  (let ((command (get-command table command-name)))
    (if command
        (progn
          (debug-out
           "calling command ~s~%  as ~s~%  with func-name ~s~%"
           (names command)
           command-name
           func-name)
          (apply (gethash func-name (functions command)) args))
        (progn
          (debug-out "no command under name ~s~%" command-name)
          (game-out "You cannot ~a.~%" command-name)))))
(defmethod get-family ((table command-table) command-name)
  (let ((command (get-command table command-name)))
    (if command
        (names command))))
(defmethod part-of-family-p ((table command-table) tested-name base-name)
  (member base-name (get-family table tested-name)))

(defclass world ()
  ((areas
    :accessor areas
    :initform (make-hash-table)
    :documentation
    "A hash that stores the areas inside the world."))
  (:documentation
   "Stores the world."))
(defmethod set-area ((world world) (area area))
  (setf (gethash (name area) (areas world)) area))
(defmethod get-area ((world world) area-name)
  (gethash area-name (areas world)))

;;; Global variables
(defvar *name* nil)
(defvar *sylladex* (make-instance 'inventory)
  "Stores the player's items.")
(defvar *commands* (make-instance 'command-table)
  "The commands available to the player. Multiple keys may correspond to the
same command.")
(defvar *world* (make-instance 'world)
  "Contains the areas the player can visit.")
(defvar *current-area* nil
  "The current loaded area.")

;;; Functions on the global variables
(defun update-areas (new-location)
  "Sets the current area to the area with the name new-location. Additionally,
it stores the area you left back in *world*."
  (if *current-area*
      (set-area *world* *current-area*))
  (if *current-area*
      (debug-out "changing area from ~s~%" (name *current-area*))
      (debug-out "changing area from scratch~%"))
  (setf *current-area* (get-area *world* new-location))
  (debug-out "entering area ~s~%" (name *current-area*)))

;;; Initialize global variables
;; Initialize *name*
;; Initialize *sylladex*
;; Initialize *world*
(defun make-room-chain (max &optional (count 1) previous-name)
  "Adds a chain of rooms to *world*."
  (let ((area (make-instance 'area)))
    (if (> count 1)
        (setf (name area) (gensym))
        (setf (name area) 'start-room))
    (setf (description area)
          (format nil "This is the ~:r room." count))
    (set-item area
      (make-instance 'item
        :name 'oof
        :quantity 2
        :starts-with-vowel t
        :description
        "This is an oof."))
    (if (> count 1)
        (progn
          (set-link area
            (make-instance 'link
             :names '(back-door door-1)
             :destination previous-name
             :description
             "This is a door."
             :action
             "You go through the door."))
          (let ((previous-area (get-area *world* previous-name))
                (link
                 (make-instance 'link
                  :names '(front-door door-2)
                  :destination (name area)
                  :description
                  "This is a door."
                  :action
                  "You go through the door.")))
            (set-link previous-area link)
            (set-area *world* previous-area))))
    (set-area *world* area)
    (if (< count max)
        (make-room-chain max (+ count 1) (name area)))))
(game-out "Making world...")
(make-room-chain 10000)
(game-out " done.~%")
;; Initialize *commands*
(set-command *commands*
  (make-instance 'command
   :names '(look examine inspect view ls)
   :functions
   #.(ini-hash-table
      (list
       'default
       (lambda (command-name &rest args)
         (let ((matches
                (remove-if
                 (lambda (x) (not x))
                 (list (get-item *current-area* (car args))
                       (get-link *current-area* (car args))))))
              (if (car matches)
                  (game-describe (car matches))
                  (game-out "You cannot ~a that.~%" command-name))))
       'area
       (lambda (command-name &rest args)
         (game-describe *current-area*))))))
(set-command *commands*
  (make-instance 'command
   :names '(go cd)
   :functions
   #.(ini-hash-table
      (list
       'default
       (lambda (command-name &rest args)
         (let ((link (get-link *current-area* (car args))))
              (if link
                  (progn
                    (game-describe link :type 'action)
                    (update-areas (destination link)))
                  (game-out "You cannot ~a that way.~%" command-name))))))))
(set-command *commands*
  (make-instance 'command
   :names '(help)
   :functions
   #.(ini-hash-table
      (list
       'default
       (lambda (command-name &rest args)
         (game-out "Type commands to do things. Try INSPECTing things, or ~
                    GOing places.~%"))))))
(set-command *commands*
  (make-instance 'command
   :names '(captchalogue captcha)
   :functions
   #.(ini-hash-table
      (list
       'default
       (lambda (command-name &rest args)
         (let* ((item-name (car args))
                (item (take-item *current-area* item-name)))
           (if item
               (set-item *sylladex* item)
               (game-out
                "This area does not contain that.~%"))))))))
(set-command *commands*
  (make-instance 'command
   :names '(take)
   :functions
   #.(ini-hash-table
      (list
       'default
       (lambda (command-name &rest args)
         (call *commands* 'captchalogue
               :args (cons command-name args)))
       'path
       (lambda (command-name &rest args)
         (call *commands* 'go
               :args (cons command-name args)))))))
;; Initialize *current-area*
(update-areas 'start-room)

;;; Start playing the game.
(loop
  ;; Technically, this part could be optimized if I saved the prefix in a
  ;; variable and didn't evaluate the conditional.
  (if (not *name*)
      (game-out "> ")
      (game-out "> ~:(~a~): " (car *name*))) ; capitalizes first letter of name
  ;; I think this works because LET* doesn't execute its body until all the
  ;; arguments have been bound. If you try to bind RESPONSE using DEFVAR or
  ;; DEFPARAMETER instead, then it'll complain about unbound variables.
  (let* ((response (game-in))
         (command-name (map-command *current-area* (car response)))
         (args (cdr response)))
    (debug-out "input was ~s~%" response)
    (cond
      ((not response))
      ((eq command-name 'quit)
       (return))
      ((and (part-of-family-p *commands* command-name 'look)
            (member args '(nil (room) (area))))
       (call *commands* command-name
             :func-name 'area
             :args (cons command-name args)))
      ((and (part-of-family-p *commands* command-name 'take)
            (get-link *current-area* (car args)))
       (call *commands* command-name
             :func-name 'path
             :args (cons command-name args)))
      (t
       (call *commands* command-name
             :args (cons command-name args))))))
