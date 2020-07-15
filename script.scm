(import (gambit))
(##include "userlib/github.com/roropincho/scm2js/@/html/html.scm")
(##include "userlib/github.com/roropincho/scm2js/@/js/js.scm")

(declare
 (extended-bindings))

(define nb-col 25)

(define nb-row 25)

(define alive (vector nb-row))

(define around (vector nb-row))

(define old-around (vector nb-row))

(define game-on #f)

(define id-go-btn "go-btn")

(define id-stop-btn "stop-btn")

(define (make-interval i j lst)
  (if (> i j)
      lst
      (make-interval
       i
       (- j 1)
       (cons j lst))))

(define int-col
  (make-interval 0 (- nb-col 1) '()))

(define int-row
  (make-interval 0 (- nb-row 1) '()))

(define int-dif-around
  (make-interval -1 1 '()))

(define (make-id x y)
  (let ((txt-x (number->string x))
        (txt-y (number->string y)))
    (append-strings
     `("x"
       ,(if (null? txt-x)
            "_"
            txt-x)
       "y"
       ,(if (null? txt-y)
            "_"
            txt-y)))))

; -------------------------------------------------------------------------
; Reset the grid once the 'Stop!' button has been clicked
; -------------------------------------------------------------------------
(define (reset-lst lst size)
  (define (lst-loop i)
    (if (< i size)
        (begin
          (element.removeAttribute
           (js->foreign (##inline-host-expression "(@1@)[@2@];" lst (scm->js i)))
           "class")
          (lst-loop (+ i 1)))))
  (lst-loop 0))

(define (reset-grid)
  (let ((cell-list (foreign->js (.querySelectorAll (document-obj) "td"))))
    (let ((list-length (js->scm (##inline-host-expression "(@1@).length;" cell-list))))
      (reset-lst cell-list list-length))))

; -------------------------------------------------------------------------
; Copy the number of live neighbours calculated in the previous round
; to be used for the next round of calculations
; -------------------------------------------------------------------------
(define (transfer-row i)
  (vector-set! old-around i (vector-copy (vector-ref around i))))

(define (transfer-rows)
  (map transfer-row int-row))

; -------------------------------------------------------------------------
; Generate the indexes around each cell to avoid calculating those
; at each iteration
; -------------------------------------------------------------------------
(define (generate-gen-gen-for-direction nb-elem)
  (lambda (indice)
    (let ((ind-plus-nb (+ indice nb-elem)))
      (lambda (dif)
        (modulo (+ ind-plus-nb dif) nb-elem)))))

(define gen-gen-for-col
  (generate-gen-gen-for-direction nb-col))

(define gen-for-col
  (map gen-gen-for-col int-col))

(define (extract-cols-around col-fct)
  (vector (map col-fct int-dif-around)))

(define cols-around
  (append-vectors (map extract-cols-around gen-for-col)))

(define gen-gen-for-row
  (generate-gen-gen-for-direction nb-row))

(define gen-for-row
  (map gen-gen-for-row int-row))

(define (extract-rows-around row-fct)
  (vector (map row-fct int-dif-around)))

(define rows-around
  (append-vectors (map extract-rows-around gen-for-row)))

(define coordos-around
  (append-vectors
   (map
    (lambda (row)
      (let ((coordo-y (vector-ref rows-around row)))
        (vector
         (append-vectors
          (map
           (lambda (col)
             (let ((coordo-x (vector-ref cols-around col)))
               (vector
                (apply
                 append
                 (map
                  (lambda (y)
                    (append
                     (map
                      (lambda (x)
                        (if (or
                             (not (equal? x col))
                             (not (equal? y row)))
                            (list x y)))
                      coordo-x)))
                  coordo-y)))))
           int-col)))))
    int-row)))

; -------------------------------------------------------------------------
; Generate each cell's function to spead up each iteration's calculation
; -------------------------------------------------------------------------
(define (generate-gen-for-row row)
  (let ((rows-around (vector-ref coordos-around row)))
    (lambda (col)
      (let ((id (make-id col row))
            (cells-around (vector-ref rows-around col)))
        (lambda ()
          (let ((nb-around (vector-ref (vector-ref old-around row) col))
                (was-alive (vector-ref (vector-ref alive row) col)))
            (let ((is-alive
                   (or (equal? nb-around 3)
                       (and (equal? nb-around 2)
                            was-alive))))
              (vector-set! (vector-ref alive row) col is-alive)
              (element.setAttribute
               (document.getElementById id)
               "class"
               (if is-alive
                   "alive"
                   "dead"))
              (if (not (equal? was-alive is-alive))
                  (let ((dif (if was-alive -1 1)))
                    (map
                     (lambda (elem)
                       (if (pair? elem)
                           (let ((ind-x (car elem))
                                 (ind-y (cadr elem)))
                             (let ((old-nb (vector-ref (vector-ref around ind-y) ind-x)))
                               (vector-set!
                                (vector-ref around ind-y)
                                ind-x
                                (+ old-nb (max dif (- 0 old-nb))))))))
                     cells-around))))))))))

(define row-gen-lst
  (map generate-gen-for-row int-row))

(define (extract-cell-action-lst row-fct)
  (map row-fct int-col))

(define cell-action-lst
  (apply append (map extract-cell-action-lst row-gen-lst)))

(define (exec-cell cell-fct)
  (cell-fct))

(define (act-on-array)
  (map exec-cell cell-action-lst))

; -------------------------------------------------------------------------
; Function that does all the necessary calls at each iteration
; to make the animation progress
; -------------------------------------------------------------------------
(define (game-of-life)
  (if game-on
      (begin
        (custom-timeout)
        (transfer-rows)
        (act-on-array))
      (begin
        (reset-grid)
        (element.removeAttribute
         (document.getElementById id-go-btn)
         "disabled"))))

(##inline-host-statement "gameOfLife = g_scm2host(@1@);" game-of-life)

; -------------------------------------------------------------------------
; Set up the animation loop
; -------------------------------------------------------------------------
(define (custom-timeout)
  (##inline-host-statement "setTimeout(gameOfLife, 500);"))

; -------------------------------------------------------------------------
; To generate a 2d vector with nb-row and nb-col dimensions
; and populate it with a specific value
; -------------------------------------------------------------------------
(define (generate-gen-array-row init-value)
  (lambda (row)
    (lambda (col)
      (vector init-value))))

(define gen-0-array
  (generate-gen-array-row 0))

(define gen-false-array
  (generate-gen-array-row #f))

(define (gen-array gen-fct)
  (let ((gen-row
         (lambda (row)
           (let ((row-fct (gen-fct row)))
             (vector
              (append-vectors
               (map
                row-fct
                int-col)))))))
    (append-vectors
     (map
      gen-row
      int-row))))

; -------------------------------------------------------------------------
; Note which cells were selected by the end user
; and make initial calculations.
; -------------------------------------------------------------------------
(define (generate-gen-init-row row)
  (let ((rows-around (vector-ref coordos-around row)))
    (lambda (col)
      (let ((id (make-id col row))
            (cells-around (vector-ref rows-around col)))
        (lambda ()
          (let ((cell (document.getElementById id)))
            (let ((old-class (element.getAttribute cell "class")))
              (let ((is-alive
                     (and (not (null? old-class))
                          (not (equal? old-class (undefined-obj)))
                          (string=? "selected" old-class))))
                (vector-set! (vector-ref alive row) col is-alive)
                (element.setAttribute
                 cell
                 "class"
                 (if is-alive
                     "alive"
                     "dead"))
                (if is-alive
                    (map
                     (lambda (elem)
                       (if (pair? elem)
                           (let ((ind-x (car elem))
                                 (ind-y (cadr elem)))
                             (let ((old-nb (vector-ref (vector-ref around ind-y) ind-x)))
                               (vector-set!
                                (vector-ref around ind-y)
                                ind-x
                                (+ old-nb 1))))))
                     cells-around))))))))))

(define row-gen-init-lst
  (map generate-gen-init-row int-row))

(define (extract-cell-init-lst row-fct)
  (map row-fct int-col))

(define cell-init-lst
  (apply append (map extract-cell-init-lst row-gen-init-lst)))

(define (init-grid)
  (map exec-cell cell-init-lst))

; -------------------------------------------------------------------------
; Start the game
; -------------------------------------------------------------------------
(define (init-life)
  (begin
    (set! alive (gen-array gen-false-array))
    (set! around (gen-array gen-0-array))
    (set! old-around (gen-array gen-0-array))
    (init-grid)
    (set! game-on #t)
    (custom-timeout)))

; -------------------------------------------------------------------------
; What happens when the 'GO!' button
; -------------------------------------------------------------------------
(define (start-life)
  (element.setAttribute
   (document.getElementById id-go-btn)
   "disabled"
   "true")
  (let ((stop-btn (document.getElementById id-stop-btn)))
    (if (element.hasAttribute stop-btn "disabled")
        (element.removeAttribute stop-btn "disabled")))
  (init-life))

(##inline-host-statement "startLife = g_scm2host(@1@);" start-life)

; -------------------------------------------------------------------------
; What happens when the 'Stop!' button
; -------------------------------------------------------------------------
(define (stop-life)
  (element.setAttribute
   (document.getElementById id-stop-btn)
   "disabled"
   "true")
  (let ((go-btn (document.getElementById id-go-btn)))
    (if (element.hasAttribute go-btn "disabled")
        (element.removeAttribute go-btn "disabled")))
  (set! game-on #f))

(##inline-host-statement "stopLife = g_scm2host(@1@);" stop-life)

; -------------------------------------------------------------------------
; Functions used to construct the GUI rows
; -------------------------------------------------------------------------
(define (generate-row-string row)
  (html->string
   (<tr> id: (make-id "_" row))))

(define (extract-rows)
  (append-strings (map generate-row-string int-row)))

; -------------------------------------------------------------------------
; Functions used to construct the GUI cells
; -------------------------------------------------------------------------
(define (cell-click id)
  (if (not game-on)
      (let ((cell (document.getElementById id)))
        (let ((old-class (element.getAttribute cell "class")))
          (element.setAttribute
           cell
           "class"
           (if (or (null? old-class)
                   (equal? old-class (undefined-obj))
                   (string=? "" old-class))
               "selected"
               ""))))))

(##inline-host-statement "cellClick = g_scm2host(@1@);" cell-click)

(define (generate-gen-for-row-string row)
  (lambda (col)
    (html->string
     (<td> id: (make-id col row)
           onclick: "cellClick(this.id);"
           (<div>)))))

(define (extract-row-content row-fct)
  (append-strings
   (map row-fct int-col)))

(define (add-row-content row)
  (let ((row-fct (generate-gen-for-row-string row)))
    (element.innerHTML
     (document.getElementById (make-id "_" row))
     (extract-row-content row-fct))))

(define (construct-cells)
  (map add-row-content int-row))

(define new-style
  (let ((temp (document.createElement "style")))
    (begin
      (element.setAttribute temp "type" "text/css")
      (element.innerHTML
        temp
        (append-strings
          `("table {"
              "witdh: " ,(number->string (* nb-col 25)) "px;"
            "}")))
      temp)))

(define new-link
  (let ((temp (document.createElement "link")))
    (begin
      (element.setAttribute temp "rel" "stylesheet")
      (element.setAttribute temp "href" "style.css")
      temp)))
  
; -------------------------------------------------------------------------
; Page construction
; -------------------------------------------------------------------------
(begin
  (##inline-host-statement "document.title = 'Game of Life | Scheme to Javascript';")

  (element.appendChild
   (.querySelector (document-obj) "head")
   new-style)
  
  (element.appendChild
   (.querySelector (document-obj) "head")
   new-link)
  
  (document.write (html->string (<table>)))
  
  (element.innerHTML
   (.querySelector (document-obj) "table")
   (extract-rows))

  (construct-cells)
  
  (document.write
   (html->string
     (<button>
      id: id-go-btn
      onclick: "startLife()"
      type: "button"
      "GO!")))
  
  (document.write
   (html->string
     (<button>
      id: id-stop-btn
      disabled:
      onclick: "stopLife()"
      type: "button"
      "Stop!"))))
