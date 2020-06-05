(##include "~/gambit/lib/gambit#.scm")
(##include "~/gambit/examples/web-server/html.scm")
(##include "js.scm")

(declare
 (extended-bindings))

(define nb-col 25)

(define nb-row 25)

(define alive (make-vector nb-row))

(define around (make-vector nb-row))

(define old-around (make-vector nb-row))

(define game-on #f)

(define id-go-btn "go-btn")

(define id-stop-btn "stop-btn")

(define (make-id x y)
  (let ((txt-x (number->string x))
        (txt-y (number->string y)))
    (string-append "x"
                   (if (null? txt-x)
                       "_"
                       txt-x)
                   "y"
                   (if (null? txt-y)
                       "_"
                       txt-y))))

(define (game-of-life)
    (define (transfer-row i)
      (if (< i nb-row)
          (begin
            (vector-set! old-around i (vector-copy (vector-ref around i)))
            (transfer-row (+ i 1)))))

    (define (cols-around x y dif-col dif-row was-alive)
      (if (<= dif-col 1)
          (begin
            (if (not (and (equal? dif-col 0)
                          (equal? dif-row 0)))
                (let ((ind-x (modulo (+ x dif-col nb-col) nb-col))
                      (ind-y (modulo (+ y dif-row nb-row) nb-row)))
                  (let ((old-nb (vector-ref (vector-ref around ind-y) ind-x)))
                    (vector-set! (vector-ref around ind-y) ind-x (+ old-nb (if was-alive
                                                                            (max -1
                                                                                 (- 0 old-nb))
                                                                            1))))))
            (cols-around x y (+ dif-col 1) dif-row was-alive))))

    (define (rows-around x y dif-row was-alive)
      (if (<= dif-row 1)
          (begin
            (cols-around x y -1 dif-row was-alive)
            (rows-around x y (+ dif-row 1) was-alive))))

    (define (iterate-on-row i no-row)
      (if (< i nb-col)
          (begin
            (let ((nb-around (vector-ref (vector-ref old-around no-row) i))
                  (was-alive (vector-ref (vector-ref alive no-row) i)))
              (let ((is-alive (or (equal? nb-around
                                         3)
                                 (and (equal? nb-around
                                              2)
                                      was-alive))))
                (vector-set! (vector-ref alive no-row) i is-alive)
                (set-attribute (get-element-by-id (make-id i no-row))
                              "class"
                              (if is-alive
                                  "alive"
                                  "dead"))
                (if (not (equal? was-alive is-alive))
                    (rows-around i no-row -1 was-alive))))
            (iterate-on-row (+ i 1) no-row))))

    (define (iterate-on-array i)
      (if (< i nb-row)
          (begin
            (iterate-on-row 0 i)
            (iterate-on-array (+ i 1)))))

    (define (reset-grid)
      (let ((cell-list (query-selector-all "td")))
        (let ((list-length (js->scm (##inline-host-expression "(@1@).length;" cell-list))))
          ((lambda (lst size)
             (define (lst-loop i)
               (if (< i size)
                   (begin
                     (remove-attribute (##inline-host-expression "(@1@)[@2@];" lst (scm->js i))
                                      "class")
                     (lst-loop (+ i 1)))))
             (lst-loop 0))
           cell-list
           list-length))))

    (if game-on
        (begin
          (custom-timeout)
          (transfer-row 0)
          (iterate-on-array 0))
        (begin
          (reset-grid)
          (remove-attribute (get-element-by-id id-go-btn)
                           "disabled"))))

(##inline-host-statement "gameOfLife = g_scm2host(@1@);" game-of-life)

(define (custom-timeout)
  (##inline-host-statement "setTimeout(gameOfLife, 500);"))

(define (cell-click id)
    (if (not game-on)
        (let ((cell (get-element-by-id id)))
          (let ((old-class (get-attribute cell "class")))
            (set-attribute cell
                          "class"
                          (if (or (not (usable-obj? old-class))
                                  (string=? ""
                                            old-class))
                              "selected"
                              ""))))))

(##inline-host-statement "cellClick = g_scm2host(@1@);" cell-click)

(define (construct-cells i base no-row)
  (if (< i nb-col)
      (let ((id (make-id i no-row)))
        (construct-cells (+ i 1)
                        (string-append base
                                       "<td id='"
                                       id
                                       "' onclick='cellClick(this.id);'><div></div></td>")
                        no-row))
      base))

(define (construct-rows i base)
  (if (< i nb-row)
      (construct-rows (+ i 1)
                     (string-append base
                                    "<tr>"
                                    (construct-cells 0 "" i)
                                    "</tr>"))
      base))

(define (init-life)
    (define (fill-row i array init-value)
      (if (< i nb-row)
          (fill-row (+ i 1)
                   (vector-append array
                                  (vector init-value))
                   init-value)
          array))

    (define (fill-array i array init-value)
      (if (< i nb-row)
          (fill-array (+ i 1)
                     (vector-append array
                                    (vector (fill-row 0 (vector) init-value)))
                     init-value)
          array))

    (define (cols-around x y dif-col dif-row)
      (if (<= dif-col 1)
          (begin
            (if (not (and (equal? dif-col 0)
                          (equal? dif-row 0)))
                (let ((ind-x (modulo (+ x dif-col nb-col) nb-col))
                      (ind-y (modulo (+ y dif-row nb-row) nb-row)))
                  (vector-set! (vector-ref around ind-y) ind-x
                               (+ 1 (vector-ref (vector-ref around ind-y) ind-x)))
                  (vector-set! (vector-ref old-around ind-y) ind-x
                               (+ 1 (vector-ref (vector-ref old-around ind-y) ind-x)))))
            (cols-around x y (+ dif-col 1) dif-row))))

    (define (rows-around x y dif-row)
      (if (<= dif-row 1)
          (begin
            (cols-around x y -1 dif-row)
            (rows-around x y (+ dif-row 1)))))

    (define (iterate-on-row i no-row)
      (if (< i nb-col)
          (begin
            (let ((cell-temp (get-element-by-id (make-id i no-row))))
              (let ((old-class (get-attribute cell-temp "class")))
                (let ((is-alive (and (usable-obj? old-class)
                                    (string=? "selected"
                                              old-class))))
                  (begin
                    (set-attribute cell-temp "class" (if is-alive
                                                       "alive"
                                                       "dead"))
                    (vector-set! (vector-ref alive no-row) i is-alive)
                    (if is-alive
                        (rows-around i no-row -1))))))
            (iterate-on-row (+ i 1) no-row))))

    (define (iterate-on-array i)
      (if (< i nb-row)
          (begin
            (iterate-on-row 0 i)
            (iterate-on-array (+ i 1)))))

    (begin
      (set! alive (fill-array 0 (vector) #f))
      (set! around (fill-array 0 (vector) 0))
      (set! old-around (fill-array 0 (vector) 0))
      (iterate-on-array 0)
      (set! game-on #t)
      (custom-timeout)))

(define (start-life)
    (set-attribute (get-element-by-id id-go-btn)
                  "disabled"
                  "true")
    (let ((stop-btn (get-element-by-id id-stop-btn)))
      (if (has-attribute stop-btn "disabled")
          (remove-attribute stop-btn "disabled")))
    (init-life))

(##inline-host-statement "startLife = g_scm2host(@1@);" start-life)

(define (stop-life)
    (set-attribute (get-element-by-id id-stop-btn)
                  "disabled"
                  "true")
    (let ((go-btn (get-element-by-id id-go-btn)))
      (if (has-attribute go-btn "disabled")
          (remove-attribute go-btn "disabled")))
    (set! game-on #f))

(##inline-host-statement "stopLife = g_scm2host(@1@);" stop-life)

(begin
  (##inline-host-statement "document.title = 'Game of Life | Scheme to Javascript';")
  (append-html (query-selector "head")
              (<style> type: "text/css"
                       (string-append "* {"
                                      "border: 0;"
                                      "border-collapse: collapse;"
                                      "margin: 0;"
                                      "padding: 0;"
                                      "transition: 0.25s background-color, 0.25s color, 0.25s opacity;"
                                      "}"
                                      "body {"
                                      "text-align: center;"
                                      "}"
                                      "table {"
                                        ; "border-collapse: separate;"
                                        ; "border-spacing: 5px;"
                                      "margin: 10px auto;"
                                      "padding: 10px;"
                                      "width: " (number->string (* nb-col 25)) "px;"
                                      "}"
                                      "tr {"
                                      "height: 25px;"
                                      "}"
                                      "td {"
                                      "position: relative;"
                                      "width: 25px;"
                                      "}"
                                      "td div {"
                                      "background-color: lightGrey;"
                                      "border-radius: 50%;"
                                      "bottom: 2.5px;"
                                      "left: 2.5px;"
                                      "position: absolute;"
                                      "right: 2.5px;"
                                      "top: 2.5px;"
                                      "}"
                                      "td:not(.dead):not(.alive):hover {"
                                      "cursor: pointer;"
                                      "opacity: 0.5;"
                                      "}"
                                      "td.selected div {"
                                      "background-color: salmon;"
                                      "}"
                                      "td.dead {"
                                      "opacity: 0.5;"
                                      "}"
                                      "td.alive div {"
                                      "background-color: darkRed;"
                                      "}"
                                      "button, button:disabled:hover {"
                                      "background-color: lightGrey;"
                                      "color: darkRed;"
                                      "}"
                                      "button {"
                                      "border-radius: 12.5px;"
                                      "margin: 0 0 0 15px;"
                                      "padding: 10px 15px;"
                                      "}"
                                      "button + button {"
                                      "margin-left: 5px;"
                                      "}"
                                      "button:disabled {"
                                      "color: darkRed;"
                                      "opacity: 0.25;"
                                      "}"
                                      "button:hover {"
                                      "background-color: darkRed;"
                                      "color: lightGrey;"
                                      "}"
                                      "button:not(:disabled):hover {"
                                      "cursor: pointer;"
                                      "}")))
  (document.write (<table>))
  (set-inner-html (query-selector "table")
                (construct-rows 0 ""))
  (document.write (<button> id: id-go-btn
                            onclick: "startLife()"
                            type: "button"
                            "GO!"))
  (document.write (<button> id: id-stop-btn
                            disabled:
                            onclick: "stopLife()"
                            type: "button"
                            "Stop!")))
