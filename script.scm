(##include "~/gambit/lib/gambit#.scm")
(##include "~/gambit/examples/web-server/html.scm")
(##include "js.scm")

(declare
 (extended-bindings))

(define nbCol 25)
(define nbRow 25)

(define alive (make-vector nbRow))
(define around (make-vector nbRow))
(define oldAround (make-vector nbRow))

(define gameOn #f)
(define idGoBtn "go-btn")
(define idStopBtn "stop-btn")

(define (makeId x y)
  (let ((txtX (number->string x))
        (txtY (number->string y)))
    (string-append "x"
                   (if (null? txtX)
                       "_"
                       txtX)
                   "y"
                   (if (null? txtY)
                       "_"
                       txtY))))

(define gameOfLife
  (lambda ()
    (define (transferRow i)
      (if (< i nbRow)
          (begin
            (vector-set! oldAround i (vector-copy (vector-ref around i)))
            (transferRow (+ i 1)))))
    (define (colsAround x y difCol difRow wasAlive)
      (if (<= difCol 1)
          (begin
            (if (not (and (equal? difCol 0)
                          (equal? difRow 0)))
                (let ((indX (modulo (+ x difCol nbCol) nbCol))
                      (indY (modulo (+ y difRow nbRow) nbRow)))
                  (let ((oldNb (vector-ref (vector-ref around indY) indX)))
                    (vector-set! (vector-ref around indY) indX (+ oldNb (if wasAlive
                                                                            (max -1
                                                                                 (- 0 oldNb))
                                                                            1))))))
            (colsAround x y (+ difCol 1) difRow wasAlive))))
    (define (rowsAround x y difRow wasAlive)
      (if (<= difRow 1)
          (begin
            (colsAround x y -1 difRow wasAlive)
            (rowsAround x y (+ difRow 1) wasAlive))))
    (define (iterateOnRow i noROw)
      (if (< i nbCol)
          (begin
            (let ((nbAround (vector-ref (vector-ref oldAround noROw) i))
                  (wasAlive (vector-ref (vector-ref alive noROw) i)))
              (let ((isAlive (or (equal? nbAround
                                         3)
                                 (and (equal? nbAround
                                              2)
                                      wasAlive))))
                (vector-set! (vector-ref alive noROw) i isAlive)
                (setAttribute (getElementById (makeId i noROw))
                              "class"
                              (if isAlive
                                  "alive"
                                  "dead"))
                (if (not (equal? wasAlive isAlive))
                    (rowsAround i noROw -1 wasAlive))))
            (iterateOnRow (+ i 1) noROw))))
    (define (iterateOnArray i)
      (if (< i nbRow)
          (begin
            (iterateOnRow 0 i)
            (iterateOnArray (+ i 1)))))
    (define (resetGrid)
      (let ((cellList (querySelectorAll "td")))
        (let ((listLength (jsObj->scmObj (##inline-host-expression "(@1@).length;" cellList))))
          ((lambda (lst size)
             (define (lstLoop i)
               (if (< i size)
                   (begin
                     (removeAttribute (##inline-host-expression "(@1@)[@2@];" lst (scmObj->jsObj i))
                                      "class")
                     (lstLoop (+ i 1)))))
             (lstLoop 0))
           cellList
           listLength))))
    (if gameOn
        (begin
          (customTimeout)
          (transferRow 0)
          (iterateOnArray 0))
        (begin
          (resetGrid)
          (removeAttribute (getElementById idGoBtn)
                           "disabled")))))

(##inline-host-statement "gameOfLife = g_scm2host(@1@);" gameOfLife)

(define (customTimeout)
  (##inline-host-statement "setTimeout(gameOfLife, 500);"))

(define cellClick
  (lambda (id)
    (if (not gameOn)
        (let ((cell (getElementById id)))
          (let ((oldClass (getAttribute cell "class")))
            (setAttribute cell
                          "class"
                          (if (or (not (isUsableObject oldClass))
                                  (string=? ""
                                            oldClass))
                              "selected"
                              "")))))))

(##inline-host-statement "cellClick = g_scm2host(@1@);" cellClick)

(define (constructCells i base noROw)
  (if (< i nbCol)
      (let ((id (makeId i noROw)))
        (constructCells (+ i 1)
                        (string-append base
                                       "<td id='"
                                       id
                                       "' onclick='cellClick(this.id);'><div></div></td>")
                        noROw))
      base))

(define (constructRows i base)
  (if (< i nbRow)
      (constructRows (+ i 1)
                     (string-append base
                                    "<tr>"
                                    (constructCells 0 "" i)
                                    "</tr>"))
      base))

(define initLife
  (lambda ()
    (define (fillRow i array initValue)
      (if (< i nbRow)
          (fillRow (+ i 1)
                   (vector-append array
                                  (vector initValue))
                   initValue)
          array))
    (define (fillArray i array initValue)
      (if (< i nbRow)
          (fillArray (+ i 1)
                     (vector-append array
                                    (vector (fillRow 0 (vector) initValue)))
                     initValue)
          array))
    (define (colsAround x y difCol difRow)
      (if (<= difCol 1)
          (begin
            (if (not (and (equal? difCol 0)
                          (equal? difRow 0)))
                (let ((indX (modulo (+ x difCol nbCol) nbCol))
                      (indY (modulo (+ y difRow nbRow) nbRow)))
                  (vector-set! (vector-ref around indY) indX
                               (+ 1 (vector-ref (vector-ref around indY) indX)))
                  (vector-set! (vector-ref oldAround indY) indX
                               (+ 1 (vector-ref (vector-ref oldAround indY) indX)))))
            (colsAround x y (+ difCol 1) difRow))))
    (define (rowsAround x y difRow)
      (if (<= difRow 1)
          (begin
            (colsAround x y -1 difRow)
            (rowsAround x y (+ difRow 1)))))
    (define (iterateOnRow i noROw)
      (if (< i nbCol)
          (begin
            (let ((cellTemp (getElementById (makeId i noROw))))
              (let ((oldClass (getAttribute cellTemp "class")))
                (let ((isAlive (and (isUsableObject oldClass)
                                    (string=? "selected"
                                              oldClass))))
                  (begin
                    (setAttribute cellTemp "class" (if isAlive
                                                       "alive"
                                                       "dead"))
                    (vector-set! (vector-ref alive noROw) i isAlive)
                    (if isAlive
                        (rowsAround i noROw -1))))))
            (iterateOnRow (+ i 1) noROw))))
    (define (iterateOnArray i)
      (if (< i nbRow)
          (begin
            (iterateOnRow 0 i)
            (iterateOnArray (+ i 1)))))
    (begin
      (set! alive (fillArray 0 (vector) #f))
      (set! around (fillArray 0 (vector) 0))
      (set! oldAround (fillArray 0 (vector) 0))
      (iterateOnArray 0)
      (set! gameOn #t)
      (customTimeout))))

(define startLife
  (lambda ()
    (setAttribute (getElementById idGoBtn)
                  "disabled"
                  "true")
    (let ((stopBtn (getElementById idStopBtn)))
      (if (hasAttribute stopBtn "disabled")
          (removeAttribute stopBtn "disabled")))
    (initLife)))

(##inline-host-statement "startLife = g_scm2host(@1@);" startLife)

(define stopLife
  (lambda ()
    (setAttribute (getElementById idStopBtn)
                  "disabled"
                  "true")
    (let ((goBtn (getElementById idGoBtn)))
      (if (hasAttribute goBtn "disabled")
          (removeAttribute goBtn "disabled")))
    (set! gameOn #f)))

(##inline-host-statement "stopLife = g_scm2host(@1@);" stopLife)

(begin
  (##inline-host-statement "document.title = 'Game of Life | Scheme to Javascript';")
  (appendHTML (querySelector "head")
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
                                      "width: " (number->string (* nbCol 25)) "px;"
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
  (setInnerHTML (querySelector "table")
                (constructRows 0 ""))
  (document.write (<button> id: idGoBtn
                            onclick: "startLife()"
                            type: "button"
                            "GO!"))
  (document.write (<button> id: idStopBtn
                            disabled:
                            onclick: "stopLife()"
                            type: "button"
                            "Stop!")))
