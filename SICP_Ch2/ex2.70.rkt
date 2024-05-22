#lang sicp

; Huffman tree
; Representing Huffman trees
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x)
  (cadr x))
(define (weight-leaf x)
  (caddr x))
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch-huffman tree)
  (car tree))
(define (right-branch-huffman tree)
  (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

; The decoding procedure
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree)) ; go back to tree, so we restart from root
              (decode-1 (cdr bits) next-branch))))) ; continue with the next branch
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch-huffman branch))
        ((= bit 1) (right-branch-huffman branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set-huffman x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set-huffman x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
         (adjoin-set-huffman (make-leaf (car pair)    ; symbol
                                        (cadr pair))  ; frequecy
                             (make-leaf-set (cdr pairs))))))

; (make-leaf-set '((A 4) (B 2) (C 1) (D 1)))

;2.3.3 Example: Representing Sets
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      (if (eq? (symbol-leaf tree) symbol)
          '()
          (error "There is no such symbol in the huffman-tree" symbol))
      (let ((left (left-branch-huffman tree)))
        (if (element-of-set? symbol (symbols left))
            (cons 0 (encode-symbol symbol left))
            (cons 1 (encode-symbol symbol (right-branch-huffman tree)))))))

; from 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

; (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; (define sample-symbol (decode sample-message sample-tree))

; (encode sample-symbol sample-tree)

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (if (null? (cdr set))
      (car set)
      (let ((left (car set))
            (right (cadr set))
            (remain (cddr set)))
        (successive-merge
         (adjoin-set-huffman (make-code-tree left right)
                             remain)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; test
; (define huffman-tree (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))
; (define sample-symbols '(A D A B B C A))
; (define message (encode sample-symbols huffman-tree))

; huffman-tree                  ; '((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)
; sample-symbols                ; '(A D A B B C A)
; message                       ; '(0 1 1 0 0 1 0 1 0 1 1 1 0)
; (decode message huffman-tree) ; '(A D A B B C A)

(define (encode-messages messages tree)
  (if (null? messages)
      '()
      (append (encode (car messages) tree)
              (encode-messages (cdr messages) tree))))

(define tree (generate-huffman-tree '((A    2) 
                                      (NA   16) 
                                      (BOOM 1)
                                      (SHA  3) 
                                      (GET  2) 
                                      (YIP  9) 
                                      (JOB  2)
                                      (WAH  1))))

(define messages '((GET A JOB)
                   (SHA NA NA NA NA NA NA NA NA)
                   (GET A JOB)
                   (SHA NA NA NA NA NA NA NA NA)
                   (WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP)
                   (SHA BOOM)))

(encode-messages messages tree)
(decode (encode-messages messages tree) tree)
(length (encode-messages messages tree))      ;; 84

;;; There are 8 symbols. If we used fixed-length code, it would require 3 binaries.
;;; There are 36 words in the lyrics, which would require 36 * 3 = 108.
;;; Therefore, we saved 108 - 84 = 24 bits.