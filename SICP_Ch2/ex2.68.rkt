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

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define sample-symbol (decode sample-message sample-tree))
sample-symbol

(encode sample-symbol sample-tree)