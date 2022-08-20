#lang racket

(car ''abracadabra)

;From footnote 34: the quotation mark is just a single-character
;abbreviation for wrapping the next complete expression with
;quote to form (quote ⟨expression⟩). Therefore, ' == quote

(car (quote (quote abracadabra)))
(car '(quote abracadabra))