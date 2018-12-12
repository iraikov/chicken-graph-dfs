;;
;; 
;; Depth-first search in a graph
;; Based on code from MLRISC
;;
;;
;; Copyright 2007-2018 Ivan Raikov.
;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; A full copy of the GPL license can be found at
;; <http://www.gnu.org/licenses/>.
;;
;;


(module graph-dfs

 (foreach
  fold
  depth
  preorder
  postorder)
		   
 (import scheme (chicken base) srfi-4 iset matchable yasos digraph
         (only srfi-1 filter-map list-tabulate)
         (rename (only srfi-1 fold) (fold list-fold))) 


 (define (graph-dfs:error x . rest)
   (let ((port (open-output-string)))
     (let loop ((objs (cons x rest)))
       (if (null? objs)
           (begin
             (newline port)
             (error 'graph-dfs (get-output-string port)))
           (begin (display (car objs) port)
                  (display " " port)
                  (loop (cdr objs)))))))
 
 (define (foreach g fn fe roots)

   (define (traverse n visited)
     (if (bit-vector-ref visited n)
         visited
         (let ((visited (bit-vector-set! visited n #t)))
           (fn n)
           (list-fold traverse-edge visited (out-edges g n)))))
   
   (define (traverse-edge e visited)
     (match e
            ((_ n _)
             (if (bit-vector-ref visited n)
                 visited
                 (let ((visited (bit-vector-set! visited n #t)))
                   (fe e)
                   (fn n)
                   (list-fold traverse-edge visited (out-edges g n)))))
            (else (graph-dfs:error 'dfs-foreach ": invalid edge " e))))
   
   (list-fold traverse  (make-bit-vector (capacity g)) roots)
   )


 (define (fold g fn fe roots x y)

   (define (traverse visited n x y)
     (if (bit-vector-ref visited n)
         (values visited x y)
         (let ((visited (bit-vector-set! visited n #t))
               (x1 (fn n x)))
           (traverse-edges visited (out-edges g n) x1 y))))
   
   (define (traverse-edges visited elst x y) =
     (match elst
            (()  (values visited x y))
            (((_ n _) . es) 
             (if (bit-vector-ref visited n)
                 (traverse-edges visited es x y)
                 (let ((visited (bit-vector-set! visited n #t))
                       (y (fe (car elst) y))
                       (x (fn n x)))
                   (let-values (((visited x y)  (traverse-edges visited (out-edges g n) x y)))
                     (traverse-edges visited es x y)))))
            (else (graph-dfs:error 'dfs-fold ": invalid edge list " elst))))
   
   (define (traverse-roots visited ns x y)
     (if (null? ns) (values x y)
         (let-values (((visited x y) (traverse visited (car ns) x y)))
           (traverse-roots visited (cdr ns) x y))))
   
   (traverse-roots (make-bit-vector (capacity g)) roots x y)
   )

 ;; compute DFS depth for each node in the graph, relative to the given
 ;; root nodes, and the number of nodes visited while traversing the
 ;; successors of each node
 (define (depth g roots)

   (define n        (capacity g))
   (define dfsnum   (make-s32vector n -1))
   (define compnum  (make-s32vector n -1))
   
   (define (traverse ns d c)
     (if (null? ns) c
         (let ((n (car ns)))
           (if (>= (s32vector-ref dfsnum n) 0)
               (traverse (cdr ns) d c)
               (begin
                 (s32vector-set! dfsnum n d)
                 (let ((c (traverse (succ g n) (+ 1 d) c)))
                   (s32vector-set! compnum n c)
                   (traverse ns d (+ 1 c))))))))
   
   (begin
     (traverse roots 0 0)
     (values dfsnum compnum)))


 (define (preorder g root)
   (define n   (capacity g))
   (define p   (make-s32vector n -1))
   
   (define (fn i n)
     (if (>= (s32vector-ref p i) 0) n
         (letrec ((fe (lambda (elst n)  
                        (match elst
                               (()  n)
                               (((_ j _) . es) (fe es (fn j n)))
                               (else (graph-dfs:error 'preorder 
                                                      ": invalid edge list " elst))))))
           (s32vector-set! p i n)
           (fe (out-edges g i) (+ 1 n)))))
   (fn root 0)
   (let ((is    (list-tabulate n values))
         (plst  (s32vector->list p)))
     (filter-map (lambda (p i) (if (>= p 0) (list i p) #f)) plst is)))
  

 (define (postorder g root)
   (define n   (capacity g))
   (define p   (make-s32vector n -2))
   
   (define (fn i n)
     (if (>= (s32vector-ref p i) -1) n
         (letrec ((fe (lambda (elst n)  
                        (match elst
                               (()  n)
                               (((_ j _) . es) (fe es (fn j n)))
                               (else (graph-dfs:error 'postorder 
                                                      ": invalid edge list " elst))))))
           (s32vector-set! p i -1)
           (let ((n (fe (out-edges g i) n)))
             (s32vector-set! p i n)
             (+ 1 n)))))
   
   (fn root 0)
   (let ((is    (list-tabulate n values))
         (plst  (s32vector->list p)))
     (filter-map (lambda (p i) (if (>= p 0) (list i p) #f)) plst is)))
 )
