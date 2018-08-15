#lang typed/racket

(require typed/racket/date)

(require "model.rkt")
(require "state.rkt")
(require "strategy.rkt")
(require "planning.rkt")
(require "trace.rkt")
(require "runtime.rkt")

(define num-seeds 39)

(define num-assembly-problems 186)
(define num-disassembly-problems 186)
(define num-reassembly-problems 115)

(define-type ProblemType (Union 'a 'd 'r))
(struct problem ([type : ProblemType]
                 [number : Natural]))

(: problem->string (-> problem String))
(define (problem->string p)
  (string-append (string-upcase (symbol->string (problem-type p)))
                 (~a (problem-number p)
                     #:width 3 #:align 'right #:pad-string "0")))

(: string->problem (-> String problem))
(define (string->problem s)
  (problem (cast (string->symbol (string-downcase (substring s 0 1)))
                 ProblemType)
           (cast (string->number (substring s 1))
                 Positive-Integer)))

(define sample-problems
  (map string->problem
       '("A001" "D001" "R001" "A023" "A050" "A067"
         "A115" "A131" "A173")))

(define all-problems
  (append (for/list : (Listof problem) ((i num-assembly-problems))
            (problem 'a (+ i 1)))
          (for/list : (Listof problem) ((i num-disassembly-problems))
            (problem 'd (+ i 1)))
          (for/list : (Listof problem) ((i num-reassembly-problems))
            (problem 'r (+ i 1)))))

(define run-only? : (Parameterof Boolean)
  (make-parameter #f))
(define selected-directory : (Parameterof (Option String))
  (make-parameter #f)) ; default depends on run-only?
(define selected-strategy : (Parameterof String)
  (make-parameter "slice-and-dice"))
(define all-problems? : (Parameterof Boolean)
  (make-parameter #f))
(define selected-problems : (Parameterof (Option String))
  (make-parameter #f))
(define write-trace-files? : (Parameterof Boolean)
  (make-parameter #f))

(define (main)
  ;; Parse options given by the user at runtime
  (command-line
    #:program "racket main.rkt"
    #:once-any
    [("-s" "--strategy") #{strategy : String}
                         ("Use the given solving strategy"
                          "(e.g., slice-and-dice)")
                         (selected-strategy strategy)]
    [("-r" "--run-only") ("Do not solve; run traces in given directory"
                          "(by default, the contest-provided solutions)")
                         (run-only? #t)]
    #:once-any
    [("-a" "--all") "Iterate over all problems"
                    "(default is a representative subset)"
                    (all-problems? #t)]
    [("-p" "--problems") #{ps : String}
                         ("Iterate over only the specified problems"
                          "(<ps> is a comma-separated list of problem"
                          "codes like A001,D023,R050)")
                         (selected-problems ps)]
    #:once-each
    [("-w" "--write-trace-files") ("Write solutions to files in given directory"
                                   "(by default, named with time and strategy)")
                                  (write-trace-files? #t)]
    [("-d" "--directory") #{dir : String}
                          "Directory for use with -r or -w"
                          (selected-directory dir)])

  ;; Validate/interpret options
  (define strategy-fn : Strategy
    (cdr (or (assoc (selected-strategy) strategies-by-name)
             (error "unknown strategy" (selected-strategy)))))
  (define problems : (Listof problem)
    (let ((selected (selected-problems)))  ; for the type-checker
      (cond ((all-problems?)
             all-problems)
            (selected
              (map string->problem
                   (string-split selected ",")))
            (else
              sample-problems))))
  (define directory : String
    (or (selected-directory)
        (if (run-only?)
          "dfltTracesF"
          (parameterize ((date-display-format 'iso-8601))
            (date->string (current-date) #t)))))

  ;; Ensure the output directory exists
  (when (and (write-trace-files?)
             (not (directory-exists? directory)))
    (make-directory directory))

  ;; Generate and/or run a trace for each selected problem
  (for ((p problems))
    (define source-model : (Option model)
      (and (not (eq? (problem-type p) 'a))
           (load-model (format "problemsF/F~a_src.mdl" (problem->string p)))))
    (define target-model : (Option model)
      (and (not (eq? (problem-type p) 'd))
           (load-model (format "problemsF/F~a_tgt.mdl" (problem->string p)))))
    (define res (model-res (or source-model target-model
                               (error "never reached"))))
    (define trace
      (if (run-only?)
        (load-trace (format "~a/F~a.nbt" directory (problem->string p)))
        (let ((plan (strategy-fn num-seeds source-model target-model)))
          (compile-plan plan 
                        (or source-model (create-model res))
                        (or target-model (create-model res))
                        num-seeds))))
    (define system
      (create-system (if source-model
                       (copy-model source-model)
                       (create-model res))
                     num-seeds trace))
    (run-system! system)
    (if (model=? (system-model system)
                 (or target-model (create-model res)))
      (printf "Success.  Energy used = ~a~n" (system-energy system))
      (printf "Failure: final model does not match target.~n"))
    (when (write-trace-files?)
      (save-trace! (format "~a/F~a.nbt" directory (problem->string p))
                   trace))))

(main)
