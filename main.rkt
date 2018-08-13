#lang typed/racket

(require typed/racket/date)

(require "model.rkt")
(require "state.rkt")
(require "strategy.rkt")
(require "planning.rkt")
(require "trace.rkt")
(require "runtime.rkt")

(define num-seeds 39)

(define num-assembly-problems 186)  ; currently only assembly is supported
(define num-disassembly-problems 186)
(define num-reassembly-problems 115)

(define sample-problems : (Listof Natural)
  '(1 23 50 67 115 121 131 173))

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
                          "(<ps> is a comma-separated list of numbers)")
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
  (define problems : (Sequenceof Natural)
    (let ((selected (selected-problems)))  ; for the type-checker
      (cond ((all-problems?)
             (in-range 1 (+ num-assembly-problems 1)))
            (selected
              (map (compose (lambda ([x : (Union Number False)]) : Natural
                              (if (and (exact-integer? x) (>= x 1))
                                x
                                (error "not a problem number" x)))
                            string->number)
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
  (for ((n problems))
    (define target-filename
      (format "problemsF/FA~a_tgt.mdl"
              (~a n #:width 3 #:align 'right #:pad-string "0")))
    (define target-model (load-model target-filename))
    (define res (model-res target-model))
    (define source-model (create-model res))
    (define trace
      (if (run-only?)
        (let ((trace-filename
                (format "~a/FA~a.nbt"
                        directory
                        (~a n #:width 3 #:align 'right #:pad-string "0"))))
          (load-trace trace-filename))
        (let ((plan (strategy-fn num-seeds source-model target-model)))
          (compile-plan plan source-model target-model num-seeds))))
    (define system (create-system res num-seeds trace))
    (run-system! system)
    (if (model=? (system-model system) target-model)
      (printf "Success.  Energy used = ~a~n" (system-energy system))
      (printf "Failure: final model does not match target.~n"))
    (when (write-trace-files?)
      (define trace-filename
        (format "~a/FA~a_soln.nbt"
                directory
                (~a n #:width 3 #:align 'right #:pad-string "0")))
      (save-trace! trace-filename trace))))

(main)
