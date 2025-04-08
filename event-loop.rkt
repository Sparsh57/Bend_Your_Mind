#lang scribble/lp2

@title{Event loop}

@table-of-contents{}

This module provides a basic implementation of the concept
of an "event loop". It is not a real implementation that one
would use in an actual program, since such an event loop has
details that are not of interest to understand the @bold{idea}
of event loops.

@chunk[
 <*>
 (require racket)

 (provide make-event-loop
          evl-schedule
          evl-trigger-event
          evl-add-handler!
          evl-remove-handler!
          evl-loop
          promise
          promise-then!
          promise-catch!)

 |Example procedure exports|
 |Debugging support|
 |Queues|
 |Event loop|
 |Promises|
 |Utilities and examples|
 <test>
 ]

The above provided procedures are the only ones you need
to know about. The rest of the code and definitions support
their implementation.

@chunk[
 |Example procedure exports|
 (provide run-in-event-loop
          set-timeout
          simulate-http-get-text)
 ]

These procedures are useful to smoke test an "event loop"
implementation. @code{run-in-event-loop} is just a convenience
wrapper around @code{event-loop}. You should be able to follow
its code or even write your own wrapper.

@chunk[
 |Debugging support|
 (define debug #t)
 (define (debugging bool)
   (set! debug (if bool #t #f)))
 ]

Call @code{(debugging #f)} to turn off tracing of internal
functions.

@section{API}

@defproc[(run-in-event-loop [promise-proc procedure?]) void?]{
 Takes a procedure that gets a single "event loop" structure
 as an argument and is expected to return a "promise" that
 is then run in that event loop.
}

@defproc[(make-event-loop) Evl?]{
Makes a new "event loop" structure that maintains the necessary
queues for tasks and events.
}

@defproc[(evl-schedule [evl Evl?] [proc procedure?]) Evl?]{
 Schedules the given procedure that takes a single argument
 which is the event loop itself. The result of the procedure
 is ignored. The proc will get to run in the next "tick".
}

@defproc[(evl-trigger-event [evl Evl?] [event-id symbol?] [args any] ...)
         Evl?]{
 Triggers the specified event (given by @code{event-id})
 The arguments are passed to any handlers installed for
 the event-id.
}

@defproc[(evl-add-handler! [evl Evl?] [event-id symbol?] [handler (or/c #f procedure?)])
         Evl?]{
 Installs a handler procedure that is called with the event-id and any
 additional arguments supplied when the event was triggered. The handler's
 result value is ignored. The handler is called like shown below --

 @code{(handler evl event-id arg1 arg2 ...)}
}

@defproc[(evl-remove-handler! [evl Evl?] [event-id symbol?] [handler (or/c #f procedure?)])
         Evl?]{
 Removes a handler installed to process the given event id.
 Give @code{#f} for the handler to remove @bold{all} handlers
 attached to the given @code{event-id}.
}

@defproc[(evl-loop [evl Evl?] [tick integer?] [upto integer?])
         Evl?]{
 Runs the event loop from the given tick number up to the
 given @code{upto} tick. Goes without saying that it expects
 @code{(> upto tick)} to hold if any tick is to elapse at all.

 A tick is considered as a pretty abstract "step". Currently it is
 coded to be approximately 1ms in duration at least, but it is
 just a step in which all the pending events and scheduled tasks
 are processed by @code{evl-loop}.
}

@defproc[(promise [evl Evl?] [proc procedure?]) P?]{
 Makes and schedules a "promise" to produce a value. The
 @code{proc} is a procedure that takes two arguments --
 a @code{resolve} procedure and a @code{reject} procedure --
 which are to be called when the final value of the promise
 becomes available. @code{P} is the struct type of a promise.
 }

@defproc[(promise-then! [p P?] [thenproc procedure?]) P?]{
 Sets things up so that when the given promise completes
 with a value, that value is made available as input to the
 given @code{thenproc} to determine what to continue with.
 The result of @code{promise-then!} is itself a new promise to
 produce the value that's the result of the @code{thenproc}.
 The result of that can itself be a promise, and so this is
 recursively handled.
}

@defproc[(promise-catch! [p P?] [catchproc procedure?]) P?]{
 Sets things up to call the given @code{catchproc} with an error
 in case the promise @code{p} fails with a rejection. Otherwise
 similar to @code{promise-then!}. The promise produced by a
 catch handler can itself resolve normally with a value.
 }

@section{Event loops}

The event loop's main processing step is called a "tick"
and in one tick, the event loop processes all pending tasks
and dispatches all pending events. This is not a fundamental
concept that an event loop must do this, but it is convenient
to do so. Since task procedures and event handlers may themselves
add new tasks and events to the loop's queues, we need to protect
the queues from being mutated by them. To do this, we keep two
queues for each kind which we swap before processing so that
any additions go into the swap queues that won't be seen by
the processing loop.

@code{tick} counts the number of elapsed ticks.

@code{handlers} is simply an association list mapping event-identifiers
to handler procedures.

@chunk[
 |Event loop|
 (struct Evl (tick
              taskq taskq-swap
              eventq eventq-swap
              handlers)
   #:mutable)
 
 |Making an event loop|
 |Scheduling a task procedure|
 |Triggering events|
 |Adding event handlers|
 |Removing event handlers|
 |Performing one tick of the event loop|
 |The event loop|
 ]

A fresh event loop has only empty queues and no handlers.

@chunk[
 |Making an event loop|
 (define (make-event-loop)
   (Evl 0 (make-queue) (make-queue) (make-queue) (make-queue) '()))
 ]

@subsection{Scheduling tasks and events}

A task procedure is an ordinary Racket procedure that takes
a single argument -- the event loop -- and does whatever it
needs to.

@chunk[
 |Scheduling a task procedure|
 (define (evl-schedule evl proc)
   (Q-push! (Evl-taskq evl) proc)
   evl)
 ]

An event triggered may come with additional information
other than the event-id. To allow for that, we think
of the whole event as a list whose first element is
the event-id and the rest of the elements are values that
describe the event. It is this list we push into the
event queue for later processing.


@chunk[
 |Triggering events|
 (define (evl-trigger-event evl event-id . args)
   (Q-push! (Evl-eventq evl) (cons event-id args))
   evl)
 ]

@subsection{Installing and removing handlers}

An event handler is a procedure whose argument list has the
same form as the event descriptor list -- i.e. @code{(<event-id> . <args>)}.
So a handler fn may look like -

@racketblock[
 (define (handler event-id x y)
   (unless (equal? event-id 'mouse-click)
     (raise 'mouse-click-event-expected))
   (code:comment ".. do other things..")
   )
 ]

@index['("API" "evl-add-handler!")]
@chunk[
 |Adding event handlers|
 (define (evl-add-handler! evl event-id handler)
   (set-Evl-handlers! evl (cons (cons event-id handler)
                                (Evl-handlers evl)))
   evl)
 ]

While adding an event handler is straight forward,
removing an event handler may involve one of two things
-- either the caller wishes to remove a specific handler
for a specific event-id, or the caller wishes to remove
all event handlers associated with an event-id. We support
both here. For the latter case, just omit the handler
argument.

@index['("API" "evl-remove-handler!")]
@chunk[       
 |Removing event handlers|
 (define (evl-remove-handler! evl event-id [handler #f])
   (let ([filter-fn (if handler
                        (λ (h) (equal? (car h) event-id))
                        (λ (h)
                          (and (equal? (car h) event-id)
                               (eq? (cdr h) handler))))])
     (set-Evl-handlers! evl (filter filter-fn (Evl-handlers evl))))
   evl)
 ]

@subsection{Running the event loop}

@index['("API" "evl-tick")]
@chunk[
 |Performing one tick of the event loop|
 (define (evl-tick evl tick)
   |swap task queues|
   |swap event queues|
   |process task queue|
   |process event queue|
   )
 ]

As mentioned earlier, running a single tick of our event loop
involves swapping the task and event queues and emptying them
one by one. The end result of a tick is that the items that were
in the queue at the start of the tick all end up being processed
by the end, though the processing steps themselves might add new
items for processing in the next tick.

A common language semantic employed in programming languages with
event loop support is that a task added is always guaranteed to
execute only on the next tick. This is a useful guarantee in some
situations.

@chunk[
 |swap task queues|
 (define taskq (Evl-taskq evl))
 (set-Evl-taskq! evl (Evl-taskq-swap evl))
 (set-Evl-taskq-swap! evl taskq)
 ]

@chunk[
 |swap event queues|
 (define eventq (Evl-eventq evl))
 (set-Evl-eventq! evl (Evl-eventq-swap evl))
 (set-Evl-eventq-swap! evl eventq)
 ]

Now, @code{taskq} and @code{eventq} give the queue state
at start of the tick. We process the tasks by applying the
task procedures to the event loop as their sole argument.

@chunk[
 |process task queue|
 (let loop ([q taskq])
   (unless (Q-empty? q)
     ((Q-pop! q) evl)
     (loop q)))
 ]

Processing the event queue involves finding the handlers
associated with each of the events and running them in
order. The order of running the handlers should not be
assumed to be the same order as that in which the handlers
are installed, but that is a common assumption people make
and event loops usually accommodate it.

@chunk[
 |process event queue|
 (let loop ([q eventq])
   (unless (Q-empty? q)
     (let* ([event (Q-pop! q)]
            [evh (map cdr
                      (filter
                       (λ (e)
                         (equal? (first e) (first event)))
                       (Evl-handlers evl)))])
       (for-each (λ (hfn)
                   (apply hfn (cons evl event)))
                 (reverse evh)))
     (loop q)))
 ]

The @code{evl-loop} procedure puts together all of that
and runs a given number of ticks, or until no tasks
or events remain. In the general case, an event loop won't
terminate when no tasks or events are in the queues,
but will "idle" until an event arrives and will therefore
need to be stopped explicitly. In our case, there are no
such external sources pumping events into our event loop
to warrant that.

@index['("API" "evl-loop")]
@chunk[
 |The event loop|
 (define (evl-loop evl tick upto)
   (when (and (<= tick upto)
              (not (and (Q-empty? (Evl-taskq evl))
                        (Q-empty? (Evl-eventq evl)))))
     (set-Evl-tick! evl tick)
     (when debug
       (when (= (remainder tick 200) 0)
         (displayln (format "<Tick ~s>" tick))
         (flush-output)))
     (evl-tick evl tick)
     (sleep 0.001)
     (evl-loop evl (+ tick 1) upto)))    
 ]


@section{Promises}

A "promise" is an abstraction that stands for a value that may
become available at some point in the future due to potentially
asynchronous activities. A promise to provide a value therefore
may either be successfully met or may fail to be kept due to
circumstances not knowable at initiation time.

The @code{desc} field is an arbitrary value that can help identify a
promise when debugging. Also, when a promise gets resolved or rejected,
it prints out this description field in the log for identification.

@chunk[
 |Promises|
 (struct P (desc evl state val error proc then catch)
   #:mutable)

 (define (P-resolved? p)
   (and (P? p) (eq? (P-state p) 'resolved)))

 (define (P-rejected? p)
   (and (P? p) (eq? (P-state p) 'rejected)))

 (define (P-pending? p)
   (and (P? p) (eq? (P-state p) 'pending)))

 (define (P-not-started? p)
   (and (P? p) (eq? (P-state p) 'not-started)))

 |Making a promise|
 |Keeping a promise|
 |Sequencing after promise success|
 |Sequencing after promise failure|
 ]

A promise may be in one of the following states -

@itemlist[
 #:style 'ordered
 @item{Not started: The computation that will eventually
  fulfill the promise has not yet started. A promise can go
  from this to "pending resolution" state upon being
  scheduled.}
 @item{Pending resolution: The computation is in progress
  and is expecting some intermediate activity to be completed
  before resolution or rejection. A promise may go from this
  to either "resolved" or "rejected" state or stay in this
  same state.}
 @item{Resolved: Once resolved, the state of the promise cannot change.}
 @item{Rejected: Once rejected, the state of the promise cannot change.}
]

@index['("API" "promise")]
@chunk[
 |Making a promise|
 (define (mk-promise desc evl proc)
   (P desc evl 'not-started (void) (void) proc empty empty))
 
 (define (promise desc evl proc)
   (promise-schedule (mk-promise desc evl proc)))

 (define (promise-schedule p)
   (evl-schedule (P-evl p) (λ (evl) (promise-keep p)))
   p)
 ]

The procedure passed to @racket[promise] takes two arguments -- a @code{resolve}
procedure of arity 1 and a @code{reject} procedure of arity 1, to be
called by the code in those circumstances (See @secref{example-code} below).

When a promise is scheduled, it is guaranteed to start only on the
next "tick" of the event loop.

@chunk[
 |Keeping a promise|   
 (define (promise-keep p)
   (if (P? p)
       (cond
         [(P-resolved? p)
          |Handle a resolved promise|
          ]
         [(P-rejected? p)
          |Handle a rejected promise|
          ]
         [(P-pending? p)
          (code:comment "Nothing to do. Task started already")
          p]
         [else
          |Invoke the promise procedure|
          ])
       p))
 ]

@chunk[
 |Handle a resolved promise|
 (let loop ([pthen (P-then p)])
   (set-P-then! p empty)
   (for-each promise-keep (reverse pthen))
   (unless (empty? (P-then p))
       (loop (P-then p))))
 (P-val p)
 ]

To handle a resolved promise, all its "then" promises
must be run.

@chunk[
 |Handle a rejected promise|
 (let loop ([pcatch (P-catch p)])
   (set-P-catch! p empty)
   (for-each promise-keep (reverse pcatch))
   (unless (empty? (P-catch p))
     (loop (P-catch p))))
 (P-error p)
 ]

To handle a rejected promise, all its "catch" promises
must be run. These may turn around the result from an
error into a normal value.

@chunk[
 |Invoke the promise procedure|
 (when (P-not-started? p)
   (set-P-state! p 'pending)
   ((P-proc p) |Resolve procedure| |Reject procedure|))
 ]

Starting a promise's activities involves running its
procedure with appropriately constructed resolver and
rejection procedures.

When a promise completes with a resolution by calling
its @code{resolve} procedure given as an argument,
we mark the promise as resolved, store the value, and
run all the computations dependent on the outcome of this
promise. That involves running through all the "then"
promises and trying to keep them, now that the promise
has a value to pass on to them.

@chunk[
 |Resolve procedure|
 (λ (val)
   (when debug
     (displayln (format "Resolving ~s with ~s" (P-desc p) val)))
   (when (or (P-resolved? p) (P-rejected? p))
     (raise 'promise-already-frozen))
   (define (done val)
     (set-P-state! p 'resolved)
     (set-P-val! p (promise-keep val))
     (let ([pthen (P-then p)])
       (set-P-then! p empty)
       (for-each promise-keep (reverse pthen)))
     val)
   (if (P? val)
       (promise-then! (format "cont ~s" (P-desc p))
                      val
                      done)
       (done val)))
 ]

Similarly when a promise fails by calling the passed
@code{reject} procedure, the promises installed
for catching the condition can themselves resolve
normally (or choose to fail). To do this we need
to keep all the catch promises installed, now that
an error value is available.

@chunk[
 |Reject procedure|
 (λ (err)
   (displayln (format "Rejecting ~s with ~s" (P-desc p) err))
   (when (or (P-resolved? p) (P-rejected? p))
     (raise 'promise-already-frozen))
   (define (done err)
     (set-P-state! p 'rejected)
     (set-P-error! p err)
     (let ([pcatch (P-catch p)])
       (set-P-catch! p empty)
       (for-each promise-keep (reverse pcatch)))
     err)
   (if (P? err)
       (promise-then! (format "errcont ~s" (P-desc p))
                      err
                      done)
       (done err)))
 ]

So how do we mark a computation that's dependent
on the outcome of a promise? The @code{promise-then!}
procedure sets up the scheduler such that the given
@code{thenproc} will be called once the promise @code{p}
completes, with the value that the promise resolves to.
In case the promise gets rejected, then the @code{thenproc}
should not be called.

In this implementation, we setup another promise that
is registered to be invoked once the given promise completes.
A similar process applies to the @code{catch} of promise
rejections, with @code{promise-catch!} playing that role.

@index['("API" "promise-then!")]
@chunk[
 |Sequencing after promise success|
 (define (promise-then! desc p thenproc)
   (let ([tp (mk-promise desc (P-evl p)
                         (λ (resolve reject)
                           (let ([p2 (thenproc (P-val p))])
                             (if (P? p2)
                                 (begin (promise-then!
                                         (format "~s + follow-succ" desc)
                                         p2
                                         resolve)
                                        (promise-catch!
                                         (format  "~s + follow-fail" desc)
                                         p2
                                         reject))
                                 (resolve p2)))))])
     (set-P-then! p (cons tp (P-then p)))
     (code:comment "Important to schedule the promise if")
     (code:comment "it has already been resolved.")
     (when (P-resolved? p)
       (promise-schedule p))
     tp))
 ]

@index['("API" "promise-catch!")]
@chunk[
 |Sequencing after promise failure|
 (define (promise-catch! desc p catchproc)
   (let ([cp (mk-promise desc (P-evl p)
                         (λ (resolve reject)
                           (let ([p2 (catchproc (P-error p))])
                             (if (P? p2)
                                 (begin (promise-then!
                                         (format "~s + follow-succ" desc)
                                         p2
                                         resolve)
                                        (promise-catch!
                                         (format  "~s + follow-fail" desc)
                                         p2
                                         reject))
                                 (resolve p2)))))])
     (set-P-catch! p (cons cp (P-catch p)))
     (code:comment "Important to schedule the promise if")
     (code:comment "it has already been rejected.")
     (when (P-rejected? p)
       (promise-schedule p))
     cp))
 ]

@chunk[
 |Utilities and examples|
 (define (run-in-event-loop promise-proc)
   (let ([evl (make-event-loop)])
     (promise-proc evl)
     (evl-loop evl 0 1000000)))
    
 (define (set-timeout evl fn delay_ticks)
   (let* ([tick (Evl-tick evl)]
          [trigger-tick (+ tick delay_ticks)])
     (define (check evl)
       (if (>= (Evl-tick evl) trigger-tick)
           (fn)
           (evl-schedule evl check)))
     (evl-schedule evl check)))

 (define (simulate-http-get-text evl url [failure-prob 0.15])
   (promise `(http-get ,url) evl
            (λ (resolve reject)
              (displayln (format "GET ~s" url))
              (set-timeout evl
                           (λ ()
                             (if (< (random) failure-prob)
                                 (begin (displayln (format "Failed request for ~s" url))
                                        (reject "Failed"))
                                 (begin (displayln (format "Response received for ~s" url))
                                        (resolve "Hello world!"))))
                           25))))
 ]

@section{Queues}

An event loop processes two queues -- a task queue
and an event queue. Tasks can be launched by other tasks
as well into an event loop and an event queue may receive
events from external sources such as network, timers,
user interface components and such. An event loop's
task is to keep these two queues serviced.

We therefore define a simple queue data structure @code{Q}
that models a first-in/first-out mutable queue suitable
for our purposes.

@chunk[
 |Queues|
 (struct Q (head tail) #:transparent #:mutable)
 
 |Make queue|
 |Empty queues|
 |Pushing to a queue|
 |Popping from a queue|
 ]

The form we use is to store a pair of lists, one called
the "head" and the other the "tail". When a value is
written to the queue, it is prefixed to the "head" list.
When a value is taken from the queue, the first element
of the "tail" is taken out and the tail stands reduced
by one element.

Supposing we exhaust the elements in the tail list, the
queue is modified so that the reverse of the head list
becomes the new tail list and the new head is empty.
This operation preserves the queue invariant of not
changing the length and the first-in/first-out characteristic.

While reverse looks like an expensive operation to do,
the fact that it only needs to be done "once in a while",
means its cost gets amortized over other queue operations.
For details of its complexity analysis (which you don't
need to get into for our purposes), see
@hyperlink["https://www.cs.cmu.edu/~wlovas/15122-r11/lectures/09-queues.pdf"]{CMU lecture notes on queues}.

A fresh queue has no elements and therefore has empty lists
for both its head and tail.

@chunk[
 |Make queue|
 (define (make-queue) (Q empty empty))
 ]

Since we can only take an element from a queue if it has any
in the first place, we need a procedure to tell us whether
a queue is empty or not. According to our scheme, a queue is
empty if and only if both its head and tail lists are empty.

@chunk[
 |Empty queues|
 (define (Q-empty? q)
   (and (empty? (Q-head q)) (empty? (Q-tail q))))
 ]

"Pushing a value into a queue" simply involves cons-ing
the head with the element as noted earlier. The new head
will contain the pushed item as its first value.
@chunk[
 |Pushing to a queue|
 (define (Q-push! q x)
   (set-Q-head! q (cons x (Q-head q)))
   q)
 ]

We have some choices regarding popping and its behaviour
when there are no elements in the queue. In our case,
we choose to have a default behaviour that raises an
@code{'empty-queue-error} in that case, but we have the
behaviour overridable with a procedure to call to determine
the value in such cases.

@chunk[
 |Popping from a queue|
 (define (raise-empty-queue-error q)
   (raise 'empty-queue-error))

 (define (Q-pop! q [onempty raise-empty-queue-error])
   (if (empty? (Q-tail q))
       (if (empty? (Q-head q))
           (onempty q) ; raises 'empty-queue-error by default
           |reverse head and use it as tail|)
       |take value from non-empty tail|
       ))
 ]

@chunk[
 |reverse head and use it as tail|
 (begin (set-Q-tail! q (reverse (Q-head q)))
        (set-Q-head! q empty)
        (Q-pop! q onempty))
 ]

@chunk[
 |take value from non-empty tail|
 (let ([x (first (Q-tail q))])
   (set-Q-tail! q (rest (Q-tail q)))
   x)
 ]


@section[#:tag "example-code"]{Example code}

@chunk[
 <test>
 empty
 ]

@chunk[
 <test-disabled>
 (define (wait-for desc evl ms val)
   (promise desc evl
            (λ (resolve reject)
              (displayln (format "Started timer for ~s ms" ms))
              (set-timeout evl
                           (λ ()
                             (displayln "Timer fired")
                             (resolve val))
                           ms))))
 (run-in-event-loop
  (λ (evl)
    (let* ([p1 (wait-for 'T1 evl 3000 "T1 completed")]
           [p2 (promise-then! 'T2 p1 (λ (val)
                                   (wait-for 'T2-waiter evl 600 "T2 completed")))]
           [p3 (promise-then! 'DONE p2 (λ (val)
                                   (displayln "DONE")))])
      p3)))


 (define evl (make-event-loop))
 (evl-schedule evl
               (λ (evl)
                 (displayln "Started timer for 2secs")
                 (set-timeout evl
                              (λ ()
                                (displayln "hello timer"))
                              2000)))
 (evl-loop evl 0 1000000)
 ]

@index-section[]