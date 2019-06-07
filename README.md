# Semaphore for Emacs 26+

This semaphore is based on a `condition-variable` and two monotonically increasing counters `acquired` and `released`.

This way, it can support a performance counter and interacts well with threads.

This package offers optional integration with [emacs-promise](https://github.com/chuntaro/emacs-promise) in `semaphore-promise`, that can function as a kind of bounded thread-pool, but for promises.

## API

```el
(require 'semaphore)
```

### semaphore-create
```el
defun semaphore-create (count &optional name)
```
Create a semaphore with count initially available slots (can be negative)

Use semaphore-acquire, to borrow a slot, and semaphore-release, to return it.

### semaphore-acquire
```el
cl-defmethod semaphore-acquire ((this semaphore))
```
Borrow a slot from the semaphore

If there are no slots available, semaphore-acquire will block your thread, until there are.
Don't do this on the main thread, or risk emacs freezing dead.

### semaphore-release
```el
cl-defmethod semaphore-release ((this semaphore))
```
Return a slot to the semaphore

This will unblock one blocked acquirer, if any.

### semaphore-name
```el
cl-defmethod semaphore-name ((this semaphore))
```
Given name of the semaphore

### semaphore-times-acquired
```el
cl-defmethod semaphore-times-acquired ((this semaphore))
```
An absolute count of times acquired over lifetime of the semaphore

### semaphore-mutex
```el
cl-defmethod semaphore-mutex ((this semaphore))
```
Access to the semaphore's mutex. Be careful.

### semaphore-available
```el
cl-defmethod semaphore-available ((this semaphore))
```
Get a current count of available slots

Mostly for logging purposes.

If you want to base any decision in the program on available
count, you need to have the semaphore-available call and
following action synchronized on the semaphore mutex.

Not recommended, unless you have a pressing need and a
convincing story about preventing deadlocks.

## promise integration

```el
(require 'semaphore-promise)
```

### semaphore-promise-gated
```el
defun semaphore-promise-gated (semaphore handler)
```
Create a promise based on handler, like promise-new, but
acquire and release the semaphore around resolving the
handler (+ result promises).

This is useful for controlling parallelism in asynchronous work flows:
e.g. if you want to fetch 10000 files, but only 7 at a time:

```el
(let ((s (semaphore-create 7)))
  (promise-all
    (mapcar
      (lambda (url)
        (semaphore-promise-gated s
          (lambda (resolve _)
            (funcall resolve (http-get url))))) ;; assuming http-get returns another promise
      url-list))
```

## Tests

```sh
cask install
cask exec ert-runner
```

## Guide: Writing multithreaded emacs batch programs

When trying to run multithreaded elisp in batch mode, run emacs as
foreground daemon, with `--fg-daemon=<name>`. Call `(kill-emacs 0)` at
the end of the script. This way, emacs will keep running your threads,
until you terminate it explicitly.

### Example

#### worker.el

```el
(defun main ()
  (make-thread
   (lambda ()
     (message "Finished, exiting")
     (kill-emacs 0))))
```

#### worker

```sh
#!/bin/sh

exec emacs --fg-daemon=worker --quick -l worker.el -f main "$@"
```
