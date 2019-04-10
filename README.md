# Semaphore for Emacs 26+

This semaphore is based on a `condition-variable` and two monotonically increasing counters `acquired` and `released`.

This way, it can support a performance counter and interacts well with threads.

It offers integration with [emacs-promise](https://github.com/chuntaro/emacs-promise) in `semaphore-promise`

## API

```el
(require 'semaphore)
```

### make-semaphore
```el
defun make-semaphore (count &optional name)
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

### semaphore-gated-promise
```el
defun semaphore-gated-promise (semaphore handler)
```
Create a promise based on handler, like promise-new, but
acquire and release the semaphore around resolving the
handler (+ result promises).

This is useful for controlling parallelism in asynchronous work flows:
e.g. if you want to fetch 10000 files, but only 7 at a time:

```el
(let ((s (make-semaphore 7)))
  (promise-all
    (mapcar
      (lambda (url)
        (semaphore-gated-promise s
          (lambda (resolve _)
            (resolve (http-get url))))) ;; assuming http-get returns another promise
      url-list))
```

## Tests

```sh
cask install
cask exec ert-runner
```
