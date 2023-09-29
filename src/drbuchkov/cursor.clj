(ns drbuchkov.cursor
  (:import (clojure.lang IAtom IAtom2 IDeref IRef)))

(defn cursor [the-atom path]
  (reify
    IAtom
    (swap [this f]
      (swap-vals! the-atom update-in path f))
    (swap [this f arg]
      (swap! the-atom update-in path f arg))
    (swap [this f arg1 arg2]
      (swap! the-atom update-in path f arg1 arg2))
    (swap [this f x y args]
      (apply swap! the-atom update-in path f x y args))
    (reset [this newval]
      (get-in (swap! the-atom assoc-in path newval) path))
    (compareAndSet [this oldv newv]
      (if (= (get-in @the-atom path) oldv)
        (swap! the-atom assoc-in path newv)))
    IDeref
    (deref [this] (get-in @the-atom path))))