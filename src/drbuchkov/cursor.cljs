(ns drbuchkov.cursor)

(deftype Cursor [the-atom path ^:mutable reaction
                 ^:mutable state ^:mutable watches]
  Object
  (equiv [this other]
    (-equiv this other))

  IAtom

  IEquiv
  (-equiv [o other] (identical? o other))

  IDeref
  (-deref [_] (get-in @the-atom path))

  IMeta
  (-meta [_] meta)

  IReset
  (-reset! [this new-value]
    (let [oldval @this
          newval (swap! the-atom assoc-in path new-value)]
      (-notify-watches this oldval newval)))

  ISwap
  (-swap! [this f] (let [oldval @this
                         newval (f oldval)]
                     (swap! the-atom assoc-in path newval)
                     (-notify-watches this oldval newval)))
  (-swap! [this f x] (let [oldval @this
                           newval (f oldval x)]
                       (swap! the-atom assoc-in path newval)
                       (-notify-watches this oldval newval)))
  (-swap! [this f x y] (let [oldval @this
                             newval (f oldval x y)]
                         (swap! the-atom assoc-in path newval)
                         (-notify-watches this oldval newval)))
  (-swap! [this f x y more] (let [oldval @this
                                  newval (apply f oldval x y more)]
                              (swap! the-atom assoc-in path newval)
                              (-notify-watches this oldval newval)))

  IPrintWithWriter
  (-pr-writer [a w opts]
    (-write w (str "#object[drbuchkov.cursor.Cursor["))
    (pr-writer @a w opts)
    (-write w ", ")
    (pr-writer path w opts)
    (-write w "]"))

  IWatchable
  (-notify-watches [this oldval newval]
    (doseq [[key f] watches]
      (f key this oldval newval)))
  (-add-watch [this key f]
    (set! (.-watches this) (assoc watches key f))
    this)
  (-remove-watch [this key]
    (set! (.-watches this) (dissoc watches key)))

  IHash
  (-hash [this] (goog/getUid this)))

(defn cursor [the-atom path]
  (->Cursor the-atom path nil nil nil))