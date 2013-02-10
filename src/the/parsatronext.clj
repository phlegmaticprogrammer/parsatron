(ns the.parsatronext
  (:use the.parsatron))

(defn char_le [c1 c2] (<= (Character/getNumericValue c1) (Character/getNumericValue c2)))
(defn char_ge [c1 c2] (char_le c2 c1))

(defparser p-letter []
  (token (fn [c] (or (and (char_ge c \a) (char_le c \z))
                     (and (char_ge c \A) (char_le c \Z))))))

(defparser p-char [c]
  (token (fn [d] (= c d))))

(defparser p-char-case-insensitive [c]
  (token (fn [d] (= (Character/toLowerCase c) (Character/toLowerCase d)))))

(defparser p-digit []
  (token (fn [c] (and (char_ge c \0) (char_le c \9)))))

(defparser p-digits []
  (let->>[digits (many1 (p-digit))]
    (always (apply str digits))))

(defparser p-letter-or-digit []
  (choice (p-letter) (p-digit)))

(defparser p-underscore []
  (p-char \_))

(defparser p-apply [f p]
  (let->> [x p]
    (always (f x))))
       
(defparser p-seq-prefix [ps]
  (if (empty? ps)
    (always nil)
    (choice (let->> [x (attempt (first ps))
                     xs (p-seq-prefix (next ps))]
              (always (cons x xs)))
            (always nil))))

(defparser p-seq [ps]
  (if (empty? ps)
    (always nil)
    (let->> [x (first ps)
             xs (p-seq (next ps))]
      (always (cons x xs)))))

(defn repeat2 [a b] (interleave (repeat a) (repeat b)))

(defparser p-alternating [p1 p2]
  (choice (let->> [x (attempt p1)
                   xs (p-seq-prefix (repeat2 p2 p1))]
            (always (cons x xs)))
          (let->> [x (attempt p2)
                   xs (p-seq-prefix (repeat2 p1 p2))]
            (always (cons x xs)))))

(declare drop-even)

(defn drop-odd [seq]
  (if (empty? seq) seq (cons (first seq) (drop-even (next seq)))))

(defn drop-even [seq]
  (if (empty? seq) seq (drop-odd (next seq))))

(defparser p-whitespace []
  (token (fn [c] (Character/isWhitespace c))))

(defparser p-str [s]
  (let->> [t (p-seq (map p-char s))]
    (always (apply str t))))

(defparser p-str-case-insensitive [s]
  (let->> [t (p-seq (map p-char-case-insensitive s))]
    (always (apply str t))))

(defparser p-optional [p]
  (choice (attempt p) (always nil)))

(defparser p-long []
  (let->>[sign (p-optional (p-str "-"))
          s (p-digits)]
    (let [x (read-string (str sign s))]
      (if (and (<= Long/MIN_VALUE x) (<= x Long/MAX_VALUE))
        (always x)
        (never)))))

(defparser p-all [p]
  (let->>[_ (many (p-whitespace))
          r p
          _ (many (p-whitespace))
          _ (eof)]
    (always r)))

(defn parse [p s]
  (try
    (run p s)
    (catch Exception e nil)))
   




