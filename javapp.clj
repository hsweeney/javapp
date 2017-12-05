; javapp: a partial implementation of javap.
; It lists the members of the specified class.

; Copyright (c) 2017 Hugh Sweeney (hsl@pobox.com)

; This program takes one argument on the command line: the name of the class of interest.
; No command-line switches are supported, but it is designed to operate as javap would
; operate with the '-private' switch (and only that switch) specified.

; I wrote this tool originally in Javascript. Then I translated it to Scala. Then to this
; Clojure implementation. Much of this is non-idiomatic (read: terrible) Clojure. I wanted to
; get a working program before having to learn everything about Clojure. I learned a lot.


(import java.lang.reflect.Constructor
	java.lang.reflect.Field
	java.lang.reflect.Method
	java.lang.reflect.Modifier)

; // Functions
(def sp     "pretty-printing"
  (fn
  ( []     (println) )
  ( [text] (sp) (println text) )
  ) )

; // Functions used throughout:
; // ...............................................................................

(defn modsToString [mods] 	"Human-readable modifiers"
  (def words [])
  (defn wp [w] (def words (conj words w)))	; there's gotta be a better way. Recursion?
	(if (Modifier/isPublic mods)    	(wp "public"))
	(if (Modifier/isPrivate mods)   	(wp "private"))
	(if (Modifier/isProtected mods) 	(wp "protected"))
	(if (Modifier/isStatic mods) 		  (wp "static"))
	(if (Modifier/isFinal mods) 		  (wp "final"))
	(if (Modifier/isSynchronized mods) 	(wp "synchronized"))
	(if (Modifier/isVolatile mods) 		(wp "volatile"))
	(if (Modifier/isTransient mods) 	(wp "transient"))
	(if (Modifier/isInterface mods) 	(wp "interface"))
	(if (Modifier/isAbstract mods) 		(wp "abstract"))
	(if (Modifier/isStrict mods) 		  (wp "strict"))
	(clojure.string/join " " words))
; // ...............................................................................

(defn jlTrim [name] "Strip 'java.lang.' from front of name"
  (if ( and (= (.lastIndexOf name ".") 9) (= "java.lang." (subs name 0 10) ) )
    ( subs name 10 )
    name )
)
; // ...............................................................................

(defn n [ text ] "Make an (array) type name more human-friendly."
	; // See http://docs.oracle.com/javase/6/docs/api/java/lang/Class.html#getName%28%29

  (if-not (= "[" (subs text 0 1))        
    (jlTrim text)			; text not an array type
  ; else, text is eg "[[[D;"
  (let 
    [ i  (inc (.lastIndexOf text "["))  ; Number of brackets eg 3
      br (subs text 0 i)                ; All the brackets eg "[[["
      bt (subs text i)                  ; The base type (code) eg "D;"
      c  (subs bt 0 1)                  ; The type code initial eg "D"
    ]
    (str
    (cond 
      (= c "Z") "boolean"
      (= c "B") "byte"
      (= c "C") "char"
      (= c "L") (jlTrim (subs bt 1 (.lastIndexOf bt ";") ) )  ; Class name
      (= c "D") "double"
      (= c "F") "float"
      (= c "I") "int"
      (= c "J") "long"
      (= c "S") "short"
      ) ; end cond        ; eg "double"
    (apply str (interleave br (repeat (count br) "]")))   ; Pair up, eg "[][][]"
    )  ; end str          ; eg "double[][][]"
  ) ; end let
  ) ; end if-not
) ; end defn
; // ...............................................................................

(defn strung-constructor [ ctor ] "Details of constructor"
  (str (modsToString (.getModifiers ctor)) " " 
       (jlTrim (.getName ctor)) "("
       (clojure.string/join ", " (map (fn[x] (n (.getName x))) (.getParameterTypes ctor)))
       ");")
)
; // ...............................................................................

(defn strung-field [ fld ] "Details of field"
  (str (modsToString (.getModifiers fld)) " " 
       (n (.getName (.getType fld))) " "
       (.getName fld)
       ";")
)
; // ...............................................................................

(defn strung-method [ mth ] "Details of method"
  (str 
       (let [s (modsToString(.getModifiers mth))] (if (> (count s) 0) (str s " ") ))
       (n (.getName (.getReturnType mth))) " "
       (.getName mth) "("
       (clojure.string/join ", " (map (fn[x] (n (.getName x))) (.getParameterTypes mth) ) )
       ");")
)

; // End of function definitions
; // :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

; // Load and display the desired class info:
(let [ oCl (Class/forName (first *command-line-args* )) ]
 
  (if (.getPackage oCl) (print " package" (.getName (.getPackage oCl)) "\n"))

  (print (modsToString (.getModifiers oCl)) "class" (.getName oCl))

  (if-not (.isInterface oCl)
  	(if-not (= "java.lang.Object" (.getName (.getSuperclass oCl)))
      (print " extends" (.getName (.getSuperclass oCl)))))

  ; (sp "Interfaces")
  (if (> (count (.getInterfaces oCl)) 0) 
      (print " implements" (clojure.string/join " " (map (fn[x] (.getName x)) (.getInterfaces oCl) ))))
  (print " {\n")     ;// End of class declaration, start of body;

  ; (sp "Enumerate constructors")
  (println (clojure.string/join "\n" (map (fn[x] (strung-constructor x)) (.getConstructors oCl))))

  ; (sp "Enumerate fields")
  (println (clojure.string/join "\n" (map (fn[x] (strung-field x)) (.getDeclaredFields oCl))))

  ; (sp "Enumerate methods")
  (println (clojure.string/join "\n" (map (fn[x] (strung-method x)) (.getDeclaredMethods oCl))))
  (println " }")
)
