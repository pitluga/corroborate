# corroborate

A dead-simple validations library for Clojure.

## Installation:

Leiningen:

```clojure
[corroborate "0.2.0"]
```

Maven:

    <dependency>
      <groupId>corroborate</groupId>
      <artifactId>corroborate</artifactId>
      <version>0.2.0</version>
    </dependency>

## Usage

```clojure
(ns people
  (:use [corroborate.core]))

(defvalidator validate-person
  :first-name (is-required)
  :last-name (is-required)
  :phone (is-formatted #"\(\d{3}\) \d{3}-\d{4}")))

(validate-person {:first-name "Tony" :last-name "" :phone "867-5309"})
; {:last-name ["is required"] :phone ["is improperly formatted"]}
```

## License

Copyright (C) 2011 Tony Pitluga

Distributed under the Eclipse Public License, the same as Clojure.
