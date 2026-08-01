;; Throwaway probe, fork-only. Reports where focus lands on each AutoFill fixture,
;; against whatever base url it is given, so http and https can be compared on one
;; machine. Observes rather than asserts: every scheme prints a row, none of them
;; fail the script, and the comparison is made by reading the two tables.
;;
;; Mirrors focus-then-wait in etaoin.api-autofill-test, including the reload-and-retry
;; for Safari's dropped clicks (#683), so a dropped click is not read as "focus kept".

(require '[etaoin.api :as e]
         '[taoensso.timbre :as timbre])

;; timbre defaults to debug under babashka, which buries the four result rows in a
;; wall of webdriver traffic.
(timbre/set-min-level! :warn)

(def base (or (first *command-line-args*)
              (throw (ex-info "usage: autofill_scheme.clj <base-url>" {}))))

(def pages ["autofill-off.html"
            "autofill-distant-password.html"
            "autofill-detached-password.html"
            "autofill-new-password.html"])

(def autofill-window-ms 1000)

(defn- active-id [driver]
  (e/js-execute driver "var a = document.activeElement;
                        return a ? (a.id || a.tagName) : 'null'"))

(defn- focus-then-wait [driver page]
  (loop [tries 3]
    (e/go driver (str base "/" page))
    (e/wait-visible driver {:id :af-end})
    (e/click driver :af-pass)
    (if (and (= "BODY" (active-id driver)) (pos? tries))
      (recur (dec tries))
      (do (Thread/sleep (long autofill-window-ms))
          (active-id driver)))))

(println (format "\n=== %s ===" base))
(e/with-safari driver
  (doseq [page pages]
    (let [landed (focus-then-wait driver page)]
      (println (format "%-34s focus -> %-8s %s"
                       page
                       landed
                       (if (= "af-user" landed) "STOLEN" "kept"))))))
