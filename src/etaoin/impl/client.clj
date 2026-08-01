(ns ^:no-doc etaoin.impl.client
  (:require
   [babashka.http-client :as client]
   [cheshire.core :as json]
   [cheshire.factory :as cheshire-factory]
   [clj-commons.slingshot :refer [throw+]]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [etaoin.impl.proc :as proc]
   [etaoin.impl.util :as util]))

(set! *warn-on-reflection* true)

;;
;; defaults
;;

(def default-timeout
  "HTTP timeout in seconds. The current value may seem high,
  but according to my experience with SPA application full of React
  modules even 20 seconds can insufficient time for a driver to process
  your request."
  60)

(defn read-timeout []
  (if-let [t (System/getenv "ETAOIN_TIMEOUT")]
    (Integer/parseInt t)
    default-timeout))

(def timeout (read-timeout))

(def client (delay (client/client
                     {:request {:headers {:accept "application/json"
                                          :content-type "application/json"
                                          :accept-encoding ["gzip" "deflate"]}
                                :timeout (* 1000 timeout)}  ;; request timeout
                      :connect-timeout (* 1000 timeout)})))
;;
;; helpers
;;
(def ^:private default-jackson-factory
  "The default jackson input string length is 20mb which is low for Etaoin.
  For example a webdriver print page returns the page pdf as a base64 encoded string.
  The current underlying jackson 2.8.3 option type is an `int` so we are effectively using the max
  possible for this option (just under 2GiB)."
  (cheshire-factory/make-json-factory {:max-input-string-length Integer/MAX_VALUE}))

;;
;; secret redaction
;;
;; WebDriver request payloads routinely carry secrets: `fill` sends what the user
;; typed, `set-cookie` sends session cookies, `js-execute` sends whatever args you
;; hand it, and creating a session sends the capabilities map, which the user guide
;; tells you to put provider tokens in. None of that belongs in a log line or in the
;; ex-data of an exception, both of which tend to end up in CI output.

(def ^:private secret-key-re
  #"(?i)token|password|passwd|secret|api[-_]?key|access[-_]?key|credential|authorization")

(def ^:private url-creds-re
  #"[a-zA-Z][a-zA-Z0-9+.-]*://[^/@\s]+:[^/@\s]+@")

;; W3C proxy settings carry credentials without a scheme, e.g. "user:pass@proxy:8080"
(def ^:private bare-creds-re
  #"^([^/@\s:]+):([^/@\s:]+)@([^/@\s]+)$")

(defn log-secrets?
  "Should payloads be logged and reported as-is, secrets and all?

  Set `ETAOIN_LOG_SECRETS` to `1`, `true` or `yes` when debugging on a machine
  where spilling your own secrets is not a concern."
  []
  (contains? #{"1" "true" "yes"}
             (some-> (System/getenv "ETAOIN_LOG_SECRETS") str/lower-case)))

(def log-secrets (log-secrets?))

(defn- redacted
  "A stand-in for `v` that reveals nothing about it but its size."
  [v]
  (if (string? v)
    (format "<redacted %d chars>" (count v))
    "<redacted>"))

(defn- secret-key? [k]
  (boolean (and (or (keyword? k) (string? k) (symbol? k))
                (re-find secret-key-re (name k)))))

(defn- scrub
  "Deeply redact values held under an obviously secret-bearing key, and strip
  credentials from any url-ish strings."
  [v]
  (cond
    ;; records are `map?` but cannot be `empty`d, and none of ours carry secrets
    (record? v) v
    (map? v)    (reduce-kv (fn [m k x]
                             (assoc m k (if (secret-key? k) (redacted x) (scrub x))))
                           (empty v)
                           v)
    (vector? v) (mapv scrub v)
    (set? v)    (into #{} (map scrub) v)
    (seq? v)    (doall (map scrub v))
    (string? v) (cond
                  (re-find url-creds-re v)     (util/strip-url-creds v)
                  (re-matches bare-creds-re v) (str/replace v bare-creds-re "<redacted>@$3")
                  :else                        v)
    :else       v))

(defn- redact-payload
  "Redact secrets from a WebDriver request `payload`.

  Beyond the generic [[scrub]], three payload keys are secret-bearing without
  being named like it. Note that `:value` is only a secret under `:cookie` --
  at the top level it is a locator term."
  [payload]
  (if-not (map? payload)
    (scrub payload)
    (cond-> (scrub payload)
      ;; what the user typed, e.g. into a password field
      (contains? payload :text)
      (assoc :text (redacted (:text payload)))

      ;; session cookies are credentials
      (contains? (:cookie payload) :value)
      (assoc-in [:cookie :value] (redacted (get-in payload [:cookie :value])))

      ;; arguments handed to injected javascript
      (contains? payload :script)
      (assoc :args (mapv redacted (:args payload))))))

(defn- for-report
  "Return `payload` as it should appear in a log line or in ex-data."
  [payload]
  (if log-secrets payload (redact-payload payload)))

(defn- driver-for-report
  "Return `driver` as it should appear in ex-data.

  Only `:capabilities` and `:webdriver-url` can hold credentials. The rest of the
  map is left exactly as-is -- notably `:process`, which is a `babashka.process`
  record holding live streams, and which the reader of an error report needs
  unaltered."
  [driver]
  (if log-secrets
    driver
    (cond-> driver
      (:capabilities driver)  (update :capabilities scrub)
      (:webdriver-url driver) (update :webdriver-url util/strip-url-creds))))

(defn- url-item-str [item]
  (cond
    (keyword? item) (name item)
    (symbol? item)  (name item)
    (string? item)  item
    :else           (str item)))

(defn- get-url-path [items]
  (str/join "/" (map url-item-str items)))

(defn- parse-json [body]
  (let [body* (str/replace body #"Invalid Command Method -" "")]
    ;; override jackson options, prefer user specified binding
    ;; (not officially supported but convenient for testing)
    (binding [cheshire-factory/*json-factory* (or cheshire-factory/*json-factory*
                                                  default-jackson-factory)]
      (json/parse-string body* true))))

(defn- error-response [body]
  (if (string? body)
    (parse-json body)
    body))

(defn- realized-driver
  "Realize process liveness (or actually deadness, if dead)"
  [{:keys [process] :as driver}]
  (try
    (if (and process (not (proc/alive? process)))
      (assoc driver :process (proc/result process))
      driver)
    (catch Throwable ex
      ;; if, by chance, something goes wrong while trying to realize process liveness
      (assoc driver :process-liveness-ex ex))))

(defn http-request
  "an isolated http-request to support mocking"
  [params]
  (client/request params))

;;
;; client
;;

(defn call
  [{driver-type :type :keys [host port webdriver-url] :as driver}
   method path-args payload]
  (let [path   (get-url-path path-args)
        url    (if webdriver-url
                 (format "%s/%s" webdriver-url path)
                 (format "http://%s:%s/%s" host port path))
        params (cond-> {:uri     url
                        :method  method
                        :client  @client
                        :throw   false}
                 (= :post method)
                 (assoc :body (.getBytes (json/generate-string (or payload {}))
                                         "UTF-8")))
        _ (log/debugf "%s %s %6s %s %s"
                      (name driver-type)
                      (if webdriver-url
                        (util/strip-url-creds webdriver-url)
                        (str host ":" port))
                      (-> method name str/upper-case)
                      path
                      (-> payload for-report (or "")))
        error (delay {:type     :etaoin/http-ex
                      :driver   (-> driver realized-driver driver-for-report)
                      :webdriver-url (some-> webdriver-url util/strip-url-creds)
                      :host     host
                      :port     port
                      :method   method
                      :path     path
                      :payload  (for-report payload)})
        resp  (try (http-request params)
                   (catch Throwable ex
                     {:exception ex}))]
    (if (:exception resp)
      (throw+ @error (:exception resp))
      (let [body  (some-> resp :body parse-json)
            error (delay (assoc @error
                                :type :etaoin/http-error
                                :status (:status resp)
                                :response (error-response body)))]
        (cond
          (-> resp :status (not= 200))
          (throw+ @error)

          (-> body :status (or 0) (> 0))
          (throw+ @error)

          :else
          body)))))

(comment
  (http-request {:method :head
                 :uri "https://clojure.org"
                 :client  @client})


  )
