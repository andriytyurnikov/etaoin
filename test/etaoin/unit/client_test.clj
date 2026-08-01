(ns etaoin.unit.client-test
  "Unit tests for `etaoin.impl.client`.

  Hermetic: `client/http-request` is the designated mocking seam, so no WebDriver
  and no browser are required."
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [etaoin.impl.client :as client]
   [etaoin.test-report]))

;; `redact-payload` is what both leak sites go through: the debug log line and the
;; ex-data of the thrown map. `for-report` is the opt-out wrapper around it.
(def ^:private redact #'client/redact-payload)
(def ^:private for-report #'client/for-report)

(defn- ex-data-from-call
  "Force `client/call` to throw, and return the ex-data of what it threw."
  [driver method path payload]
  (with-redefs [client/http-request (fn [_] (throw (ex-info "connection refused" {})))]
    (try
      (client/call driver method path payload)
      (catch Throwable ex (ex-data ex)))))

;; ---------------------------------------------------------------------------
;; redaction rules

(deftest typed-text-is-redacted
  (testing "what `fill` sends is where passwords live"
    (is (= {:text "<redacted 19 chars>"}
           (redact {:text "hunter2-my-password"}))))
  (testing "length is revealed, content is not"
    (let [out (:text (redact {:text "s3cret"}))]
      (is (not (str/includes? out "s3cret")))
      (is (= "<redacted 6 chars>" out)))))

(deftest cookie-values-are-redacted-but-locator-values-are-not
  (testing "a cookie value is a credential"
    (is (= {:cookie {:name "session" :value "<redacted 17 chars>" :domain "example.org"}}
           (redact {:cookie {:name   "session"
                             :value  "SESSION-JWT-VALUE"
                             :domain "example.org"}}))))
  (testing "`:value` at the top level is a locator term, not a secret"
    ;; find-element sends {:using "xpath" :value "//div[@id='x']"} -- redacting that
    ;; would gut the log line and protect nothing.
    (is (= {:using "xpath" :value "//div[@id='x']"}
           (redact {:using "xpath" :value "//div[@id='x']"})))))

(deftest javascript-arguments-are-redacted
  (let [out (redact {:script "return login(arguments[0])" :args ["api-token-abc123"]})]
    (testing "args are redacted"
      (is (= ["<redacted 16 chars>"] (:args out))))
    (testing "the script survives -- it is code, and it is what you need in order to debug"
      (is (= "return login(arguments[0])" (:script out))))))

(deftest secret-bearing-capability-keys-are-redacted
  (let [caps (-> (redact {:capabilities
                          {:firstMatch [{:browserName        "chrome"
                                         "browserless:token" "TOKEN-abc123"
                                         :goog:chromeOptions {:args ["--no-sandbox"]}}]}})
                 :capabilities :firstMatch first)]
    (is (= "<redacted 12 chars>" (get caps "browserless:token")))
    (testing "non-secret capabilities are left alone"
      (is (= "chrome" (:browserName caps)))
      (is (= {:args ["--no-sandbox"]} (:goog:chromeOptions caps))))))

(deftest embedded-url-credentials-are-stripped
  (testing "credentials in a url string"
    (is (= {:url "https://example.org/wd/hub"}
           (redact {:url "https://user:pass@example.org/wd/hub"}))))
  (testing "scheme-less proxy credentials"
    (let [out (get-in (redact {:capabilities {:proxy {:httpProxy "puser:ppass@corp.proxy:8080"}}})
                      [:capabilities :proxy :httpProxy])]
      (is (not (str/includes? out "ppass")))
      (is (str/includes? out "corp.proxy:8080"))))
  (testing "an ordinary host:port is not mistaken for credentials"
    (is (= {:capabilities {:proxy {:httpProxy "corp.proxy:8080"}}}
           (redact {:capabilities {:proxy {:httpProxy "corp.proxy:8080"}}})))))

(deftest benign-payloads-pass-through-untouched
  (is (= {:url "https://clojure.org"} (redact {:url "https://clojure.org"})))
  (is (= {:width 800 :height 600 :x 0 :y 0} (redact {:width 800 :height 600 :x 0 :y 0})))
  (is (= {:handle "CDwindow-1"} (redact {:handle "CDwindow-1"})))
  (is (nil? (redact nil))))

;; ---------------------------------------------------------------------------
;; the leak sites

(deftest thrown-ex-data-does-not-spill-secrets
  (let [exd  (ex-data-from-call
              {:type          :chrome
               :webdriver-url "https://user:sekret@ondemand.example.com/wd/hub"
               :capabilities  {"browserless:token" "TOKEN-abc123"}}
              :post [:session] {:text "hunter2-my-password"})
        dump (pr-str exd)]
    (is (= :etaoin/http-ex (:type exd)))
    (testing "payload"
      (is (= {:text "<redacted 19 chars>"} (:payload exd))))
    (testing "webdriver-url credentials"
      (is (= "https://ondemand.example.com/wd/hub" (:webdriver-url exd))))
    (testing "capabilities carried on the driver map"
      (is (= "<redacted 12 chars>" (get-in exd [:driver :capabilities "browserless:token"]))))
    (testing "nothing secret anywhere in the whole ex-data"
      (doseq [secret ["hunter2-my-password" "sekret" "TOKEN-abc123"]]
        (is (not (str/includes? dump secret))
            (str "ex-data leaked " (pr-str secret)))))
    (testing "the parts you need in order to debug are still there"
      (is (= :post (:method exd)))
      (is (= "session" (:path exd))))))

(deftest the-log-line-and-the-ex-data-share-one-redaction-path
  ;; The debug log line at the top of `call` formats `(-> payload for-report)`, the
  ;; same call the thrown map uses, so covering `for-report` covers both sites.
  (is (= {:text "<redacted 19 chars>"} (for-report {:text "hunter2-my-password"}))))

(deftest opting-out-restores-the-raw-payload
  (testing "ETAOIN_LOG_SECRETS is what turns redaction off"
    (with-redefs [client/log-secrets true]
      (is (= {:text "hunter2-my-password"} (for-report {:text "hunter2-my-password"}))))))
