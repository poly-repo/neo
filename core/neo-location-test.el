;;; neo-location-test.el --- Tests for NEO location discovery -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'neo-custom)
(require 'neo-location)

(defconst neo-location-test--response
  "{\"city\":\"Pittsburgh\",\"region\":\"Pennsylvania\",\"country\":\"US\",\
\"loc\":\"40.4406,-79.9959\",\"timezone\":\"America/New_York\",\
\"ip\":\"173.75.42.197\",\"hostname\":\"example.test\",\
\"postal\":\"15212\",\"org\":\"AS701 Verizon Business\"}")

(defconst neo-location-test--ip-api-response
  "{\"status\":\"success\",\"country\":\"United States\",\
\"countryCode\":\"US\",\"region\":\"PA\",\"regionName\":\"Pennsylvania\",\
\"city\":\"Pittsburgh\",\"zip\":\"15206\",\"lat\":40.4725,\"lon\":-79.9109,\
\"timezone\":\"America/New_York\",\"isp\":\"Verizon Business\",\
\"org\":\"Verizon Business\",\"as\":\"AS701 Verizon Business\",\
\"query\":\"173.75.42.197\"}")

(ert-deftest neo/normalize-ipinfo-response ()
  "Normalize an ipinfo.io response without an external JSON processor."
  (let* ((payload (neo--location-parse-json neo-location-test--response))
         (ipinfo (neo--location-normalize-response 'ipinfo payload)))
    (should (neo/ipinfo-p ipinfo))
    (should (eq (neo/ipinfo-provider ipinfo) 'ipinfo))
    (should (equal (neo/ipinfo-city ipinfo) "Pittsburgh"))
    (should (= (neo/ipinfo-latitude ipinfo) 40.4406))
    (should (= (neo/ipinfo-longitude ipinfo) -79.9959))
    (should (equal (neo/ipinfo-country-code ipinfo) "US"))
    (should (equal (neo/ipinfo-organization ipinfo)
                   "AS701 Verizon Business"))))

(ert-deftest neo/normalize-ip-api-response ()
  "Normalize a successful ip-api.com response."
  (let* ((payload
          (neo--location-parse-json neo-location-test--ip-api-response))
         (ipinfo (neo--location-normalize-response 'ip-api payload)))
    (should (neo/ipinfo-p ipinfo))
    (should (eq (neo/ipinfo-provider ipinfo) 'ip-api))
    (should (equal (neo/ipinfo-ip ipinfo) "173.75.42.197"))
    (should (equal (neo/ipinfo-country-name ipinfo) "United States"))
    (should (equal (neo/ipinfo-region-code ipinfo) "PA"))
    (should (= (neo/ipinfo-latitude ipinfo) 40.4725))
    (should (= (neo/ipinfo-longitude ipinfo) -79.9109))
    (should (equal (neo/ipinfo-autonomous-system ipinfo)
                   "AS701 Verizon Business"))))

(ert-deftest neo/parse-ipinfo-rejects-invalid-json ()
  "Return nil for a malformed ipinfo response."
  (should-not (neo--location-parse-json "{not-json")))

(ert-deftest neo/normalize-ip-api-rejects-failure-response ()
  "Return nil when ip-api.com reports failure."
  (should-not
   (neo--location-normalize-response
    'ip-api
    '((status . "fail") (message . "reserved range")))))

(ert-deftest neo/normalize-provider-rejects-invalid-coordinates ()
  "Return nil when a provider supplies incomplete or invalid coordinates."
  (should-not
   (neo--location-normalize-response
    'ipinfo
    '((loc . "not-a-coordinate")
      (timezone . "America/New_York"))))
  (should-not
   (neo--location-normalize-response
    'ip-api
    '((status . "success")
      (lat . 100)
      (lon . -79.9)
      (timezone . "America/New_York")))))

(ert-deftest neo/fetch-ipinfo-uses-bounded-curl-request ()
  "Fetch ipinfo with curl and parse its output."
  (let (arguments)
    (cl-letf (((symbol-function #'executable-find)
               #'(lambda (_program) "/usr/bin/curl"))
              ((symbol-function #'process-file)
               #'(lambda (_program _infile destination _display &rest args)
                   (setq arguments args)
                   (if (eq destination t)
                       (insert neo-location-test--ip-api-response)
                     (with-current-buffer destination
                       (insert neo-location-test--ip-api-response)))
                   0)))
      (let* ((neo/ipinfo-provider 'ip-api)
             (neo/ipinfo-timeout 3)
             (ipinfo (neo--location-fetch-ipinfo)))
        (should (eq (neo/ipinfo-provider ipinfo) 'ip-api))
        (should (equal (neo/ipinfo-city ipinfo) "Pittsburgh"))
        (should (member "--max-time" arguments))
        (should (member "3" arguments))
        (should (member "http://ip-api.com/json/" arguments))))))

(ert-deftest neo/fetch-ipinfo-fails-quietly ()
  "Return nil when curl is unavailable or exits unsuccessfully."
  (cl-letf (((symbol-function #'executable-find)
             #'(lambda (_program) nil)))
    (should-not (neo--location-fetch-ipinfo)))
  (cl-letf (((symbol-function #'executable-find)
             #'(lambda (_program) "/usr/bin/curl"))
            ((symbol-function #'process-file)
             #'(lambda (&rest _args) 22)))
    (should-not (neo--location-fetch-ipinfo)))
  (cl-letf (((symbol-function #'executable-find)
             #'(lambda (_program) "/usr/bin/curl"))
            ((symbol-function #'process-file)
             #'(lambda (&rest _args) "killed")))
    (should-not (neo--location-fetch-ipinfo))))

(ert-deftest neo/apply-ipinfo-configures-calendar-location ()
  "Configure Calendar coordinates, name, and time zone from ipinfo."
  (let (calendar-latitude
        calendar-longitude
        calendar-location-name
        calendar-time-zone
        calendar-standard-time-zone-name
        calendar-daylight-time-zone-name)
    (cl-letf (((symbol-function #'neo--location-calendar-time-zone-values)
               #'(lambda (_time-zone)
                   '(:offset -300
                     :standard-name "EST"
                     :daylight-name "EDT"))))
      (neo--location-apply-ipinfo
       (neo--location-normalize-response
        'ipinfo
        (neo--location-parse-json neo-location-test--response))))
    (should (= calendar-latitude 40.4406))
    (should (= calendar-longitude -79.9959))
    (should (equal calendar-location-name "Pittsburgh, Pennsylvania, US"))
    (should (= calendar-time-zone -300))
    (should (equal calendar-standard-time-zone-name "EST"))
    (should (equal calendar-daylight-time-zone-name "EDT"))))

(ert-deftest neo/initialize-location-publishes-ipinfo ()
  "Publish fetched data and apply it during initialization."
  (let ((neo/ipinfo nil)
        applied)
    (cl-letf (((symbol-function #'neo--location-fetch-ipinfo)
               #'(lambda ()
                   (neo--location-make-ipinfo :city "Pittsburgh")))
              ((symbol-function #'neo--location-apply-ipinfo)
               #'(lambda (ipinfo)
                   (setq applied ipinfo))))
      (should (neo/ipinfo-p (neo/initialize-location)))
      (should (eq neo/ipinfo applied)))))

(provide 'neo-location-test)
;;; neo-location-test.el ends here
