;;; neo-location.el --- Location discovery for NEO -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'solar)
(require 'subr-x)

(cl-defstruct (neo-ipinfo
               (:constructor neo--location-make-ipinfo)
               (:copier nil)
               (:predicate neo/ipinfo-p)
               (:conc-name neo/ipinfo-))
  "Provider-independent IP geolocation information."
  provider
  ip
  hostname
  city
  region
  region-code
  country-name
  country-code
  postal-code
  latitude
  longitude
  timezone
  isp
  organization
  autonomous-system)

(defcustom neo/ipinfo-provider 'ipinfo
  "Provider used to retrieve IP geolocation information.

The default ipinfo.io provider uses HTTPS.  The free ip-api.com
endpoint uses unencrypted HTTP and is therefore opt-in."
  :type '(choice
          (const :tag "ipinfo.io" ipinfo)
          (const :tag "ip-api.com" ip-api))
  :group 'neo)

(defcustom neo/ipinfo-ipinfo-url "https://ipinfo.io"
  "Endpoint used for the ipinfo.io provider."
  :type 'string
  :group 'neo)

(defcustom neo/ipinfo-ip-api-url "http://ip-api.com/json/"
  "Endpoint used for the ip-api.com provider.

The provider's free endpoint uses unencrypted HTTP."
  :type 'string
  :group 'neo)

(defcustom neo/ipinfo-timeout 2
  "Maximum number of seconds to wait for the IP geolocation request."
  :type 'number
  :group 'neo)

(defvar neo/ipinfo nil
  "IP geolocation information for the current NEO session.

The value is a `neo-ipinfo' struct, or nil when location discovery was
unavailable or unsuccessful.")

(defun neo--location-parse-json (json)
  "Parse provider JSON into an alist, returning nil for invalid JSON."
  (condition-case nil
      (let ((value (json-parse-string
                    json
                    :object-type 'alist
                    :array-type 'list
                    :null-object nil
                    :false-object nil)))
        (and (listp value) value))
    (error nil)))

(defun neo--location-string (value)
  "Return trimmed string VALUE, or nil when it is empty or not a string."
  (when (stringp value)
    (let ((trimmed (string-trim value)))
      (unless (string-empty-p trimmed)
        trimmed))))

(defun neo--location-provider-url ()
  "Return the configured endpoint for `neo/ipinfo-provider'."
  (pcase neo/ipinfo-provider
    ('ipinfo neo/ipinfo-ipinfo-url)
    ('ip-api neo/ipinfo-ip-api-url)))

(defun neo--location-fetch-ipinfo ()
  "Retrieve and parse IP geolocation information with curl.
Return nil when curl is unavailable or the request fails."
  (when-let ((curl (executable-find "curl"))
             (url (neo--location-provider-url)))
    (condition-case nil
        (with-temp-buffer
          (when (eq 0
                    (process-file curl nil t nil
                                  "--silent"
                                  "--fail"
                                  "--location"
                                  "--max-time"
                                  (number-to-string neo/ipinfo-timeout)
                                  url))
            (when-let ((payload (neo--location-parse-json (buffer-string))))
              (neo--location-normalize-response
               neo/ipinfo-provider payload))))
      (file-error nil))))

(defun neo--location-coordinate-number (value)
  "Parse coordinate VALUE, returning nil when it is not a decimal number."
  (when (and (stringp value)
             (string-match-p
              "\\`[+-]?[[:digit:]]+\\(?:\\.[[:digit:]]+\\)?\\'"
              value))
    (string-to-number value)))

(defun neo--location-coordinate (value minimum maximum)
  "Return coordinate VALUE when it is between MINIMUM and MAXIMUM."
  (and (numberp value)
       (<= minimum value maximum)
       value))

(defun neo--location-ipinfo-coordinates (payload)
  "Return validated latitude and longitude from ipinfo PAYLOAD."
  (when-let ((location (alist-get 'loc payload)))
    (when (stringp location)
      (pcase (split-string location "," t "[[:space:]]*")
        (`(,latitude-text ,longitude-text)
         (let ((latitude (neo--location-coordinate-number latitude-text))
               (longitude (neo--location-coordinate-number longitude-text)))
           (when (and latitude
                      longitude
                      (<= -90 latitude 90)
                      (<= -180 longitude 180))
             (cons latitude longitude))))))))

(defun neo--location-normalize-ipinfo (payload)
  "Normalize an ipinfo.io PAYLOAD into a `neo-ipinfo' struct."
  (unless (alist-get 'error payload)
    (let ((coordinates (neo--location-ipinfo-coordinates payload))
          (timezone (neo--location-string (alist-get 'timezone payload))))
      (when (and coordinates timezone)
        (neo--location-make-ipinfo
         :provider 'ipinfo
         :ip (neo--location-string (alist-get 'ip payload))
         :hostname (neo--location-string (alist-get 'hostname payload))
         :city (neo--location-string (alist-get 'city payload))
         :region (neo--location-string (alist-get 'region payload))
         :country-code (neo--location-string (alist-get 'country payload))
         :postal-code (neo--location-string (alist-get 'postal payload))
         :latitude (car coordinates)
         :longitude (cdr coordinates)
         :timezone timezone
         :organization (neo--location-string (alist-get 'org payload)))))))

(defun neo--location-normalize-ip-api (payload)
  "Normalize an ip-api.com PAYLOAD into a `neo-ipinfo' struct."
  (when (equal (alist-get 'status payload) "success")
    (let ((latitude
           (neo--location-coordinate (alist-get 'lat payload) -90 90))
          (longitude
           (neo--location-coordinate (alist-get 'lon payload) -180 180))
          (timezone
           (neo--location-string (alist-get 'timezone payload))))
      (when (and latitude longitude timezone)
        (neo--location-make-ipinfo
         :provider 'ip-api
         :ip (neo--location-string (alist-get 'query payload))
         :city (neo--location-string (alist-get 'city payload))
         :region (neo--location-string (alist-get 'regionName payload))
         :region-code (neo--location-string (alist-get 'region payload))
         :country-name (neo--location-string (alist-get 'country payload))
         :country-code (neo--location-string
                        (alist-get 'countryCode payload))
         :postal-code (neo--location-string (alist-get 'zip payload))
         :latitude latitude
         :longitude longitude
         :timezone timezone
         :isp (neo--location-string (alist-get 'isp payload))
         :organization (neo--location-string (alist-get 'org payload))
         :autonomous-system (neo--location-string
                             (alist-get 'as payload)))))))

(defun neo--location-normalize-response (provider payload)
  "Normalize PROVIDER's PAYLOAD into a `neo-ipinfo' struct."
  (pcase provider
    ('ipinfo (neo--location-normalize-ipinfo payload))
    ('ip-api (neo--location-normalize-ip-api payload))))

(defun neo--location-ipinfo-name (ipinfo)
  "Return a human-readable location name from IPINFO."
  (let ((parts
         (seq-filter
          #'(lambda (value)
              (and (stringp value)
                   (not (string-empty-p value))))
          (list (neo/ipinfo-city ipinfo)
                (neo/ipinfo-region ipinfo)
                (or (neo/ipinfo-country-name ipinfo)
                    (neo/ipinfo-country-code ipinfo))))))
    (and parts (string-join parts ", "))))

(defun neo--location-calendar-time-zone-values (time-zone)
  "Return Calendar time-zone values derived from IANA TIME-ZONE.
The result is a plist containing the standard UTC offset in minutes
and the standard and daylight abbreviations."
  (condition-case nil
      (let* ((year (decoded-time-year (decode-time)))
             (january-time (encode-time 0 0 12 15 1 year t))
             (july-time (encode-time 0 0 12 15 7 year t))
             (january-zone (current-time-zone january-time time-zone))
             (july-zone (current-time-zone july-time time-zone))
             (january-dst (decoded-time-dst
                            (decode-time january-time time-zone)))
             (july-dst (decoded-time-dst
                         (decode-time july-time time-zone)))
             (standard-zone (if january-dst july-zone january-zone))
             (daylight-zone (cond
                              (january-dst january-zone)
                              (july-dst july-zone)
                              (t standard-zone))))
        (when (and (numberp (car standard-zone))
                   (stringp (cadr standard-zone))
                   (stringp (cadr daylight-zone)))
          (list :offset (/ (car standard-zone) 60)
                :standard-name (cadr standard-zone)
                :daylight-name (cadr daylight-zone))))
    (error nil)))

(defun neo--location-apply-ipinfo (ipinfo)
  "Configure Emacs location-aware variables from IPINFO."
  (when (and (numberp (neo/ipinfo-latitude ipinfo))
             (numberp (neo/ipinfo-longitude ipinfo)))
    (setq calendar-latitude (neo/ipinfo-latitude ipinfo)
          calendar-longitude (neo/ipinfo-longitude ipinfo)))
  (when-let ((location-name (neo--location-ipinfo-name ipinfo)))
    (setq calendar-location-name location-name))
  (when-let* ((time-zone (neo/ipinfo-timezone ipinfo))
              (values (and (stringp time-zone)
                           (neo--location-calendar-time-zone-values time-zone))))
    (setq calendar-time-zone (plist-get values :offset)
          calendar-standard-time-zone-name
          (plist-get values :standard-name)
          calendar-daylight-time-zone-name
          (plist-get values :daylight-name))))

(defun neo/initialize-location ()
  "Discover this session's location and configure Emacs for it."
  (setq neo/ipinfo (neo--location-fetch-ipinfo))
  (when neo/ipinfo
    (neo--location-apply-ipinfo neo/ipinfo))
  neo/ipinfo)

(provide 'neo-location)
;;; neo-location.el ends here
