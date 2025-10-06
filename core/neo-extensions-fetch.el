(require 'neo)

(cl-defmethod neo-fetch-extensions ((neo neo))
  "Fetch all available extensions for NEO.
This method populates the `available-extensions' slot of the NEO struct."
  ;; The user will populate this later.
  )

(provide 'neo-extensions-fetch)
