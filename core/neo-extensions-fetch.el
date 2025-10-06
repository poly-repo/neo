(require 'neo-struct)

(cl-defmethod neo-fetch-extensions ((neo neo))
  "Fetch all available extensions for NEO.
This method populates the `available-extensions' slot of the NEO struct."
  ;; The user will populate this later.
  )

(cl-defmethod initialize-instance :after ((neo neo) &rest _initargs)
  "Fetch extensions after a `neo' instance is created."
  (neo-fetch-extensions neo))

(provide 'neo-extensions-fetch)
