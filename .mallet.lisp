(:mallet-config
  (:extends :default)
  (:enable :line-length :max 100)
  (:enable :no-package-use :allow ("CLEAN" "PATHWAY/PATHNAME-UTILITIES"))
  (:disable :bare-float-literal))
