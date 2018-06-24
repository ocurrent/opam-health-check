module Make (Backend : Backend_intf.BACKEND) :
  Backend_intf.CACHE with type backend = Backend.t
