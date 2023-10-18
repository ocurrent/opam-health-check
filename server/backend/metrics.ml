open Prometheus

let namespace = "opam"
let subsystem = "health_check"

let number_of_packages =
  let help = "Total number packages in the run" in
  Gauge.v ~help ~namespace ~subsystem "number_of_packages"

let running =
  let help = "Run is in progress" in
  Gauge.v ~help ~namespace ~subsystem "running"
